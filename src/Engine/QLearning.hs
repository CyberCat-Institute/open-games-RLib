{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Engine.QLearning where

import           Control.DeepSeq
import           Control.Monad.Reader
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson
import qualified Data.Array.IO as A
import           Data.Csv
import           Data.Hashable
import           Data.Ix
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import           Engine.Memory (Memory)
import qualified Engine.Memory as Memory
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           GHC.Generics
import           Optics.Operators
import           Optics.Optic ((%))
import           Optics.TH (makeLenses)
import qualified RIO
import           RIO (HasGLogFunc(..))
import           System.Random
import qualified System.Random as Rand
import           System.Random.MWC.CondensedTable
import           System.Random.Stateful

--------------------------------------------------------------------------------
-- Logging

data QLearningMsg n o a
  = RewardMsg !(Reward n o a)
  | QTableDirtied !(Dirtied n o a)

data Dirtied n o a = Dirtied
  { dirtiedIteration :: !Int
  , dirtiedPlayer :: !Int
  , dirtiedStateActionIndex :: !Int
  , dirtiedQValue :: !Double
  }

data Reward n o a = Reward
  { rewardPlayer :: !Int
  , rewardIteration :: !Int
  , rewardStateAction :: (Memory.Vector n (o (Idx a)), a)
  , rewardStateActionIndex :: !Int
  , rewardReward :: !Double
  }

--------------------------------------------------------------------------------
-- A simple condensed table type

data CTable a = CTable
  { ctable :: !(CondensedTableV a)
  , population :: !(V.Vector a)
  }

-- | Create a uniform distribution condensed table.
uniformCTable :: V.Vector a -> CTable a
uniformCTable population =
  CTable
    { population
    , ctable = tableFromProbabilities (fmap (, probability) population)
    }
  where
    probability = 1 / fromIntegral (V.length population)

-- | Read the table at an index.
readTable :: CTable a -> Idx a -> a
readTable (CTable {population}) (Idx i) =
  population V.! i

--------------------------------------------------------------------------------
-- Random sampling

{-# INLINE samplePopulation #-}
samplePopulation :: CTable a -> StdGen -> (a, StdGen)
samplePopulation population gen = runStateGen gen $ genFromTable (ctable population)

{-# INLINE samplePopulation_ #-}
samplePopulation_ :: CTable a -> StdGen -> a
samplePopulation_ population gen = fst $ samplePopulation population gen

--------------------------------------------------------------------------------
-- Indexable values

newtype Idx a = Idx Int deriving (A.Ix, Eq, Ord, Show, NFData, ToJSON, Hashable, ToField)
class ToIdx a where
  toIdx :: a -> Idx a

--------------------------------------------------------------------------------
-- Comments

-- TODO check the random update
-- TODO take care of the range of values being used by array on one side and by the random module on the other side
-- TODO Replace the array access functionality with a lens
-- NOTE The _x_ input needs to be manipulated in the outside game definition

------------------
-- 0 Preliminaries

type Agent = String

type Temperature = Double

type ExploreRate = Double

type LearningRate = Double

type DiscountFactor = Double

type QTable n o a = A.IOUArray (Memory.Vector n (o (Idx a)), Idx a) Double

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized

data State n o a = State
  { _env :: Env n o a
  , _obs :: o a
  }

data Env n o a = Env
  { _name   :: String
  , _player :: Int
  , _qTable :: QTable n o a
  , _iteration  :: Int
  , _exploreRate :: ExploreRate
  , _randomGen :: Rand.StdGen
  , _obsAgent :: Memory.Vector n (o (Idx a))
  , _temperature :: Temperature
  }  deriving (Generic)
deriving instance (Show a, Show (o a), Show (o (Idx a)), Memory n) => Show (Env n o a)
-- ^ Added here the agent observation the idea is that global and local information might diverge
instance (NFData a, NFData (o (Idx a)), (NFData (Memory.Vector n (o (Idx a))))) => NFData (Env n o a)
instance Show (QTable n o a) where show _ = "QTable"
instance NFData (QTable n o a) where rnf _ = ()

type QLearningStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r

makeLenses ''Env
makeLenses ''State


------------------------
-- 1 Auxiliary functions
-- and updating functions
-- given a q-table, derive maximal score and the action that guarantees it
{-# INLINE maxScore #-}
maxScore ::
     (ToIdx a, Ord a, Functor o, Ix (o (Idx a)), Ix (Memory.Vector n (o (Idx a))))
  => Memory.Vector n (o (Idx a))
  -> QTable n o a
  -> CTable a
  -> IO (Double, a)
maxScore obs table0 support = do
  valuesAndActions <-
    V.mapM
      (\action -> do
         let index = (obs, toIdx action)
         value <- A.readArray table0 index
         pure (value, action))
      (population support)
  let !maximum' = V.maximum valuesAndActions
  pure maximum'

-- better prepare output to be used
extractFst :: Maybe (a,b) -> Maybe a
extractFst Nothing      = Nothing
extractFst (Just (a,b)) = Just a

extractSnd :: Maybe (a,b) -> Maybe b
extractSnd Nothing      = Nothing
extractSnd (Just (a,b)) = Just b

-- Update randomG
updateRandomG :: State n o a -> Rand.StdGen -> State n o a
updateRandomG s r = env % randomGen .~ r  $  s

-- Update QTable
updateQTable :: State n o a -> QTable n o a  -> State n o a
updateQTable s q = env % qTable .~ q  $  s

-- Update Observation for each agent
updateObservationAgent ::  Memory.Vector n (o (Idx a)) -> State n o a -> State n o a
updateObservationAgent obs s = env % obsAgent .~ obs $  s

-- Update iterator
updateIteration :: State n o a -> State n o a
updateIteration = env % iteration %~ (+ 1)

-- Update temperature
updateTemperature :: Temperature -> State n o a ->  State n o a
updateTemperature decreaseFactor = env % temperature %~ (* decreaseFactor)

-- Update explorRate
updateExploreRate :: ExploreRate -> State n o a -> State n o a
updateExploreRate decreaseFactor = env % exploreRate %~ (* decreaseFactor)

-- Update gen, qtable
updateRandomGAndQTable :: State n o a -> Rand.StdGen -> State n o a
updateRandomGAndQTable s r = updateRandomG s r

-- Update gen, qtable, temp
updateRandomGQTableTemp :: Temperature -> State n o a -> Rand.StdGen -> State n o a
updateRandomGQTableTemp decreaseFactor s r = (updateTemperature decreaseFactor) $ updateRandomGAndQTable s r

-- Update gen, qtable,exploreRate
updateRandomGQTableExplore :: ExploreRate -> State n o a -> Rand.StdGen -> State n o a
updateRandomGQTableExplore decreaseFactor s r = (updateExploreRate decreaseFactor) $ updateRandomGAndQTable s r

-- Update gen, qtable,exploreRate,agentObs
updateRandomGQTableExploreObs :: ExploreRate -> Memory.Vector n (o (Idx a)) -> State n o a -> Rand.StdGen -> State n o a
updateRandomGQTableExploreObs decreaseFactor obs s r  = (updateObservationAgent obs) $ updateRandomGQTableExplore decreaseFactor s r

-- Update gen, qtable,exploreRate,agentObs, iteration
updateRandomGQTableExploreObsIteration :: ExploreRate -> Memory.Vector n (o (Idx a)) -> State n o a -> Rand.StdGen -> State n o a
updateRandomGQTableExploreObsIteration decreaseFactor obs s r  = updateIteration $ updateRandomGQTableExploreObs decreaseFactor obs s r

-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- | Choose optimally given qmatrix; do not explore. This is for the play part
{-# INLINE chooseActionNoExplore  #-}
chooseActionNoExplore :: (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a, Ord a, ToIdx a, Functor o, Ix (o (Idx a)), Memory n, Ix (Memory.Vector n (o (Idx a)))) =>
  CTable a -> State n o a -> ST.StateT (State n o a) m a
chooseActionNoExplore support s = do
  maxed <- liftIO $ maxScore obsVec (_qTable $ _env s) support
  let (exploreR, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
      optimalAction = snd $  maxed
  return optimalAction
  where  obsVec = _obsAgent (_env s)

-- | Choose the optimal action given the current state or explore greedily

{-# INLINE chooseExploreAction #-}
chooseExploreAction :: (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a, Ord a, ToIdx a, Functor o, Ix (o (Idx a)), Memory n, Ix (Memory.Vector n (o (Idx a)))) =>
  CTable a -> State n o a -> ST.StateT (State n o a) m a
chooseExploreAction support s = do
  -- NOTE: gen'' is not updated anywhere...!!!
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let !action' = samplePopulation_ support gen'
      return action'
    else do
      maxed <- liftIO $ maxScore obsVec (_qTable $ _env s) support
      let optimalAction = snd $  maxed
      return optimalAction
  where  obsVec = _obsAgent (_env s)
  
-- | Explore until temperature is below exgogenous threshold; with each round the threshold gets reduced
{-# INLINE chooseExploreActionDecrTemp  #-}
chooseExploreActionDecrTemp :: (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a, Ord a, ToIdx a, Functor o, Ix (o (Idx a)), Memory n, Ix (Memory.Vector n (o (Idx a)))) =>
  Temperature -> CTable a -> State n o a -> ST.StateT (State n o a) m a
chooseExploreActionDecrTemp tempThreshold support  s = do
    let temp           = _temperature $ _env s
        (_, gen')      = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
        q              = _qTable $ _env s
        chooseExplore  =
          do
            let !action' = samplePopulation_ support gen'
            return action'
        chooseNoExplore =
            do
              maxed' <- liftIO $ maxScore obsVec
                                          (_qTable $ _env s)
                                          support
              let optimalAction = snd $  maxed'
              return optimalAction
        in if temp < tempThreshold
           then chooseNoExplore
           else chooseExplore
    where  obsVec = _obsAgent (_env s)


-- 2.2. Different updates of the qmatrix depending on learning form
-- | Given an action, state, obs and a reward, update the qmatrix
-- | TODO Constant exploration rate

{-# INLINE updateQTableST #-}
updateQTableST ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a, Ord a, ToIdx a, Functor o, Ix (o (Idx a)), Memory n, Ix (Memory.Vector n (o (Idx a)))) =>
                     LearningRate ->  DiscountFactor ->   CTable a -> State n o a -> o a -> a -> Double ->  ST.StateT (State n o a) m a
updateQTableST learningRate gamma support s obs2 action reward  = do
        let table0             = _qTable $ _env s
        prediction    <- liftIO $ A.readArray table0 (obsVec, toIdx action)
        maxed <- liftIO $ maxScore (Memory.pushEnd obsVec (fmap toIdx obs2)) table0 support
        let (_exp, gen')  = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            updatedValue  = reward + gamma * (fst $ maxed)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
        recordingWriteArray (_iteration (_env s)) (_player (_env s)) table0 (Memory.pushEnd obsVec (fmap toIdx (_obs s)), toIdx action) newValue
        ST.put $ updateRandomGAndQTable s gen'
        return action
  where  obsVec = _obsAgent (_env s)


-- | Update the qmatrix with evolving exploration rate

{-# INLINE chooseLearnDecrExploreQTable #-}
chooseLearnDecrExploreQTable ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a, Ord a, ToIdx a, Functor o, Ix (o (Idx a)), Memory n, Ix (Memory.Vector n (o (Idx a)))) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> CTable a -> State n o a -> o a -> a -> Double ->  ST.StateT (State n o a) m a
chooseLearnDecrExploreQTable learningRate gamma decreaseFactorExplore support s obs2 action reward  = do
       let table0             = _qTable $ _env s
       prediction    <- liftIO $ A.readArray table0 (obsVec, toIdx action)
       maxed <- liftIO $ maxScore (Memory.pushEnd obsVec (fmap toIdx obs2)) table0 support
       let  (_,gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            updatedValue = reward + gamma * (fst $ maxed)
            newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
       recordingWriteArray (_iteration (_env s)) (_player (_env s)) table0 (Memory.pushEnd obsVec (fmap toIdx (_obs s)), toIdx action) newValue
       ST.put $  updateRandomGQTableExploreObsIteration decreaseFactorExplore (Memory.pushEnd obsVec (fmap toIdx obs2)) s gen'
       return action
  where obsVec = _obsAgent (_env s)

{-# INLINE recordingWriteArray #-}
recordingWriteArray :: (MonadIO m, A.MArray a1 Double IO, Ix i, HasGLogFunc env, MonadReader env m, GMsg env ~ QLearningMsg n o a2) => Int -> Int -> a1 i Double -> i -> Double -> m ()
recordingWriteArray dirtiedIteration dirtiedPlayer table0 index' value = do
  value0 <- liftIO $ A.readArray table0 index'
  if value0 /= value
    then do
      liftIO $ A.writeArray table0 index' value
      bounds <- liftIO (A.getBounds table0)
      RIO.glog
        (QTableDirtied
           Dirtied
             { dirtiedIteration
             , dirtiedPlayer
             , dirtiedStateActionIndex = Ix.index bounds index'
             , dirtiedQValue = value
             })
    else pure ()

-----------------
-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?

{-# INLINE pureDecisionQStage #-}
pureDecisionQStage ::
     ( MonadIO m
     , MonadReader r m
     , HasGLogFunc r
     , GMsg r ~ QLearningMsg n o a
     , Ix (Memory.Vector n (o (Idx a)))
     , ToIdx a
     )
  => CTable a
  -> Agent
  -> (CTable a -> State n o a -> ST.StateT (State n o a) m a)
  -> (CTable a -> State n o a -> o a -> a -> Double -> ST.StateT (State n o a) m a)
  -> QLearningStageGame m '[ m (a, Env n o a)] '[ m (a, Env n o a)] (o a) () a ( Double
                                                                               , (o a))
pureDecisionQStage actionSpace name chooseAction updateQTable = OpenGame {
  play =  \(strat ::- Nil) -> let  v obs = do
                                           (_,env') <- strat
                                           let s obs = State env' obs
                                           action   <- ST.evalStateT  (chooseAction actionSpace (s obs)) (s obs)
                                           pure ((),action)
                                        in MonadOptic v (\_ -> (\_ -> pure ())),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(strat ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,pdenv') <- strat
                   (z,obs) <- h
                   -- ^ Take the (old observation) from the context
                   action <- ST.evalStateT  (chooseAction actionSpace (State pdenv' obs)) (State pdenv' obs)
                   (reward,obsNew) <- k z action
                   let st = (_obsAgent pdenv')
                   bounds <- liftIO (A.getBounds (_qTable pdenv'))
                   RIO.glog (RewardMsg
                               Reward
                                 { rewardPlayer = _player pdenv'
                                 , rewardIteration = (1 + _iteration pdenv')
                                 , rewardStateAction = (st, action)
                                 , rewardStateActionIndex = Ix.index bounds (st, toIdx action)
                                 , rewardReward = reward})
                   (State env' _) <- ST.execStateT (updateQTable actionSpace (State pdenv' obs) obsNew  action reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}




{-# INLINE deterministicStratStage #-}
deterministicStratStage ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a) =>  Agent -> (o a -> a) -> QLearningStageGame m '[m a] '[m a] (o a) () a  (Double,(o a))
deterministicStratStage name policy = OpenGame {
  play =  \(_ ::- Nil) -> let v obs = pure $ ((),policy obs)
                              in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = \(a ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,obs) <- h
                   -- ^ Take the (old observation) from the context
                   pure $ policy obs
                in (output ::- Nil)}

--------------------------
-- 4 Further functionality


{-# INLINE fromLens #-}
fromLens :: Monad m => (x -> y) -> (x -> r -> s) -> QLearningStageGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> pure (x, v x)) (\x -> (\r -> pure $ u x r)),
  evaluate = \Nil _ -> Nil}



{-# INLINE fromFunctions #-}
fromFunctions :: Monad m => (x -> y) -> (r -> s) -> QLearningStageGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)
