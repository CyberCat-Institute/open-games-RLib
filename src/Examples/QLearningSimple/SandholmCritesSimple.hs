{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
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


module Examples.QLearning.SandholmCritesSimple
                     ( evalStageLS
                     , initiateStrat
                     , sequenceL
                     , evalStageM
                     )
                     where

import qualified Engine.Memory as Memory
import           Control.Comonad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import qualified Data.Array.IO as A
import           Data.Coerce
import           Data.Function
import           Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           GHC.Generics
import           Language.Haskell.TH
import qualified System.Random as Rand

import           Engine.QLearningSimple
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.AbstractSyntax
import           Preprocessor.Compile
import           Preprocessor.THSyntax

type N = 1

newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,A.Ix,Ord, Eq, Functor)

------------------
-- Introducing specialized learning rule:

-- Choose the optimal action given the current state
chooseBoltzQTable ::
     MonadIO m
  => CTable Action
  -> State N Observation Action
  -> ST.StateT (State N Observation Action) m Action
chooseBoltzQTable ls s = do
    theMaxScore <- liftIO $ maxScore obsVec (_qTable s) ls
    let temp      = _temperature s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen s)
        q         = _qTable s
        chooseNoExplore =
            do
              let (_, gen'')   = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                  action'      = snd $  theMaxScore
              return action'
    qCooperate    <- liftIO $ A.readArray q (obsVec, toIdx (Action True))
    qDefect       <- liftIO $ A.readArray q (obsVec, toIdx (Action False))
    let chooseExplore  =
          do
            let
                eCooperate       = (exp 1.0)** qCooperate / temp
                eDefect          = (exp 1.0)** qDefect / temp
                probCooperate    = eCooperate / (eCooperate + eDefect)
                (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                action'          = Action (if actionP < probCooperate then True else False)
            return action'
        in if temp < 0.01
           then chooseNoExplore
           else chooseExplore
  where obsVec = _obsAgent s

-- choose optimally or explore according to the Boltzmann rule
chooseUpdateBoltzQTable ::
     MonadIO m
  => CTable Action
  -> State N Observation Action
  -> Observation Action
  -> Action
  -> Double
  -> ST.StateT (State N Observation Action) m Action
chooseUpdateBoltzQTable ls s obs2 action reward  = do
    let q         = _qTable s
    prediction    <- liftIO $ A.readArray q (obsVec, toIdx action)
    let temp      = _temperature s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen s)


    maxed <- liftIO $ maxScore (Memory.pushEnd obsVec (fmap toIdx (_obs s))) q ls
    let updatedValue = reward + gamma * (fst $ maxed)
        newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
        -- newQ         = q A.// [((_obs s, action), newValue)]
    liftIO $ A.writeArray q (Memory.pushEnd obsVec (fmap toIdx (_obs s)), toIdx action) newValue
    ST.put $ updateRandomGQTableTemp (0.999) s gen'
    return action
  where obsVec = _obsAgent s




------------------
-- Types

newtype Action = Action Bool  deriving (Show, Eq, Ord)
instance ToIdx Action where
  toIdx =
    \case
      Action False -> Idx 0
      Action True -> Idx 1

actionSpace :: CTable Action
actionSpace = uniformCTable (fmap coerce (V.fromList [False,True]))

-------------
-- Parameters

gamma = 0.8

learningRate = 0.40



pdMatrix :: Action -> Action -> Double
pdMatrix = on pdMatrix' coerce

pdMatrix' :: Bool -> Bool -> Double
pdMatrix' True True = 0.3
pdMatrix' True False = 0
pdMatrix' False True = 0.5
pdMatrix' False False = 0.1

-- Policy for deterministic player
titForTat :: Observation Action -> Action
titForTat (Obs (_,Action True))   = Action True
titForTat (Obs (_,Action False))  = Action False

-----------------------------------
-- Constructing initial state
-- TODO change this; make it random
lstIndexValues = [ (((Action x, Action y),Action z),v) | (((x,y),z),v) <-
  [(((True,True), True),0),(((True,False),True),0),(((False,True),True), 0),(((False,False),True),0),
    (((True,True), False),0),(((True,False),False),0),(((False,True),False), 0),(((False,False),False),0)]]

lstIndexValues2 = [
  (((True,True), True),4),(((True,False),True),0),(((False,True),True), 2),(((False,False),True),1),
   (((True,True), False),5),(((True,False),False),2),(((False,True),False), 4),(((False,False),False),1)]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: MonadIO m => m (QTable N Observation Action)
initialArray = liftIO (do
   arr <- A.newArray_ (asIdx l, asIdx u)
   traverse (\(k, v) -> A.writeArray arr (asIdx k) v) lstIndexValues
   pure arr)
  where
    l = minimum $ fmap fst lstIndexValues
    u = maximum $ fmap fst lstIndexValues
    asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)


-- initialEnv and parameters
initialEnv1 :: IO (State N Observation Action)
initialEnv1 = initialArray >>= \arr -> pure $ State "Player1" arr 0  0.2  (Rand.mkStdGen 3) (fmap (fmap toIdx) (Memory.fromSV(SV.replicate initialObservation))) initialObservation (5 * 0.999)
initialEnv2 :: IO (State N Observation Action)
initialEnv2 = initialArray >>= \arr ->  pure $ State "PLayer2" arr 0  0.2  (Rand.mkStdGen 100) (fmap (fmap toIdx) (Memory.fromSV(SV.replicate initialObservation))) initialObservation (5 * 0.999)
-- ^ Value is taking from the benchmark paper Sandholm and Crites


---------------------------------------
-- Preparing the context for evaluation
-- Initial observation
-- TODO randomize it
initialObservation :: Observation Action
initialObservation = fmap coerce $ Obs (True,True)

-- | the initial observation is used twice as the context for two players requires it in that form
initialObservationContext :: (Observation Action, Observation Action)
initialObservationContext = (initialObservation,initialObservation)


initialContext :: Monad m => MonadicLearnLensContext m (Observation Action,Observation Action) () (Action,Action) ()
initialContext = MonadicLearnLensContext (pure initialObservationContext) (pure (\(_,_) -> pure ()))

-- initialstrategy
initiateStrat :: IO (List '[IO (Action, State N Observation Action ), IO Action])
initiateStrat = do e <- initialEnv1
                   pure $ pure (Action True,e) ::- pure (Action True) ::- Nil


------------------------------
-- Updating state


toObs :: Monad m => m (Action,State N Observation Action) -> m Action -> m ((), ((),Observation Action))
toObs a1 a2 = do
             (act1,env1) <- a1
             act2 <- a2
             let obs1 = Obs (act1,act2)
                 obs2 = Obs (act2,act1)
                 in return ((),((),obs2))


toObsFromLS :: Monad m => List '[m (Action,State N Observation Action), m Action] -> m ((), ((),Observation Action))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (Action,State N Observation Action), m Action] ->
                     MonadContext m ((),Observation Action) () (Action,Action) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))




------------------------------
-- Game stage 1
stageDeterministic = [opengame|
   inputs    : (), state2  ;
   feedback  :   ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : pureDecisionQStage actionSpace "Player1" chooseBoltzQTable chooseUpdateBoltzQTable ;
   outputs   :  act1 ;
   returns   :  (pdMatrix act1 act2, Obs (act1,act2)) ;

   inputs    :  state2    ;
   feedback  :      ;
   operation : deterministicStratStage "Player2" titForTat ;
   outputs   :  act2 ;
   returns   :  (pdMatrix act2 act1, Obs (act1,act2))    ;
   :-----------------:

   outputs   :  (act1, act2)    ;
   returns   :      ;

|]


----------------------------------
-- Defining the iterator structure
evalStage ::
     List '[ IO (Action, State N Observation Action), IO Action]
  -> MonadContext IO ((), Observation Action) () (Action, Action) ()
  -> List '[ IO (Action, State N Observation Action), IO Action]
evalStage  strat context  = evaluate stageDeterministic strat context



-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]


hoist ::
     Applicative f
  => List '[ (Action, State N Observation Action), Action]
  -> List '[ f (Action, State N Observation Action), f Action]
hoist (x ::- y ::- Nil) = pure x ::- pure y ::- Nil

sequenceL ::
     Monad f
  => List '[ f (Action, State N Observation Action), f Action]
  -> f (List '[ (Action, State N Observation Action), Action])
sequenceL (x ::- y ::- Nil) = do
  v <- x
  v' <- y
  pure (v ::- v' ::- Nil)

evalStageM ::
     List '[ (Action, State N Observation Action), Action]
  -> Int
  -> IO [List '[ (Action, State N Observation Action), Action]]
evalStageM startValue 0 = pure []
evalStageM startValue n = do
  newStrat <-
    sequenceL
      (evalStage (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM newStrat (pred n)
  pure (newStrat : rest)
