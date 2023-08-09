{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Examples.ExternalEnvironment.MontyHall where

---------------------------------------------------------------------
-- Open game implementation of the Monty Hall game
---------------------------------------------------------------------

import           Control.Exception                   (handle, SomeException(..))
import           Control.Monad                       (forever)
import           Control.Monad                       (replicateM)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (ToJSON, FromJSON)
import           Data.Aeson                          (encode, decode)
import           Data.ByteString.Lazy.Internal       (ByteString)
import           Data.Tuple.Extra                    (uncurry3)
import           OpenGames.Engine.Engine hiding (fromLens, fromFunctions, state, nature)
import           Engine.ExternalEnvironment          ( ExternalEnvironmentGame, fromFunctions, fromLens
                                                     , interactWithEnv, liftStochastic, nature )
import           Examples.ExternalEnvironment.Common (extractNextState)
import           GHC.Generics                        (Generic)
import           Network.WebSockets.Connection       (PendingConnection)
import           OpenGames.Preprocessor
import           Servant                             (Handler)
import           System.Random
import qualified Data.Set as S
import qualified Network.WebSockets                  as WS

-------------
-- Data types
type DoorCar  = Int
type Door     = Int
type DoorGoat = Int

type ChangeChoice = Bool

data PlayParameters = PlayParameters
  { theDoor :: Door
  , playerAction :: ChangeChoice
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { playerPayoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

-------------
-- Parameters

initialSetDoors :: S.Set DoorCar
initialSetDoors = S.fromList [1,2,3]

prize :: Double
prize = 10

----------------------
-- Auxiliary functions

-- Fix door behind which is the car
chooseWinningDoor :: IO DoorCar
chooseWinningDoor = do
  g <- newStdGen
  let winning_door = fst $ randomR (1,3) g
  return winning_door

-- Given a set of doors choose one randomly
chooseRandomDoorFromSet :: S.Set Door -> IO DoorGoat
chooseRandomDoorFromSet doors = do
  g <- newStdGen
  -- Note: randomR is inclusive in the bounds
  let index = fst $ randomR (0, S.size doors - 1) g
  let result = S.elemAt index doors
  return result

-- Given door with car behind and chosen door, reveal a door which contains a goat.
revealGoatDoor :: DoorCar -> Door -> IO DoorGoat
revealGoatDoor winner choice = do
  opened_door <- chooseRandomDoorFromSet $ S.delete choice $ S.delete winner initialSetDoors
  -- ^ ignore the winner door and the chosen door (might overlap, but that's alright)
  return opened_door

----------
-- Payoffs

payoffDecision :: DoorCar ->  Door -> ChangeChoice -> Double
payoffDecision winner doorChosen choiceChanged
  | winner == doorChosen && not choiceChanged  = prize
  -- ^ if you chose the right door initially and did not change your mind, you win
  | winner /= doorChosen && choiceChanged      = prize
  -- ^ if you chose the wrong door initially and change your mind, you win
  | otherwise = 0

-------
-- Game

-- Monolithic version of the game
montyHallExternal :: ExternalEnvironmentGame
                       '[Door,Bool]
                       '[]
                       ()
                       (Double)
                       (Door, Bool)
                       ()
montyHallExternal = [opengame|
   inputs    :      ;
   feedback  :  payoff    ;
   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature chooseWinningDoor ;
   outputs   : winningDoor ;
   returns   :     ;
   // Chooses behind which door the car is

   inputs    :     ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision1 ;
   returns   : 0 ;
   // first decision; fix payoff at zero

   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   operation : liftStochastic (uncurry revealGoatDoor) ;
   outputs   : revealedDoor ;
   returns   :  ;
   // if first decision did not win the prize, reveal one door which contains a goat

   inputs    : revealedDoor ;
   feedback  : payoff ;
   operation : interactWithEnv ;
   outputs   : decision2 ;
   returns   : payoffDecision winningDoor decision1 decision2 ;
   // reverse or keep first choice; if winning prize else zero
   :----------------------------:
   outputs   : decision1,decision2 ;
   returns   :    ;
  |]

-- Game 1: Choose a random door to hide the car behind.
chooseInitialDoor :: ExternalEnvironmentGame
                       '[]
                       '[]
                       ()
                       ()
                       DoorCar
                       ()
chooseInitialDoor = [opengame|
   inputs    :      ;
   feedback  :      ;
   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature chooseWinningDoor ;
   outputs   : winningDoor ;
   returns   :     ;
   // Chooses behind which door the car is
   :----------------------------:
   outputs   : winningDoor ;
   returns   :     ;
  |]

-- Game 2: Have the player pick a door.
playerChooseDoor :: ExternalEnvironmentGame
                      '[Door]
                      '[]
                      DoorCar
                      ()
                      Door
                      ()
playerChooseDoor = [opengame|
   inputs    : winningDoor ;
   feedback  :     ;
   :----------------------------:
   inputs    : winningDoor ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision1 ;
   returns   : 0   ;
   // first decision; fix payoff at zero
   :----------------------------:
   outputs   : decision1 ;
   returns   :     ;
  |]

-- Game 3: Have the presenter choose and open a goat door.
chooseGoatDoor :: ExternalEnvironmentGame
                    '[]
                    '[]
                    (DoorCar, Door)
                    ()
                    DoorGoat
                    ()
chooseGoatDoor = [opengame|
   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   operation : liftStochastic (uncurry revealGoatDoor) ;
   outputs   : revealedDoor ;
   returns   :  ;
   // if first decision did not win the prize, reveal one door which contains a goat
   :----------------------------:
   outputs   : revealedDoor ;
   returns   :     ;
  |]

-- Game 4: Have the player decide whether they wish to stick to their original
-- choice or whether they wish to switch to the remaining closed door.
playerChangeChoice :: ExternalEnvironmentGame
                        '[ChangeChoice]
                        '[]
                        (DoorCar, Door, DoorGoat)
                        ()
                        ChangeChoice
                        ()
playerChangeChoice = [opengame|
   inputs    : winningDoor,decision1,revealedDoor ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1,revealedDoor ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision2 ;
   returns   : 0 ;
   // reverse or keep first choice
   :----------------------------:
   outputs   : decision2 ;
   returns   :     ;
  |]

-- Game 5: Considering everything, reveal whether the player won or not.
playerFinalPayoff :: ExternalEnvironmentGame
                       '[]
                       '[]
                       (DoorCar, Door, ChangeChoice)
                       ()
                       Double
                       ()
playerFinalPayoff = [opengame|
   inputs    : winningDoor,decision1,decision2 ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1,decision2 ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 payoffDecision) id ;
   outputs   : payoff ;
   // compute the payoff
   :----------------------------:
   outputs   : payoff ;
   returns   :     ;
  |]

----------------
-- test the game

-- Duplicates the functionality of wsPlay but takes the inputs as arguments and
-- returns the results as outputs, instead of using a socket.
test :: PlayParameters -> IO PlayResult
test PlayParameters { theDoor, playerAction } = do
  -- Game 0: Choose the door behind which the car is.
  let strategy0 :: List '[]
      strategy0 = Nil

      game0' :: MonadOpticLearning IO () () DoorCar ()
      game0' = play chooseInitialDoor strategy0
  game0 <- extractNextState game0' ()

  -- Game 1: Ask the player for a first door choice (integer).
  let strategy1 :: List '[Door]
      strategy1 = theDoor ::- Nil

      game1' :: MonadOpticLearning IO DoorCar () Door ()
      game1' = play playerChooseDoor strategy1
  game1 <- extractNextState game1' (game0)

  -- Game 2: Choose and open a door hiding a goat.
  let strategy2 :: List '[]
      strategy2 = Nil

      game2' :: MonadOpticLearning IO (DoorCar, Door) () DoorGoat ()
      game2' = play chooseGoatDoor strategy2
  game2 <- extractNextState game2' (game0, game1)

  -- Game 3: Ask the player whether they want to change their choice (boolean).
  let strategy3 :: List '[ChangeChoice]
      strategy3 = playerAction ::- Nil

      game3' :: MonadOpticLearning IO (DoorCar, Door, DoorGoat) () ChangeChoice ()
      game3' = play playerChangeChoice strategy3
  game3 <- extractNextState game3' (game0, game1, game2)

  -- Game 4: Compute the payoff for the player, given (most of) the history.
  let strategy4 :: List '[]
      strategy4 = Nil

      game4' :: MonadOpticLearning IO (DoorCar, Door, ChangeChoice) () Double ()
      game4' = play playerFinalPayoff strategy4
  payoff <- extractNextState game4' (game0, game1, game3) -- NOTE: skipped game2

  return $ PlayResult { playerPayoff = payoff }

switch :: Int -> IO Double
switch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = True })))

dontSwitch :: Int -> IO Double
dontSwitch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = False })))

-----------------------------
-- play the game via a socket

wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do

      -- Game 0: Choose the door behind which the car is.
      let strategy0 :: List '[]
          strategy0 = Nil

          game0' :: MonadOpticLearning IO () () DoorCar ()
          game0' = play chooseInitialDoor strategy0
      game0 <- extractNextState game0' ()

      -- Game 1: Ask the player for a first door choice (integer).
      (Just firstDoorChoice) <- decode <$> WS.receiveData @ByteString connection

      let strategy1 :: List '[Door]
          strategy1 = firstDoorChoice ::- Nil

          game1' :: MonadOpticLearning IO DoorCar () Door ()
          game1' = play playerChooseDoor strategy1
      game1 <- extractNextState game1' (game0)

      -- TODO: Ensure that the integer given is in [1..3]

      -- Game 2: Choose and open a door hiding a goat.
      let strategy2 :: List '[]
          strategy2 = Nil

          game2' :: MonadOpticLearning IO (DoorCar, Door) () DoorGoat ()
          game2' = play chooseGoatDoor strategy2
      game2 <- extractNextState game2' (game0, game1)

      -- Inform the client about the door that was opened by the presenter
      WS.sendTextData connection (encode game2)

      -- Game 3: Ask the player whether they want to change their choice (boolean).
      (Just changeChoice) <- decode <$> WS.receiveData @ByteString connection

      let strategy3 :: List '[ChangeChoice]
          strategy3 = changeChoice ::- Nil

          game3' :: MonadOpticLearning IO (DoorCar, Door, DoorGoat) () ChangeChoice ()
          game3' = play playerChangeChoice strategy3
      game3 <- extractNextState game3' (game0, game1, game2)

      -- Game 4: Compute the payoff for the player, given (most of) the history.
      let strategy4 :: List '[]
          strategy4 = Nil

          game4' :: MonadOpticLearning IO (DoorCar, Door, ChangeChoice) () Double ()
          game4' = play playerFinalPayoff strategy4

      payoff <- extractNextState game4' (game0, game1, game3) -- NOTE: skipped game2

      WS.sendTextData connection (encode payoff)
  where
    disconnect :: SomeException -> IO ()
    disconnect e = putStrLn ("Disconnecting. Error: " ++ show e) >> pure ()

