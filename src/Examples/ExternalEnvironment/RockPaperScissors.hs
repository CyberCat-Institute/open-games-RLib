{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Examples.ExternalEnvironment.RockPaperScissors where

---------------------------------------------------------------------
-- Open game implementation of the Rock-Paper-Scissors game
---------------------------------------------------------------------

import           Control.Exception                   (SomeException, handle)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (ToJSON, FromJSON, encode, decode)
import           Data.ByteString.Lazy.Internal       (ByteString)
import           OpenGames.Engine.Engine hiding                (fromLens, fromFunctions, state)
import           Engine.ExternalEnvironment          (ExternalEnvironmentGame, fromFunctions, fromLens, interactWithEnv)
import           Examples.ExternalEnvironment.Common (extractPayoffAndNextState)
import           GHC.Generics                        (Generic)
import           Network.WebSockets.Connection       (PendingConnection)
import           OpenGames.Preprocessor
import           Servant                             (Handler)
import qualified Network.WebSockets                  as WS

-- Types

data ActionRPS = Rock | Paper | Scissors
  deriving (Ord, Eq, Show, Generic, ToJSON, FromJSON)

data PlayParameters = PlayParameters
  { player1Action :: ActionRPS
  , player2Action :: ActionRPS
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { player1Payoff  :: Double
  , player2Payoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Payoff function

rockPaperScissorsPayoff :: ActionRPS -> ActionRPS -> Double
rockPaperScissorsPayoff Rock Rock = 0
rockPaperScissorsPayoff Rock Paper = -1
rockPaperScissorsPayoff Rock Scissors = 1
rockPaperScissorsPayoff Paper Rock = 1
rockPaperScissorsPayoff Paper Paper = 0
rockPaperScissorsPayoff Paper Scissors = -1
rockPaperScissorsPayoff Scissors Rock = -1
rockPaperScissorsPayoff Scissors Paper = 1
rockPaperScissorsPayoff Scissors Scissors = 0

wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do
      -- Receive and decode the action of each player
      Just (PlayParameters { player1Action, player2Action }) <- decode <$> WS.receiveData @ByteString connection

      -- Play the game to compute the payoffs
      let strategy = player1Action ::- player2Action ::- Nil
          nextgame = play rockPaperScissorsExternal strategy
      ((p1, p2), _) <- extractPayoffAndNextState nextgame () ()

      -- Send the payoffs back to the client
      let playResult = PlayResult { player1Payoff = p1, player2Payoff = p2 }
      let response = encode playResult
      WS.sendTextData connection response

      pure ()
    pure ()
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

-- Game

rockPaperScissorsExternal :: ExternalEnvironmentGame
                               '[ActionRPS, ActionRPS]
                               '[]
                               ()
                               (Double, Double)
                               (ActionRPS, ActionRPS)
                               ()
rockPaperScissorsExternal = [opengame|
   inputs    :      ;
   feedback  : (payoff1,payoff2)     ;
   :----------------------------:
   inputs    :      ;
   feedback  : payoff1    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer1 ;
   returns   : rockPaperScissorsPayoff decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  : payoff2    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer2 ;
   returns   : rockPaperScissorsPayoff decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
  |]

