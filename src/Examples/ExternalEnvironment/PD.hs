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

module Examples.ExternalEnvironment.PD where

---------------------------------------------------------------------
-- Open game implementation of the Prisoner's Dilemma game
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

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Payoff
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1



data PlayParameters = PlayParameters
  { player1Action :: ActionPD
  , player2Action :: ActionPD
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { player1Payoff  :: Double
  , player2Payoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do
      -- Receive and decode the action of each player
      Just (PlayParameters { player1Action, player2Action }) <- decode <$> WS.receiveData @ByteString connection

      -- Play the game to compute the payoffs
      let strategy = player1Action ::- player2Action ::- Nil
          nextgame = play prisonersDilemmaExternal strategy
      ((p1, p2), _) <- extractPayoffAndNextState nextgame () ()

      -- Send the payoffs back to the client
      let playResult = PlayResult { player1Payoff = p1, player2Payoff = p2 }
      let response = encode playResult
      WS.sendTextData connection response
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

prisonersDilemmaExternal :: ExternalEnvironmentGame
                              '[ActionPD, ActionPD]
                              '[]
                              ()
                              (Double, Double)
                              (ActionPD, ActionPD)
                              ()
prisonersDilemmaExternal = [opengame|
   inputs    :      ;
   feedback  : (payoff1,payoff2)     ;
   :----------------------------:
   inputs    :      ;
   feedback  : payoff1    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  : payoff2    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
  |]

