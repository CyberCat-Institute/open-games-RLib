{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Examples.ExternalEnvironment.TrustGame where

import           Control.Exception                   (throw, handle, fromException, Exception, SomeException(..))
import           Control.Monad                       (forever, when)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (encode, decode)
import           Data.ByteString.Lazy.Internal       (ByteString)
import           Data.Tuple.Extra                    (uncurry3)
import           Engine.Engine hiding                (fromLens, fromFunctions, state, nature)
import           Engine.ExternalEnvironment          (ExternalEnvironmentGame, fromFunctions, fromLens, interactWithEnv)
import           Examples.ExternalEnvironment.Common (extractNextState)
import           Examples.SequentialMoves            (trustGamePayoffProposer, trustGamePayoffResponder, Pie, Factor, Sent, SentBack)
import           Network.WebSockets.Connection       (PendingConnection)
import           Preprocessor.Preprocessor
import           Servant                             (Handler)
import qualified Network.WebSockets                  as WS

data GameException = BadSentInputException     { got :: Double, max :: Double }
                   | BadSentBackInputException { got :: Double, max :: Double }
  deriving (Show, Exception)

wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do
      -- Constants: Note, we read them as input.
      (Just (pie, factor)) <- decode <$> WS.receiveData @ByteString connection

      -- Note: This has been modified so that the input it requests from
      -- Python is always a _fraction_ (between 0 and 1), and then we compute
      -- the actual amounts in the appropriate way. This simplifies the Python
      -- RL model design.

      -- Game 1
      --  - Ask for an amount to send
      (Just sentInputF) <- decode <$> WS.receiveData @ByteString connection

      let sentIsValid = sentInputF >= 0 && sentInputF <= 1
          sentInput = sentInputF * pie

      WS.sendTextData connection (encode sentIsValid) -- true = valid

      when (not sentIsValid) $ do
        throw $ BadSentInputException sentInputF 1

      let step1 :: List '[Double]
          step1 = sentInput ::- Nil
          game1 = play proposerDecision step1

      sent <- extractNextState game1 pie

      -- Game 2
      --  - Ask for an amount to send back.
      (Just sentBackInputF) <- decode <$> WS.receiveData @ByteString connection

      let sentBackInputIsValid = sentBackInputF >= 0 && sentBackInputF <= 1
          sentBackInput = sentBackInputF * sent * factor

      WS.sendTextData connection (encode sentBackInputIsValid) -- true = valid

      when (not sentBackInputIsValid) $ do
        throw $ BadSentBackInputException sentBackInputF 1

      let step2 :: List '[Double]
          step2 = sentBackInput ::- Nil
          game2 = play responderDecision step2

      sentBack <- extractNextState game2 sent

      -- Game 3 & 4
      --  - Compute the payoffs and send them back.
      let game3 = play proposerPayoff Nil
      payoff1 <- extractNextState game3 (pie, sent, sentBack)

      let game4 = play responderPayoff Nil
      payoff2 <- extractNextState game4 (factor, sent, sentBack)

      WS.sendTextData connection (encode (payoff1, payoff2))
  where
    disconnect :: SomeException -> IO ()
    disconnect e =
      case fromException e :: Maybe GameException of
        Just ge -> putStrLn ("Bad game play: " ++ show ge ++ ". Goodbye.") >> pure ()
        _       -> pure ()

-- 1.1. Trust Game
trustGame :: p
             -> Factor
             -> ExternalEnvironmentGame
                  '[Double, Double]
                  '[]
                  ()
                  (Double, Double)
                  (Double, Double)
                  ()
trustGame pie factor = [opengame|
   inputs    :      ;
   feedback  :  (payoff1,payoff2)  ;
   :----------------------------:
   inputs    :   ;
   feedback  : payoff1     ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : trustGamePayoffProposer factor sent sentBack; // TODO: should factor be pie here?

   inputs    : sent;
   feedback  : payoff2     ;
   operation : interactWithEnv;
   outputs   : sentBack ;
   returns   : trustGamePayoffResponder factor sent sentBack ;
   :----------------------------:
   outputs   :  (sent,sentBack)  ;
   returns   :      ;
   |]

-- 2. splitting things into components

proposerDecision :: ExternalEnvironmentGame
                      '[Double]
                      '[]
                      Pie
                      ()
                      Sent
                      Double -- payoffProposer
proposerDecision = [opengame|
   inputs    : pie ;
   feedback  :   ;
   :----------------------------:
   inputs    : pie ;
   feedback  : ignorePayoff    ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : payoffProposer    ;
   :----------------------------:
   outputs   :  sent        ;
   returns   :  payoffProposer    ;
  |]


responderDecision :: ExternalEnvironmentGame
                       '[Double]
                       '[]
                       Double
                       ()
                       Double
                       Double
responderDecision = [opengame|
   inputs    : sent     ;
   feedback  :   ;
   :----------------------------:
   inputs    : sent  ;
   feedback  : ignorePayoff     ;
   operation : interactWithEnv ;
   outputs   : sentBack ;
   returns   : payoffResponder ;
   :----------------------------:
   outputs   :  sentBack         ;
   returns   :  payoffResponder  ;
   |]


proposerPayoff :: ExternalEnvironmentGame
                    '[]
                    '[]
                    (Pie, Sent, SentBack)
                    ()
                    Payoff
                    ()
proposerPayoff = [opengame|
   inputs    : pie, sent, sentBack    ;
   feedback  :   ;
   :----------------------------:
   inputs    : pie, sent, sentBack  ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 trustGamePayoffProposer) id;
   outputs   : payoffSender;
   returns   :  ;
   :----------------------------:
   outputs   :  payoffSender    ;
   returns   :      ;
  |]

responderPayoff :: ExternalEnvironmentGame
                     '[]
                     '[]
                     (Factor, Sent, SentBack)
                     ()
                     Payoff
                     ()
responderPayoff = [opengame|
   inputs    : factor, sent, sentBack    ;
   feedback  :   ;
   :----------------------------:
   inputs    : factor, sent, sentBack  ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 trustGamePayoffResponder) id;
   outputs   : payoffResponder;
   returns   :  ;
   :----------------------------:
   outputs   :  payoffResponder    ;
   returns   :      ;
  |]

-- Trustless
--   I give you 0
--   you get 0
--   you give me back 0
--
-- Trusty
--   I give you X
--   you get 3X             (-2x for the manager)
--   you give me back 2X    (+x p1, +x p2)

-- issue: in proposerDecision: unenforced logic: 0 <= sent <= pie
-- issue: sendBack is unrestricted; it doesn't even depend on factor currently
-- 0 <= sentBack <= factor * sent

-- e.g. (test 100 10 6 3)

test :: Pie -> Sent -> SentBack -> Factor -> IO (Sent, SentBack, Payoff, Payoff)
test pie sentInput sentBackInput factor = do
  -- GAME 1
  let strategy1 :: List '[Double]
      strategy1 = sentInput ::- Nil

      game1 :: MonadOptic IO Pie () Double Double
      game1 = play proposerDecision strategy1
  sent <- extractNextState game1 pie

  -- GAME 2
  let strategy2 :: List '[Double]
      strategy2 = sentBackInput ::- Nil

      game2 :: MonadOptic IO Double () Double Double
      game2 = play responderDecision strategy2
  sentBack <- extractNextState game2 sent

  -- GAME 3
  let game3 :: MonadOptic IO (Pie, Sent, SentBack) () Double ()
      game3 = play proposerPayoff Nil
  payoff1 <- extractNextState game3 (pie, sent, sentBack)

  -- GAME 4
  let game4 :: MonadOptic IO (Factor, Sent, SentBack) () Double ()
      game4 = play responderPayoff Nil
  payoff2 <- extractNextState game4 (factor, sent, sentBack)

  return (sent, sentBack, payoff1, payoff2)

