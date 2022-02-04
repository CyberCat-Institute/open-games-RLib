{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.InteractiveIO.InteractiveIOM where

import Engine.QLearningToIO
import Engine.OpenGames
import Engine.OpticClass
import Engine.TLL
import Preprocessor.Preprocessor



import Control.Monad.Reader hiding (void)

------------------------------------------------------------------------
-- EXPERIMENTAL Implement a simple interactive game which does evaluation accordingly

-----------------------
-- 1. Types and payoffs

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1

--------------------
-- 1. Representation
-- 1.0 Prisoner's dilemma

prisonersDilemmaIO ::
                       InteractiveMonadicStageGame IO
                       '[ActionPD, ActionPD]
                       '[IO [DiagnosticInfoInteractive ActionPD], IO [DiagnosticInfoInteractive ActionPD]]
                       ()
                       ()
                       ()
                       ()
prisonersDilemmaIO = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : interactiveMonadicInput "player1" [Cooperate,Defect];
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : interactiveMonadicInput "player2" [Cooperate,Defect];
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


prisonersDilemmaIOLearner ::
                       InteractiveMonadicStageGame IO
                       '[ActionPD, ActionPD]
                       '[IO [DiagnosticInfoInteractive ActionPD], IO [DiagnosticInfoInteractive ActionPD]]
                       ()
                       ()
                       ()
                       ()
prisonersDilemmaIOLearner = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : interactiveMonadicInput "player1" [Cooperate,Defect];
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : interactiveMonadicInput "player2" [Cooperate,Defect];
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]




-- interfacing outside world
inputStrat x = if x == "cooperate" then Cooperate else Defect


evaluateGame = do
    let strategyTuple = Cooperate ::- Cooperate ::- Nil
        (dia1 ::- dia2 ::- Nil) = evaluate prisonersDilemmaIO strategyTuple void
    dia1' <- dia1
    print $ show $ optimalPayoffIO  $ head dia1'
