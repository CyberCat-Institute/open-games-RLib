{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields #-}

import System.Random

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario



main :: IO ()
main = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gPrice1 <- newStdGen
  gPrice2 <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parameters =
        Scenario.Parameters
          { pKsi = 0.1
          , pBeta = (- 0.00001)
          , pBertrandPrice = 1.47
          , pMonopolyPrice = 1.92
          , pGamma = 0.95
          , pLearningRate = 0.15
          , pMu = 0.25
          , pA1 = 2
          , pA2 = 2
          , pA0 = 0
          , pC1 = 1
          , pM  = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
          , pGeneratorEnv1 = gEnv1
          , pGeneratorEnv2 = gEnv2
          , pGeneratorPrice1 = gPrice1
          , pGeneratorPrice2 = gPrice2
          , pGeneratorObs1 = gObs1
          , pGeneratorObs2 = gObs2
          }
      exportConfig =
        QLearning.ExportConfig
          { iterations = 100
          , outputEveryN = 1
          , incrementalMode = True
          , mapStagesM_ = Scenario.mapStagesM_ parameters
          , initial = Scenario.initialStrat parameters >>= Scenario.sequenceL
          , ctable = Scenario.actionSpace parameters
          , mkObservation = \a b -> Scenario.Obs (a, b)
          }
  BS.writeFile "parameters.csv" $ Scenario.csvParameters parameters
  QLearning.runQLearningExporting exportConfig
  putStrLn "completed task"
