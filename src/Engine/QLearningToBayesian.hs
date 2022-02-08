{-# LANGUAGE BangPatterns #-}
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



module Engine.QLearningToBayesian where

import qualified Engine.Memory as Memory
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.QLearning (QTable(..), Idx(..), ToIdx (..), CTable(..),maxScore, QLearningMsg(..))

import           Control.Arrow                      hiding ((+:+))
import           Control.Monad.Reader
import qualified Data.Array.IArray as IA
import qualified Data.Array.IO as A
import           Data.Array.IO
import           Data.Foldable
import           Data.Ix
import qualified Data.Ix as Ix
import           Data.List (maximumBy,nub,sortBy)
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Numeric.Probability.Distribution hiding (map, lift, filter)
import qualified Optics.Lens as L
import           RIO
import           System.IO.Unsafe


---------------------------------------------------------------------
-- EXPERIMENTAL: This module implements a connector between learned
-- policies and the Bayesian framework
---------------------------------------------------------------------
-- transforms the q table into a pure strategy
fromQMatrixToStrategy ::
  ( ToIdx a, Ix (o (Idx a))
  , Ix (Memory.Vector n (o (Idx a)))
  , MonadReader r IO
  , HasGLogFunc r
  , Num prob
  , Functor o
  , Ord a
  , GMsg r ~ QLearningMsg n o a
  )
  => QTable n o a
  -> Int
  -> CTable a
  -> Kleisli Stochastic (Memory.Vector n (o (Idx a))) a
fromQMatrixToStrategy table playerNo support =
  Kleisli $  
    \obs -> fromIOToStoch obs table support playerNo
 where
    helper obs table support playerNo = do
      a <-  maxScore obs table support playerNo
      return $ certainly $ snd a
    fromIOToStoch obs table support playerNo = unsafePerformIO $ helper obs table support playerNo

-- Constructs the strategy from a simpler list of associations between (state,action) pairs
fromListToStrategy :: [((s,a),Double)] -> Kleisli Stochastic s a
fromListToStrategy ls = Kleisli $ \state -> certainly $ findOptimalAction ls state
  where
    findOptimalAction :: [((s,a),Double)] -> s -> a
    findOptimalAction ls state = fst $ maximumBy (comparing snd) ls'
      where
        ls' = [(action,value) | ((_,action),value) <- ls]