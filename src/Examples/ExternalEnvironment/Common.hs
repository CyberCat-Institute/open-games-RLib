module Examples.ExternalEnvironment.Common (extractNextState, extractPayoffAndNextState) where

import OpenGames.Engine.Engine

---------------------------------------------------------------------
-- Utility functions to extract the next state and payoff from a
-- monad optic. TODO: Perhaps there is a better location for these
-- functions.
---------------------------------------------------------------------

extractNextState :: MonadOpticLearning m s t a b -> s -> m a
extractNextState (MonadOpticLearning v _) x = do
  (_, nextState) <- v x
  pure nextState

extractPayoffAndNextState :: MonadOpticLearning m s t a b -> s -> b -> m (t, a)
extractPayoffAndNextState (MonadOpticLearning v u) x r = do
  (z, nextState) <- v x
  payoff <- u z r
  pure (payoff, nextState)

