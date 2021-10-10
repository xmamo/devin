module Evaluator.Internal (
  Configuration (..),
  EvaluatorT (..)
) where

import Data.Functor

import Control.Monad.Trans.Class

import Evaluator.State (State)


newtype EvaluatorT m a = EvaluatorT {runT :: Configuration m -> State -> m (a, State)}
  deriving Functor


data Configuration m = Configuration {
  beforeStatement :: EvaluatorT m (),
  afterStatement :: EvaluatorT m (),
  beforePredicate :: EvaluatorT m (),
  afterPredicate :: EvaluatorT m ()
}


instance Monad m => Applicative (EvaluatorT m) where
  pure a = EvaluatorT \_ state -> pure (a, state)

  evaluator1 <*> evaluator2 = EvaluatorT \configuration state -> do
    (f, state') <- runT evaluator1 configuration state
    (x, state'') <- runT evaluator2 configuration state'
    pure (f x, state'')


instance Monad m => Monad (EvaluatorT m) where
  evaluator >>= f = EvaluatorT \configuration state -> do
    (x, state') <- runT evaluator configuration state
    runT (f x) configuration state'


instance MonadFail m => MonadFail (EvaluatorT m) where
  fail string = EvaluatorT \_ _ -> fail string


instance MonadTrans EvaluatorT where
  lift ma = EvaluatorT \_ input -> ma <&> (, input)
