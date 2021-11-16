module Evaluator.Internal (
  Evaluator (..),
  Configuration (..),
) where

import Control.Monad.Trans.Class

import Evaluator.State
import Syntax


newtype Evaluator m a =
  Evaluator {runEvaluator :: Configuration m -> State -> m (a, State)}
  deriving Functor


data Configuration m = Configuration {
  root :: Devin,
  beforeStatement :: Statement -> Evaluator m (),
  afterStatement :: Statement -> Evaluator m (),
  beforeExpression :: Expression -> Evaluator m (),
  afterExpression :: Expression -> Evaluator m ()
}


instance Monad m => Applicative (Evaluator m) where
  pure x = Evaluator $ \_ state -> pure (x, state)

  evaluator1 <*> evaluator2 = Evaluator $ \configuration state -> do
    (f, state') <- runEvaluator evaluator1 configuration state
    (x, state'') <- runEvaluator evaluator2 configuration state'
    pure (f x, state'')


instance Monad m => Monad (Evaluator m) where
  evaluator >>= f = Evaluator $ \configuration state -> do
    (x, state') <- runEvaluator evaluator configuration state
    runEvaluator (f x) configuration state'


instance MonadFail m => MonadFail (Evaluator m) where
  fail message = Evaluator $ \_ _ -> fail message


instance MonadTrans Evaluator where
  lift mx = Evaluator $ \_ state -> do
    x <- mx
    pure (x, state)
