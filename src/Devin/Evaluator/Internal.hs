module Devin.Evaluator.Internal (
  Evaluator (..),
  Result (..),
  runEvaluator
) where

import Control.Monad.Trans.Class

import Devin.Evaluator.State
import Devin.Range
import Devin.Syntax


newtype Evaluator m a = Evaluator (Devin -> State -> m (Result m a, State))


data Result m a where
  Done :: {value :: a} -> Result m a
  Step :: (Range b, Node b) => {node :: b, next :: Evaluator m a} -> Result m a


instance Monad m => Functor (Result m) where
  fmap f (Done x) = Done (f x)
  fmap f (Step node evaluator) = Step node (f <$> evaluator)


instance Monad m => Functor (Evaluator m) where
  fmap f evaluator1 = Evaluator $ \root state -> do
    (result, state') <- runEvaluator evaluator1 root state

    case result of
      Done x -> pure (Done (f x), state')
      Step node evaluator2 -> pure (Step node (f <$> evaluator2), state')


instance Monad m => Applicative (Evaluator m) where
  pure x = Evaluator $ \_ state -> pure (Done x, state)

  evaluator1 <*> evaluator2 = Evaluator $ \root state -> do
    (result1, state') <- runEvaluator evaluator1 root state

    case result1 of
      Done f -> runEvaluator (f <$> evaluator2) root state'
      Step node mf -> pure (Step node (mf <*> evaluator2), state')


instance Monad m => Monad (Evaluator m) where
  evaluator1 >>= f = Evaluator $ \root state -> do
    (result1, state') <- runEvaluator evaluator1 root state

    case result1 of
      Done x -> runEvaluator (f x) root state'
      Step node evaluator2 -> pure (Step node (f =<< evaluator2), state')


instance MonadFail m => MonadFail (Evaluator m) where
  fail message = Evaluator $ \_ _ -> fail message


instance MonadTrans Evaluator where
  lift mx = Evaluator $ \_ state -> do
    x <- mx
    pure (Done x, state)


runEvaluator :: Evaluator m a -> Devin -> State -> m (Result m a, State)
runEvaluator (Evaluator f) = f
