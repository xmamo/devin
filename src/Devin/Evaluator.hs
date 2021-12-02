module Devin.Evaluator (
  Evaluator (..),
  runEvaluator,
  getRoot,
  getState,
  getVariable,
  setVariable,
  updateVariable,
  defineVariable,
  pushFrame,
  popFrame,
  withNewFrame,
  yield
) where

import Data.Text (Text)

import Data.Map ((!?))
import qualified Data.Map as Map

import Devin.Evaluator.State
import Devin.Range
import Devin.Syntax
import Devin.Value

import Devin.Evaluator.Internal


getRoot :: Applicative m => Evaluator m Devin
getRoot = Evaluator $ \root state -> pure (Done root, state)


getState :: Applicative m => Evaluator m State
getState = Evaluator $ \_ state -> pure (Done state, state)


getVariable :: Applicative m => Text -> Evaluator m Value
getVariable name = Evaluator $ \_ state -> pure (Done (go state), state)
  where
    go [] = undefined

    go ((offset, variables) : parents) = case variables !? name of
      Just variable -> variable
      Nothing -> go (drop (offset - 1) parents)


setVariable :: Applicative m => Text -> Value -> Evaluator m ()
setVariable name value = Evaluator $ \_ state -> pure (Done (), go state)
  where
    go [] = undefined

    go ((offset, variables) : parents) | Map.member name variables =
      (offset, Map.insert name value variables) : parents

    go ((offset, variables) : parents) = do
      let (parents1, parents2) = splitAt (offset - 1) parents
      (offset, variables) : (parents1 ++ go parents2)


updateVariable :: Applicative m => Text -> (Value -> Value) -> Evaluator m ()
updateVariable name f = Evaluator $ \_ state -> pure (Done (), go state)
  where
    go [] = undefined

    go ((offset, variables) : parents) = case variables !? name of
      Just variable ->
        (offset, Map.insert name (f variable) variables) : parents

      Nothing -> do
        let (parents1, parents2) = splitAt (offset - 1) parents
        (offset, variables) : (parents1 ++ go parents2)


defineVariable :: Applicative m => Text -> Value -> Evaluator m ()
defineVariable name value = Evaluator $ \_ ((offset, variables) : parents) ->
  pure (Done (), (offset, Map.insert name value variables) : parents)


pushFrame :: (Integral a, Applicative m) => a -> Evaluator m ()
pushFrame offset = Evaluator $ \root state ->
  pure (Done (), (fromIntegral offset, Map.empty) : state)


popFrame :: Applicative m => Evaluator m ()
popFrame = Evaluator $ \root state -> pure (Done (), tail state)


withNewFrame :: (Integral a, Monad m) => a -> Evaluator m b -> Evaluator m b
withNewFrame offset evaluator = do
  pushFrame offset
  x <- evaluator
  popFrame
  pure x


yield :: (Range a, Node a, Monad m) => a -> Evaluator m ()
yield node = Evaluator $ \_ state -> pure (Step node (pure ()), state)
