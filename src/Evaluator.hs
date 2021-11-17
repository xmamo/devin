module Evaluator (
  Evaluator (..),
  getRoot,
  getConfiguration,
  getState,
  getVariable,
  setVariable,
  updateVariable,
  defineVariable,
  push
) where

import Data.Text (Text)

import Data.Map ((!?))
import qualified Data.Map as Map

import Evaluator.State
import Syntax
import Value

import Evaluator.Internal


getRoot :: Applicative m => Evaluator m Devin
getRoot = Evaluator $ \configuration state ->
  pure (configuration.root, state)


getConfiguration :: Applicative m => Evaluator m (Configuration m)
getConfiguration = Evaluator $ \configuration state ->
  pure (configuration, state)


getState :: Applicative m => Evaluator m State
getState = Evaluator $ \_ state ->
  pure (state, state)


getVariable :: Applicative m => Text -> Evaluator m Value
getVariable name = Evaluator $ \_ state -> pure (go state, state)
  where
    go [] = undefined

    go ((parent, variables) : parents) = case variables !? name of
      Just variable -> variable
      Nothing -> go (drop (parent - 1) parents)


setVariable :: Applicative m => Text -> Value -> Evaluator m ()
setVariable name variable = Evaluator $ \_ state -> pure ((), go state)
  where
    go [] = undefined

    go ((parent, variables) : parents) = case variables !? name of
      Just _ ->
        (parent, Map.insert name variable variables) : parents

      Nothing -> do
        let (parents1, parents2) = splitAt (parent - 1) parents
        (parent, variables) : (parents1 ++ go parents2)


updateVariable :: Applicative m => Text -> (Value -> Value) -> Evaluator m ()
updateVariable name f = Evaluator $ \_ state -> pure ((), go state)
  where
    go [] = undefined

    go ((parent, variables) : parents) = case variables !? name of
      Just variable ->
        (parent, Map.insert name (f variable) variables) : parents

      Nothing -> do
        let (parents1, parents2) = splitAt (parent - 1) parents
        (parent, variables) : (parents1 ++ go parents2)


defineVariable :: Applicative m => Text -> Value -> Evaluator m ()
defineVariable name variable = Evaluator $ \_ ((parent, variables) : parents) ->
  pure ((), (parent, Map.insert name variable variables) : parents)


push :: (Integral a, Monad m) => a -> Evaluator m b -> Evaluator m b
push parent evaluator = Evaluator $ \configuration state -> do
  let state' = (fromIntegral parent, Map.empty) : state
  (value, state'') <- runEvaluator evaluator configuration state'
  pure (value, tail state'')
