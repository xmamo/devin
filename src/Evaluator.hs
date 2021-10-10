module Evaluator (
  EvaluatorT (..),
  Evaluator,
  evaluator,
  getConfiguration,
  getVariable,
  setVariable,
  updateVariable,
  push,
  pop
) where

import Data.Functor.Identity
import Data.List

import Data.Text (Text)

import Data.Map ((!?))
import qualified Data.Map as Map

import Evaluator.State (State)
import Evaluator.Variable (Variable)

import Evaluator.Internal


type Evaluator = EvaluatorT Identity


evaluator :: Applicative m => (Configuration m -> State -> (a, State)) -> EvaluatorT m a
evaluator f = EvaluatorT \configuration state -> pure (f configuration state)


getConfiguration :: Applicative m => EvaluatorT m (Configuration m)
getConfiguration = evaluator (,)


getVariable :: Applicative m => Text -> EvaluatorT m Variable
getVariable name = evaluator \_ state -> (go state, state)
  where
    go [] = undefined

    go (current : parents) = case current !? name of
      Just variable -> variable
      Nothing -> go parents



setVariable :: Applicative m => Text -> Variable -> EvaluatorT m ()
setVariable name variable = evaluator \_ (current : parents) ->
  ((), Map.insert name variable current : parents)


updateVariable :: Applicative m => Text -> (Variable -> Variable) -> EvaluatorT m ()
updateVariable name f = evaluator \_ state -> ((), go state)
  where
    go [] = undefined

    go (current : parents) = case current !? name of
      Just variable -> Map.insert name (f variable) current : parents
      Nothing -> go parents


push :: EvaluatorT m a -> EvaluatorT m a
push evaluator = EvaluatorT \configuration state ->
  runT evaluator configuration (Map.empty : state)


pop :: Integer -> EvaluatorT m a -> EvaluatorT m a
pop depth evaluator = EvaluatorT \configuration state ->
  runT evaluator configuration (genericDrop depth state)
