module Evaluator.State (State) where

import Data.Text (Text)

import Data.Map (Map)

import Evaluator.Variable (Variable)


type State = [Map Text Variable]
