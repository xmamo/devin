module Evaluator.State(
  State,
  predefined
) where

import Data.Text (Text)

import Data.Map (Map)
import Data.Map qualified as Map

import Value


type State = [(Int, Map Text Value)]


predefined :: State
predefined =
  [
    (0, Map.fromList [
      ("true", Bool True),
      ("false", Bool False),
      ("unit", Unit)
    ])
  ]
