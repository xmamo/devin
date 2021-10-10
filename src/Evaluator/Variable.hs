module Evaluator.Variable (Variable (..)) where

import Data.Text (Text)

import Evaluator.Value (Value)


data Variable where
  ByValue :: Value -> Variable
  ByReference :: Text -> Variable
  deriving (Eq, Show, Read)
