module Devin.Value (
  Value (..),
  pretty
) where

import Data.Int

import Data.Text (Text)
import qualified Data.Text as Text


data Value where
  Unit :: Value
  Bool :: Bool -> Value
  Int :: Int64 -> Value
  Float :: Double -> Value
  deriving (Eq, Show, Read)


pretty :: Value -> Text
pretty Unit = "unit"
pretty (Bool True) = "true"
pretty (Bool False) = "false"
pretty (Int x) = Text.pack (show x)
pretty (Float x) = Text.pack (show x)
