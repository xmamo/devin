module Evaluator.Value (
  Value (..),
  t
) where

import Data.Int

import qualified Type


data Value where
  Unit :: Value
  Bool :: Bool -> Value
  Int :: Int64 -> Value
  Float :: Double -> Value
  deriving (Eq, Show, Read)


t :: Value -> Type.Type
t Unit = Type.Unit
t (Bool _) = Type.Bool
t (Int _) = Type.Int
t (Float _) = Type.Float
