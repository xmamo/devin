module Result (Result (..)) where

import Data.Text (Text)

import Input (Input)


data Result a where
  Success :: a -> Input -> Result a
  Failure :: Bool -> Int -> [Text] -> Result a
  deriving (Eq, Functor, Foldable, Traversable, Show, Read)
