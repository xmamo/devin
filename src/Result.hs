module Result (Result (..)) where

import Data.Text (Text)

import Input (Input)


data Result a where
  Success :: {value :: a, rest :: Input} -> Result a
  Failure :: {isFatal :: Bool, position :: Int, expectations :: [Text]} -> Result a
  deriving (Eq, Functor, Foldable, Traversable, Show, Read)
