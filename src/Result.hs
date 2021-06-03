module Result (Result (..)) where

import Data.Functor.Classes

import Data.Text (Text)

import Input (Input)


data Result a where
  Success :: {value :: a, rest :: Input} -> Result a
  Failure :: {isFatal :: Bool, position :: Int, expectations :: [Text]} -> Result a
  deriving (Eq, Functor, Foldable, Traversable, Show, Read)


instance Eq1 Result where
  liftEq eq (Success value1 rest1) (Success value2 rest2) = eq value1 value2 && rest1 == rest2

  liftEq _ (Failure isFatal1 position1 expectations1) (Failure isFatal2 position2 expectations2) =
    isFatal1 == isFatal2 && position1 == position2 && expectations1 == expectations2

  liftEq _ _ _ = False
