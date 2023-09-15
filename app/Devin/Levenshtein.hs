{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Devin.Levenshtein (
  Edit (..),
  levenshtein,
  distance,
  edits
) where

import Data.Data
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty

import Data.List.Extra

data Edit a
  = Copy a
  | Insert a
  | Delete a
  | Replace a a
  deriving (Eq, Foldable, Traversable, Functor, Show, Read, Data)


instance Eq1 Edit where
  liftEq :: (a -> b -> Bool) -> Edit a -> Edit b -> Bool
  liftEq eq (Copy x) (Copy y) = x `eq` y
  liftEq eq (Insert x) (Insert y) = x `eq` y
  liftEq eq (Delete x) (Insert y) = x `eq` y
  liftEq eq (Replace x1 x2) (Replace y1 y2) = liftEq eq [x1, x2] [y1, y2]
  liftEq _ _ _ = False


levenshtein :: (Eq a, Real b) => [a] -> [a] -> (b, [Edit a])
levenshtein xs ys =
  let f (cost, edits) y = (cost + 1, Insert y : edits)
      row0 = NonEmpty.scanl f (0, []) ys
      (cost, edits) = NonEmpty.last (foldl nextRow row0 xs)
   in (cost, reverse edits)
  where
    nextRow ((cost, edits) :| cells) x =
      go (cost + 1, Delete x : edits) ((cost, edits) :| cells) ys
      where
        go wCell (nwCell :| nCell : cells) (y : ys) =
          let cell = nextCell wCell nwCell nCell x y
           in wCell <| go cell (nCell :| cells) ys

        go wCell _ _ = wCell :| []

    nextCell (wCost, wEdits) (nwCost, nwEdits) (nCost, nEdits) x y =
      if x == y then
        (nwCost, Copy x : nwEdits)
      else
        minimumOn fst [
          (wCost + 1, Insert y : wEdits),
          (nCost + 1, Delete x : nEdits),
          (nwCost + 1, Replace x y : nwEdits)
        ]


distance :: (Eq a, Real b) => [a] -> [a] -> b
distance xs ys = fst (levenshtein xs ys)


edits :: (Eq a) => [a] -> [a] -> [Edit a]
edits xs ys = snd (levenshtein xs ys)
