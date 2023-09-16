{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module Devin.Levenshtein (
  Edit (..),
  TreeEdit (..),
  levenshtein,
  distance,
  diff,
  treeDiff,
  forestDiff
) where

import Data.Data
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Tree

import Data.List.Extra


data Edit a
  = Copy a
  | Insert a
  | Delete a
  | Replace a a
  deriving (Eq, Foldable, Traversable, Functor, Show, Read, Data)


data TreeEdit a
  = TreeCopy (Tree a)
  | TreeInsert (Tree a)
  | TreeDelete (Tree a)
  | TreeReplace (Tree a) (Tree a)
  | TreeUpdate (Tree a) [TreeEdit a]
  deriving (Eq, Foldable, Traversable, Functor, Show, Read, Data)


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


diff :: Eq a => [a] -> [a] -> [Edit a]
diff xs ys = snd (levenshtein xs ys)


treeDiff :: Eq a => Tree a -> Tree a -> TreeEdit a
treeDiff tree1 tree2
  | rootLabel tree1 /= rootLabel tree2 = TreeReplace tree1 tree2
  | otherwise = TreeUpdate tree1 (forestDiff (subForest tree1) (subForest tree2))


forestDiff :: Eq a => Forest a -> Forest a -> [TreeEdit a]
forestDiff forest1 forest2 = map f (diff forest1 forest2)
  where
    f (Copy tree) = TreeCopy tree
    f (Insert tree) = TreeInsert tree
    f (Delete tree) = TreeDelete tree
    f (Replace tree1 tree2) = treeDiff tree1 tree2
