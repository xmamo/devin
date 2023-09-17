{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module Devin.Levenshtein (
  Edit (..),
  TreeEdit (..),
  levenshteinWith,
  levenshteinOn,
  levenshtein,
  distanceWith,
  distanceOn,
  distance,
  diffWith,
  diffOn,
  diff,
  treeDiffWith,
  treeDiffOn,
  treeDiff,
  forestDiffWith,
  forestDiffOn,
  forestDiff
) where

import Data.Data
import Data.Function
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))

import Data.Tree

import Data.List.Extra


data Edit a
  = Copy a a
  | Insert a
  | Delete a
  | Replace a a
  deriving (Eq, Foldable, Traversable, Functor, Show, Read, Data)


data TreeEdit a
  = TreeCopy (Tree a) (Tree a)
  | TreeInsert (Tree a)
  | TreeDelete (Tree a)
  | TreeReplace (Tree a) (Tree a)
  | TreeUpdate (Tree a) [TreeEdit a]
  deriving (Eq, Foldable, Traversable, Functor, Show, Read, Data)


levenshteinWith :: Real b => (a -> a -> Bool) -> [a] -> [a] -> (b, [Edit a])
levenshteinWith eq xs ys =
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
      if x `eq` y then
        (nwCost, Copy x y : nwEdits)
      else
        minimumOn fst [
          (wCost + 1, Insert y : wEdits),
          (nCost + 1, Delete x : nEdits),
          (nwCost + 1, Replace x y : nwEdits)
        ]


levenshteinOn :: (Eq b, Real c) => (a -> b) -> [a] -> [a] -> (c, [Edit a])
levenshteinOn f = levenshteinWith ((==) `on` f)


levenshtein :: (Eq a, Real b) => [a] -> [a] -> (b, [Edit a])
levenshtein = levenshteinWith (==)


distanceWith :: Real b => (a -> a -> Bool) -> [a] -> [a] -> b
distanceWith eq xs ys = fst (levenshteinWith eq xs ys)


distanceOn :: (Eq b, Real c) => (a -> b) -> [a] -> [a] -> c
distanceOn f = distanceWith ((==) `on` f)


distance :: (Eq a, Real b) => [a] -> [a] -> b
distance = distanceWith (==)


diffWith :: (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
diffWith eq xs ys = snd (levenshteinWith eq xs ys)


diffOn :: Eq b => (a -> b) -> [a] -> [a] -> [Edit a]
diffOn f = diffWith ((==) `on` f)


diff :: Eq a => [a] -> [a] -> [Edit a]
diff = diffWith (==)


treeDiffWith :: (a -> a -> Bool) -> Tree a -> Tree a -> TreeEdit a
treeDiffWith eq tree1 tree2
  | rootLabel tree1 `eq` rootLabel tree2 = treeDiffHelper eq tree1 tree2
  | otherwise = TreeReplace tree1 tree2


treeDiffOn :: Eq b => (a -> b) -> Tree a -> Tree a -> TreeEdit a
treeDiffOn f = treeDiffWith ((==) `on` f)


treeDiff :: Eq a => Tree a -> Tree a -> TreeEdit a
treeDiff = treeDiffWith (==)


forestDiffWith :: (a -> a -> Bool) -> Forest a -> Forest a -> [TreeEdit a]
forestDiffWith eq forest1 forest2 =
  map f (diffWith (eq `on` rootLabel) forest1 forest2)
  where
    f (Copy tree1 tree2) = treeDiffHelper eq tree1 tree2
    f (Insert tree2) = TreeInsert tree2
    f (Delete tree1) = TreeDelete tree1
    f (Replace tree1 tree2) = TreeReplace tree1 tree2


forestDiffOn :: Eq b => (a -> b) -> Forest a -> Forest a -> [TreeEdit a]
forestDiffOn f = forestDiffWith ((==) `on` f)


forestDiff :: Eq a => Forest a -> Forest a -> [TreeEdit a]
forestDiff = forestDiffWith (==)


treeDiffHelper :: (a -> a -> Bool) -> Tree a -> Tree a -> TreeEdit a
treeDiffHelper eq tree1 tree2 =
  let edits = forestDiffWith eq (subForest tree1) (subForest tree2)
   in if all isCopy edits then TreeCopy tree1 tree2 else TreeUpdate tree1 edits
  where
    isCopy (TreeCopy _ _) = True
    isCopy _ = False
