{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Devin.Levenshtein (
  Edit (..),
  TreeEdit (..),
  levenshteinBy,
  levenshteinOn,
  levenshtein,
  distanceBy,
  distanceOn,
  distance,
  diffWith,
  diffOn,
  diff,
  treeDiffBy,
  treeDiffOn,
  treeDiff,
  forestDiffBy,
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


levenshteinBy :: Real b => (a -> a -> Bool) -> [a] -> [a] -> (b, [Edit a])
levenshteinBy eq xs ys =
  let f (cost, editDL) y = (cost + 1, editDL . (Insert y :))
      row0 = NonEmpty.scanl f (0, id) ys
      (cost, editDL) = NonEmpty.last (foldl nextRow row0 xs)
   in (cost, editDL [])

  where
    nextRow ((cost, editDL) :| cells) x =
      go (cost + 1, editDL . (Delete x :)) ((cost, editDL) :| cells) ys

      where
        go wCell (nwCell :| nCell : cells) (y : ys) =
          let cell = nextCell wCell nwCell nCell x y
           in wCell <| go cell (nCell :| cells) ys

        go wCell _ _ = NonEmpty.singleton wCell

    nextCell (wCost, wEditDL) (nwCost, nwEditDL) (nCost, nEditDL) x y =
      if x `eq` y then
        (nwCost, nwEditDL . (Copy x y :))
      else
        minimumOn fst [
          (wCost + 1, wEditDL . (Insert y :)),
          (nCost + 1, nEditDL . (Delete x :)),
          (nwCost + 1, nwEditDL . (Replace x y :))
        ]


levenshteinOn :: (Eq b, Real c) => (a -> b) -> [a] -> [a] -> (c, [Edit a])
levenshteinOn f = levenshteinBy ((==) `on` f)


levenshtein :: (Eq a, Real b) => [a] -> [a] -> (b, [Edit a])
levenshtein = levenshteinOn id


distanceBy :: Real b => (a -> a -> Bool) -> [a] -> [a] -> b
distanceBy eq xs ys = fst (levenshteinBy eq xs ys)


distanceOn :: (Eq b, Real c) => (a -> b) -> [a] -> [a] -> c
distanceOn f = distanceBy ((==) `on` f)


distance :: (Eq a, Real b) => [a] -> [a] -> b
distance = distanceOn id


diffWith :: (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
diffWith eq xs ys = snd (levenshteinBy eq xs ys)


diffOn :: Eq b => (a -> b) -> [a] -> [a] -> [Edit a]
diffOn f = diffWith ((==) `on` f)


diff :: Eq a => [a] -> [a] -> [Edit a]
diff = diffOn id


treeDiffBy :: (a -> a -> Bool) -> Tree a -> Tree a -> TreeEdit a
treeDiffBy eq tree1 tree2
  | rootLabel tree1 `eq` rootLabel tree2 = treeDiffHelper eq tree1 tree2
  | otherwise = TreeReplace tree1 tree2


treeDiffOn :: Eq b => (a -> b) -> Tree a -> Tree a -> TreeEdit a
treeDiffOn f = treeDiffBy ((==) `on` f)


treeDiff :: Eq a => Tree a -> Tree a -> TreeEdit a
treeDiff = treeDiffOn id


forestDiffBy :: (a -> a -> Bool) -> Forest a -> Forest a -> [TreeEdit a]
forestDiffBy eq forest1 forest2 =
  flip map (diffWith (eq `on` rootLabel) forest1 forest2) $ \case
    Copy tree1 tree2 -> treeDiffHelper eq tree1 tree2
    Insert tree2 -> TreeInsert tree2
    Delete tree1 -> TreeDelete tree1
    Replace tree1 tree2 -> TreeReplace tree1 tree2


forestDiffOn :: Eq b => (a -> b) -> Forest a -> Forest a -> [TreeEdit a]
forestDiffOn f = forestDiffBy ((==) `on` f)


forestDiff :: Eq a => Forest a -> Forest a -> [TreeEdit a]
forestDiff = forestDiffOn id


treeDiffHelper :: (a -> a -> Bool) -> Tree a -> Tree a -> TreeEdit a
treeDiffHelper eq tree1 tree2 =
  let edits = forestDiffBy eq (subForest tree1) (subForest tree2)
   in if all isCopy edits then TreeCopy tree1 tree2 else TreeUpdate tree1 edits

  where
    isCopy (TreeCopy _ _) = True
    isCopy _ = False
