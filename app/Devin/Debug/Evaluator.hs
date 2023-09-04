{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Debug.Evaluator (
  stateForest,
  frameForest
) where

import Control.Monad.IO.Class
import Data.Foldable
import Numeric

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Vector ((!))
import qualified Data.Vector as Vector

import qualified Data.Tree as Tree

import Devin.Evaluator


stateForest :: MonadIO m => State -> m (Tree.Forest (Text, Text))
stateForest = \case
  [] -> pure []

  frames -> do
    (tree, frames') <- go [] frames
    forest' <- stateForest frames'

    case tree of
      Tree.Node label [] -> pure (Tree.Node label [Tree.Node ("—", "") []] : forest')
      _ -> pure (tree : forest')

    where
      go result [] = pure (Tree.Node ("—", "") result, [])

      go result (frame : frames) = do
        result' <- frameForest frame result

        case label frame of
          Nothing -> go result' frames
          Just label -> pure (Tree.Node (Text.pack label, "") result', frames)


frameForest :: MonadIO m => Frame -> Tree.Forest (Text, Text) -> m (Tree.Forest (Text, Text))
frameForest Frame {vars} forest = foldlM f forest vars
  where
    f varsForest (name, cell) = do
      val <- readCell cell
      s <- displayVal val
      pure (Tree.Node (Text.pack name, Text.pack s) [] : varsForest)


displayVal :: MonadIO m => Value -> m String
displayVal val = do
  s <- displaysVal val
  pure (s "")


displaysVal :: MonadIO m => Value -> m ShowS
displaysVal = \case
  Unit -> pure (showString "unit")
  Bool x -> pure (showString (if x then "true" else "false"))
  Int x -> pure (shows x)
  Float x -> pure (showFFloat Nothing x)

  Array cells | Vector.null cells -> pure (showString "[]")

  Array cells -> do
    val <- readCell (cells ! 0)
    s1 <- displaysVal val
    s2 <- go (Vector.length cells) 1
    pure (showChar '[' . s1 . s2)

    where
      go n i | i >= n = pure (showChar ']')

      go n i = do
        cell <- readCell (cells ! i)
        s1 <- displaysVal cell
        s2 <- go n (i + 1)
        pure (showString ", " . s1 . s2)
