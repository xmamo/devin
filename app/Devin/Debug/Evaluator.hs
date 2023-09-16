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

import Data.Tree

import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.Vector as Vector
import Data.Vector ((!))

import Devin.Evaluator


stateForest :: MonadIO m => State -> m (Forest (Text, Text))
stateForest = \case
  [] -> pure []

  frames -> do
    (tree, frames') <- go [] frames
    forest' <- stateForest frames'

    case tree of
      Node label [] -> pure (Node label [Node ("—", "") []] : forest')
      _ -> pure (tree : forest')

    where
      go result [] = pure (Node ("—", "") result, [])

      go result (frame : frames) = do
        result' <- frameForest frame result

        case label frame of
          Nothing -> go result' frames
          Just label -> pure (Node (Text.pack label, "") result', frames)


frameForest :: MonadIO m => Frame -> Forest (Text, Text) -> m (Forest (Text, Text))
frameForest Frame {vars} forest = foldlM f forest vars
  where
    f varsForest (name, cell) = do
      val <- readCell cell
      s <- displayVal val
      pure (Node (Text.pack name, Text.pack s) [] : varsForest)


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
