{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Debug.Evaluator (
  stateForest,
  displayVal,
  displaysVal
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.String
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
        result' <- foldlM f result (vars frame)

        case label frame of
          Nothing -> go result' frames
          Just label -> pure (Node (Text.pack label, "") result', frames)

      f varsForest (name, cell) = do
        val <- readCell cell
        valText <- displayVal val
        pure (Node (Text.pack name, valText) [] : varsForest)


displayVal :: (MonadIO m, IsString a) => Value -> m a
displayVal val = do
  s <- displaysVal val
  pure (fromString (s ""))


displaysVal :: MonadIO m => Value -> m ShowS
displaysVal = \case
  Unit -> pure (showString "unit")
  Bool x -> pure (showString (if x then "true" else "false"))
  Int x -> pure (shows x)

  Float x | isNaN x -> pure (showString "NaN")
  Float x | isInfinite x -> pure (showString (if x < 0 then "-∞" else "∞"))
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
