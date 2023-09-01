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
    (tree, frames') <- go frames []
    forest' <- stateForest frames'

    case tree of
      Tree.Node label [] -> pure (Tree.Node label [Tree.Node ("—", "") []] : forest')
      _ -> pure (tree : forest')

  where
    go [] forest = pure (Tree.Node ("—", "") forest, [])

    go (frame : frames) forest = do
      forest' <- frameForest frame forest

      case label frame of
        Nothing -> go frames forest'
        Just label -> pure (Tree.Node (Text.pack label, "") forest', frames)


frameForest :: MonadIO m => Frame -> Tree.Forest (Text, Text) -> m (Tree.Forest (Text, Text))
frameForest Frame {vars} forest = foldlM f forest vars
  where
    f varsForest (name, r) = do
      v <- readRef r
      s <- displayVal v
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

  Array rs | Vector.null rs -> pure (showString "[]")

  Array rs -> do
    x <- readRef (rs ! 0)
    s1 <- displaysVal x
    s2 <- go (Vector.length rs) 1
    pure (showChar '[' . s1 . s2)

    where
      go n i | i >= n = pure (showChar ']')

      go n i = do
        x <- readRef (rs ! i)
        s1 <- displaysVal x
        s2 <- go n (i + 1)
        pure (showString ", " . s1 . s2)
