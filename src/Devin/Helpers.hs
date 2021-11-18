module Devin.Helpers (
  expectationsText,
  getLineColumn,
  getStyle
) where

import Control.Monad.IO.Class

import Data.Text (Text)

import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource


expectationsText :: [Text] -> Text
expectationsText [] = "Unexpected input"
expectationsText expectations = "Expected " <> go expectations
  where
    go [] = undefined
    go [expectation] = expectation
    go [expectation1, expectation2] = expectation1 <> " or " <> expectation2
    go (expectation : expectations) = expectation <> ", " <> go expectations


getLineColumn :: (Num a, MonadIO m) => Gtk.TextIter -> m (a, a)
getLineColumn iter = do
  iter' <- Gtk.textIterCopy iter
  Gtk.textIterSetLineOffset iter' 0

  let go column = do
        result <- Gtk.textIterCompare iter' iter

        if result >= 0 then
          pure column
        else do
          Gtk.textIterForwardCursorPosition iter'
          go (column + 1)

  line <- Gtk.textIterGetLine iter
  column <- go 1
  pure (fromIntegral line + 1, column)


getStyle :: (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) => a -> b -> Text -> m (Maybe GtkSource.Style)
getStyle language scheme styleId = go [] styleId
  where
    go seen styleId = do
      style <- GtkSource.styleSchemeGetStyle scheme styleId

      case style of
        Just style -> pure (Just style)

        Nothing | styleId `notElem` seen -> do
          fallbackStyleId <- GtkSource.languageGetStyleFallback language styleId

          case fallbackStyleId of
            Just fallbackStyleId -> go (styleId : seen) fallbackStyleId
            Nothing -> pure Nothing

        Nothing -> pure Nothing
