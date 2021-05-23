module Helpers (
  getInsertTextIter,
  getLineColumn,
  getStyle,
  expectationsText
) where

import Control.Monad.IO.Class

import Control.Monad.Trans.Maybe

import Data.Text (Text)

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource


getInsertTextIter :: (Gtk.IsTextBuffer a, MonadIO m) => a -> m Gtk.TextIter
getInsertTextIter isTextBuffer = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer

  insertTextMark <- #getInsert textBuffer
  #getIterAtMark textBuffer insertTextMark


getLineColumn :: (Num a, MonadIO m) => Gtk.TextIter -> m (a, a)
getLineColumn textIter = do
  line <- (1 +) . fromIntegral <$> #getLine textIter

  textIter' <- #copy textIter
  #setLineOffset textIter' 0

  let
    go column = do
      result <- #compare textIter' textIter

      if result >= 0 then
        pure column
      else do
        #forwardCursorPosition textIter'
        go (column + 1)

  column <- go 1
  pure (line, column)


getStyle ::
  (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) =>
  a -> b -> Text -> m (Maybe GtkSource.Style)

getStyle isLanguage isStyleScheme styleId = runMaybeT (go styleId [])
  where
    language = isLanguage `asA` GtkSource.Language
    styleScheme = isStyleScheme `asA` GtkSource.StyleScheme

    go styleId seen = MaybeT $ do
      style <- #getStyle styleScheme styleId

      case style of
        Just style -> pure (Just style)

        Nothing | styleId `notElem` seen -> runMaybeT $ do
          fallbackStyleId <- MaybeT (#getStyleFallback language styleId)
          go fallbackStyleId (styleId : seen)

        Nothing -> pure Nothing


expectationsText :: [Text] -> Text
expectationsText [] = "Unexpected input"
expectationsText expectations = "Expected " <> go expectations
  where
    go [] = undefined
    go [expectation] = expectation
    go [expectation1, expectation2] = expectation1 <> " or " <> expectation2
    go (head : tail) = head <> ", " <> go tail
