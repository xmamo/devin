module Helpers (
  expectationsText,
  getInsertTextIter,
  getLineColumn,
  getStyle
) where

import Data.Text (Text)

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource


expectationsText :: [Text] -> Text
expectationsText [] = "Unexpected input"
expectationsText expectations = "Expected " <> go expectations
  where
    go [] = undefined
    go [expectation] = expectation
    go [expectation1, expectation2] = expectation1 <> " or " <> expectation2
    go (head : tail) = head <> ", " <> go tail


getInsertTextIter :: Gtk.IsTextBuffer a => a -> IO Gtk.TextIter
getInsertTextIter isTextBuffer = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer
  insertTextMark <- #getInsert textBuffer
  #getIterAtMark textBuffer insertTextMark


getLineColumn :: Integral a => Gtk.TextIter -> IO (a, a)
getLineColumn textIter = do
  line <- (1 +) . fromIntegral <$> #getLine textIter

  textIter' <- #copy textIter
  #setLineOffset textIter' 0

  let go column = do
        result <- #compare textIter' textIter

        if result >= 0 then
          pure column
        else do
          #forwardCursorPosition textIter'
          go (column + 1)

  column <- go 1
  pure (line, column)


getStyle :: (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b) => a -> b -> Text -> IO (Maybe GtkSource.Style)
getStyle isLanguage isStyleScheme styleId = go styleId []
  where
    language = isLanguage `asA` GtkSource.Language
    styleScheme = isStyleScheme `asA` GtkSource.StyleScheme

    go styleId seen = #getStyle styleScheme styleId >>= \case
      Just style -> pure (Just style)

      Nothing | styleId `notElem` seen -> #getStyleFallback language styleId >>= \case
        Just fallbackStyleId -> go fallbackStyleId (styleId : seen)
        Nothing -> pure Nothing

      Nothing -> pure Nothing
