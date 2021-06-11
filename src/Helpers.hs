module Helpers (
  allM,
  anyM,
  andM,
  orM,
  expectationsText,
  getInsertTextIter,
  getLineColumn,
  getStyle
) where

import Data.Foldable

import Data.Text (Text)

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource


allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldlM (\a x -> if a then f x else pure False) True


anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldlM (\a x -> if a then pure True else f x) False


andM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
andM = allM id


orM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
orM = anyM id


expectationsText :: [Text] -> Text
expectationsText [] = "Unexpected input"
expectationsText expectations = "Expected " <> go expectations
  where
    go [] = undefined
    go [expectation] = expectation
    go [expectation1, expectation2] = expectation1 <> " or " <> expectation2
    go (expectation : expectations) = expectation <> ", " <> go expectations


getInsertTextIter :: Gtk.IsTextBuffer a => a -> IO Gtk.TextIter
getInsertTextIter textBuffer' = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer
  insertTextMark <- #getInsert textBuffer
  #getIterAtMark textBuffer insertTextMark


getLineColumn :: Integral a => Gtk.TextIter -> IO (a, a)
getLineColumn textIter = do
  textIter' <- #copy textIter
  #setLineOffset textIter' 0

  let go column = do
        result <- #compare textIter' textIter

        if result >= 0 then
          pure column
        else do
          #forwardCursorPosition textIter'
          go (column + 1)

  line <- (1 +) . fromIntegral <$> #getLine textIter
  column <- go 1
  pure (line, column)


getStyle :: (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b) => a -> b -> Text -> IO (Maybe GtkSource.Style)
getStyle language' styleScheme' = go []
  where
    language = language' `asA` GtkSource.Language
    styleScheme = styleScheme' `asA` GtkSource.StyleScheme

    go seen styleId = #getStyle styleScheme styleId >>= \case
      Just style -> pure (Just style)

      Nothing | styleId `notElem` seen -> #getStyleFallback language styleId >>= \case
        Just fallbackStyleId -> go (styleId : seen) fallbackStyleId
        Nothing -> pure Nothing

      Nothing -> pure Nothing
