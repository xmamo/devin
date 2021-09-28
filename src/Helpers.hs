module Helpers (
  allM,
  anyM,
  andM,
  orM,
  utf8Length,
  expectationsText,
  getInsertTextIter,
  getLineColumn,
  getStyle
) where

import Control.Monad.IO.Class
import Data.Foldable

import qualified Data.ByteString as ByteString

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

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


utf8Length :: Integral a => Text -> a
utf8Length = fromIntegral . ByteString.length . Text.encodeUtf8


expectationsText :: [Text] -> Text
expectationsText [] = "Unexpected input"
expectationsText expectations = "Expected " <> go expectations
  where
    go [] = undefined
    go [expectation] = expectation
    go [expectation1, expectation2] = expectation1 <> " or " <> expectation2
    go (expectation : expectations) = expectation <> ", " <> go expectations


getInsertTextIter :: (Gtk.IsTextBuffer a, MonadIO m) => a -> m Gtk.TextIter
getInsertTextIter textBuffer = do
  insertTextMark <- Gtk.textBufferGetInsert textBuffer
  Gtk.textBufferGetIterAtMark textBuffer insertTextMark


getLineColumn :: (Integral a, MonadIO m) => Gtk.TextIter -> m (a, a)
getLineColumn textIter = do
  textIter' <- Gtk.textIterCopy textIter
  Gtk.textIterSetLineOffset textIter' 0

  let go column = do
        result <- Gtk.textIterCompare textIter' textIter

        if result >= 0 then
          pure column
        else do
          Gtk.textIterForwardCursorPosition textIter'
          go (column + 1)

  line <- (1 +) . fromIntegral <$> Gtk.textIterGetLine textIter
  column <- go 1
  pure (line, column)


getStyle :: (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) => a -> b -> Text -> m (Maybe GtkSource.Style)
getStyle language styleScheme = go []
  where
    go seen styleId = GtkSource.styleSchemeGetStyle styleScheme styleId >>= \case
      Just style -> pure (Just style)

      Nothing | styleId `notElem` seen ->
        GtkSource.languageGetStyleFallback language styleId >>= \case
          Just fallbackStyleId -> go (styleId : seen) fallbackStyleId
          Nothing -> pure Nothing

      Nothing -> pure Nothing
