{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Devin.Highlight (
  Tags (..),
  generateTags,
  applyTags,
  highlightInterval
) where

import Control.Monad.IO.Class

import Control.Monad.Extra

import Devin.Interval

import Data.Text (Text)

import qualified Data.Set as Set

import qualified Data.GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource


data Tags = Tags {
  highlightTag :: GtkSource.Tag,
  bracketTag :: GtkSource.Tag,
  keywordTag :: GtkSource.Tag,
  varIdTag :: GtkSource.Tag,
  funIdTag :: GtkSource.Tag,
  typeTag :: GtkSource.Tag,
  numberTag :: GtkSource.Tag,
  operatorTag :: GtkSource.Tag,
  commentTag :: GtkSource.Tag,
  errorTag :: GtkSource.Tag
} deriving Eq


generateTags :: (GtkSource.IsStyleScheme a, MonadIO m) => Maybe a -> m Tags
generateTags scheme = do
  languageManager <- GtkSource.languageManagerGetDefault
  defaultLanguage <- GtkSource.languageManagerGetLanguage languageManager "def"

  tag01 <- getTag defaultLanguage scheme "search-match"
  tag02 <- getTag defaultLanguage scheme "bracket-match"
  tag03 <- getTag defaultLanguage scheme "def:keyword"
  tag04 <- getTag defaultLanguage scheme "def:identifier"
  tag05 <- getTag defaultLanguage scheme "def:function"
  tag06 <- getTag defaultLanguage scheme "def:type"
  tag07 <- getTag defaultLanguage scheme "def:number"
  tag08 <- getTag defaultLanguage scheme "def:operator"
  tag09 <- getTag defaultLanguage scheme "def:comment"
  tag10 <- getTag defaultLanguage scheme "def:error"

  pure (Tags tag01 tag02 tag03 tag04 tag05 tag06 tag07 tag08 tag09 tag10)


applyTags :: (Gtk.IsTextTagTable a, MonadIO m) => Tags -> a -> m Bool
applyTags tags tagTable = do
  Gtk.textTagTableAdd tagTable (highlightTag tags)
  Gtk.textTagTableAdd tagTable (bracketTag tags)
  Gtk.textTagTableAdd tagTable (keywordTag tags)
  Gtk.textTagTableAdd tagTable (varIdTag tags)
  Gtk.textTagTableAdd tagTable (funIdTag tags)
  Gtk.textTagTableAdd tagTable (typeTag tags)
  Gtk.textTagTableAdd tagTable (numberTag tags)
  Gtk.textTagTableAdd tagTable (operatorTag tags)
  Gtk.textTagTableAdd tagTable (commentTag tags)
  Gtk.textTagTableAdd tagTable (errorTag tags)


highlightInterval ::
  (Gtk.IsTextTag a, Gtk.IsTextBuffer b, Interval c, MonadIO m) =>
  a -> b -> c -> m ()
highlightInterval tag buffer interval = do
  startIter <- Gtk.textBufferGetIterAtOffset buffer (start interval)
  endIter <- Gtk.textBufferGetIterAtOffset buffer (end interval)
  Gtk.textBufferApplyTag buffer tag startIter endIter


getTag ::
  (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) =>
  Maybe a -> Maybe b -> Text -> m GtkSource.Tag
getTag language scheme styleId = do
  maybeStyle <- case (scheme, language) of
    (Just scheme, Just language) -> getStyle language scheme styleId
    (Just scheme, Nothing) -> GtkSource.styleSchemeGetStyle scheme styleId
    (Nothing, _) -> pure Nothing

  tag <- GtkSource.tagNew Nothing
  whenJust maybeStyle (\style -> GtkSource.styleApply style tag)
  pure tag


getStyle ::
  (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) =>
  a -> b -> Text -> m (Maybe GtkSource.Style)
getStyle language scheme styleId = go Set.empty styleId
  where
    go seen styleId = GtkSource.styleSchemeGetStyle scheme styleId >>= \case
      Just style -> pure (Just style)

      Nothing | Set.member styleId seen -> pure Nothing

      Nothing -> GtkSource.languageGetStyleFallback language styleId >>= \case
        Just fallbackStyleId -> go (Set.insert styleId seen) fallbackStyleId
        Nothing -> pure Nothing
