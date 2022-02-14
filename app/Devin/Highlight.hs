{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
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
import Data.Traversable

import Data.Text (Text)

import Data.Set qualified as Set

import Control.Monad.Extra

import Data.GI.Gtk qualified as Gtk
import GI.GtkSource qualified as GtkSource

import Devin.Interval


data Tags = Tags {
  highlightTag :: GtkSource.Tag,
  bracketTag :: GtkSource.Tag,
  keywordTag :: GtkSource.Tag,
  variableIdTag :: GtkSource.Tag,
  functionIdTag :: GtkSource.Tag,
  typeTag :: GtkSource.Tag,
  numberTag :: GtkSource.Tag,
  operatorTag :: GtkSource.Tag,
  commentTag :: GtkSource.Tag,
  errorTag :: GtkSource.Tag
} deriving Eq


generateTags :: (GtkSource.IsStyleScheme a, MonadIO m) => Maybe a -> m Tags
generateTags scheme = do
  let s1 = "search-match"
  let s2 = "bracket-match"
  let s3 = "def:keyword"
  let s4 = "def:identifier"
  let s5 = "def:function"
  let s6 = "def:type"
  let s7 = "def:number"
  let s8 = "def:operator"
  let s9 = "def:comment"
  let s10 = "def:error"
  let styleIds = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

  languageManager <- GtkSource.languageManagerGetDefault
  defaultLanguage <- GtkSource.languageManagerGetLanguage languageManager "def"

  tags <- for styleIds $ \styleId -> do
    result <- case (scheme, defaultLanguage) of
      (Just scheme, Just language) -> getStyle language scheme styleId
      (Just scheme, Nothing) -> GtkSource.styleSchemeGetStyle scheme styleId
      (Nothing, _) -> pure Nothing

    tag <- GtkSource.tagNew Nothing
    whenJust result (\style -> GtkSource.styleApply style tag)
    pure tag

  let [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10] = tags
  pure (Tags t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)


applyTags :: (Gtk.IsTextTagTable a, MonadIO m) => Tags -> a -> m Bool
applyTags tags tagTable = do
  Gtk.textTagTableAdd tagTable (highlightTag tags)
  Gtk.textTagTableAdd tagTable (bracketTag tags)
  Gtk.textTagTableAdd tagTable (keywordTag tags)
  Gtk.textTagTableAdd tagTable (variableIdTag tags)
  Gtk.textTagTableAdd tagTable (functionIdTag tags)
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


getStyle ::
  (GtkSource.IsLanguage a, GtkSource.IsStyleScheme b, MonadIO m) =>
  a -> b -> Text -> m (Maybe GtkSource.Style)
getStyle language scheme styleId = go Set.empty styleId
  where
    go seen styleId = GtkSource.styleSchemeGetStyle scheme styleId >>= \case
      Just style -> pure (Just style)

      Nothing | Set.member styleId seen -> pure Nothing

      Nothing ->
        GtkSource.languageGetStyleFallback language styleId >>= \case
          Just fallbackStyleId -> go (Set.insert styleId seen) fallbackStyleId
          Nothing -> pure Nothing
