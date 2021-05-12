module Helpers (
  getStyle,
  expectationsText
) where

import Control.Monad.IO.Class

import Control.Monad.Trans.Maybe

import Data.Text (Text)

import Data.GI.Base
import qualified GI.GtkSource as GtkSource


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
