module Lib (main) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Functor
import Data.Foldable
import Data.Maybe
import System.Environment
import System.Exit

import Control.Monad.Trans.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Data.GI.Base
import qualified Data.GI.Gtk as Gtk
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.GtkSource as GtkSource

import Input (Input (Input))

import qualified Result
import qualified Parser
import qualified Parsers

import Span (Span (Span))
import qualified Syntax


main :: IO ()
main = do
  args <- getArgs
  application <- new Gtk.Application []
  on application #activate (onActivate application)
  status <- #run application (Just args)
  exitWith (if status == 0 then ExitSuccess else ExitFailure (fromIntegral status))


onActivate :: Gtk.IsApplication a => a -> IO ()
onActivate isApplication = do
  let application = isApplication `asA` Gtk.Application

  logTextBuffer <- new Gtk.TextBuffer []

  buffer <- new GtkSource.Buffer [#highlightMatchingBrackets := False]
  tagTable <- #getTagTable buffer
  styleScheme <- fromJust <$> #getStyleScheme buffer

  languageManager <- new GtkSource.LanguageManager []
  defaultLanguage <- fromJust <$> #getLanguage languageManager "def"

  let
    styles =
      [
        ("keyword", "def:keyword"),
        ("identifier", "def:identifier"),
        ("integer", "def:decimal"),
        ("unary-operator", "def:operator"),
        ("binary-operator", "def:operator"),
        ("parenthesis", "bracket-match"),
        ("error", "def:error")
      ]

  for_ styles $ \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    style <- fromJust <$> getStyle defaultLanguage styleScheme styleId
    #apply style tag
    #add tagTable tag

  threadIdVar <- newMVar Nothing
  expressionVar <- newMVar Nothing

  on buffer #changed $ do
    text <- fromJust <$> get buffer #text

    traverse_ killThread =<< takeMVar threadIdVar
    swapMVar expressionVar Nothing

    threadId <- forkIO $ do
      let expression = Parser.parse Parsers.expression (Input 0 text)

      case expression of
        Result.Success expression _ -> Gtk.postGUIASync $ do
          swapMVar expressionVar (Just expression)

          (startTextIter, endTextIter) <- #getBounds buffer
          for_ styles $ \(tagName, _) -> #removeTagByName buffer tagName startTextIter endTextIter

          highlightExpression buffer expression
          highlightExpressionParentheses buffer expression

          set logTextBuffer [#text := ""]

        Result.Failure _ position expectations -> Gtk.postGUIASync $ do
          (startTextIter, endTextIter) <- #getBounds buffer
          #removeTagByName buffer "error" startTextIter endTextIter

          startTextIter <- #getIterAtOffset buffer (fromIntegral position)
          for_ styles $ \(tagName, _) -> #removeTagByName buffer tagName startTextIter endTextIter
          #applyTagByName buffer "error" startTextIter endTextIter

          line <- (+ 1) <$> #getLine startTextIter
          column <- (+ 1) <$> #getLineOffset startTextIter
          let prefix = "[" <> Text.pack (show line) <> ":" <> Text.pack (show column) <> "] "
          set logTextBuffer [#text := prefix <> expectationsText expectations]

    putMVar threadIdVar (Just threadId)

  on buffer (#notify ::: "cursor-position") . const . void . runMaybeT $ do
    (startTextIter, endTextIter) <- #getBounds buffer
    #removeTagByName buffer "parenthesis" startTextIter endTextIter

    expression <- MaybeT (readMVar expressionVar)
    highlightExpressionParentheses buffer expression

  logTextView <- new Gtk.TextView
    [
      #editable := False,
      #monospace := True,
      #wrapMode := Gtk.WrapModeWord,
      #buffer := logTextBuffer
    ]

  sourceView <- new GtkSource.View
    [
      #showLineNumbers := True,
      #highlightCurrentLine := True,
      #tabWidth := 4,
      #monospace := True,
      #buffer := buffer
    ]

  scrolledWindow <- new Gtk.ScrolledWindow []
  #add scrolledWindow sourceView

  paned <- new Gtk.Paned [#orientation := Gtk.OrientationVertical]
  #pack1 paned scrolledWindow True False
  #pack2 paned logTextView False False

  window <- new Gtk.ApplicationWindow
    [
      #application := application,
      #title := "",
      #defaultWidth := 800,
      #defaultHeight := 600
    ]

  #add window paned
  #showAll window


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> m ()
highlightExpressionParentheses isBuffer expression = void (go expression =<< getInsertTextIter)
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    getInsertTextIter = do
      insertTextMark <- #getInsert buffer
      #getIterAtMark buffer insertTextMark

    go (Syntax.IntegerExpression _ _) _ = pure False

    go (Syntax.IdentifierExpression _ _) _ = pure False

    go (Syntax.UnaryExpression _ operand _) insertTextIter = go operand insertTextIter

    go (Syntax.BinaryExpression left _ right _) insertTextIter = do
      done <- go left insertTextIter

      if done then
        pure True
      else
        go right insertTextIter

    go (Syntax.ParenthesizedExpression expression (Span start end)) insertTextIter = do
      done <- go expression insertTextIter

      if done then
        pure True
      else do
        leftStartTextIter <- #getIterAtOffset buffer (fromIntegral start)
        leftEndTextIter <- #copy leftStartTextIter
        #forwardChar leftEndTextIter

        rightEndTextIter <- #getIterAtOffset buffer (fromIntegral end)
        rightStartTextIter <- #copy rightEndTextIter
        #backwardChar rightStartTextIter

        applyParenthesisTag <- or <$> traverse (#equal insertTextIter)
          [leftStartTextIter, leftEndTextIter, rightEndTextIter, rightStartTextIter]

        if applyParenthesisTag then do
          #applyTagByName buffer "parenthesis" leftStartTextIter leftEndTextIter
          #applyTagByName buffer "parenthesis" rightStartTextIter rightEndTextIter
          pure True
        else
          pure False


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a ->  Syntax.Expression -> m ()
highlightExpression isBuffer = go
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    go (Syntax.IntegerExpression _ (Span start end)) = do
      startTextIter <- #getIterAtOffset buffer (fromIntegral start)
      endTextIter <- #getIterAtOffset buffer (fromIntegral end)
      #applyTagByName buffer "integer" startTextIter endTextIter

    go (Syntax.IdentifierExpression _ (Span start end)) = do
      startTextIter <- #getIterAtOffset buffer (fromIntegral start)
      endTextIter <- #getIterAtOffset buffer (fromIntegral end)
      #applyTagByName buffer "identifier" startTextIter endTextIter

    go (Syntax.UnaryExpression operator operand _) = do
      highlightUnaryOperator buffer operator
      go operand

    go (Syntax.BinaryExpression left operator right _) = do
      go left
      highlightBinaryOperator buffer operator
      go right

    go (Syntax.ParenthesizedExpression expression _) = go expression


highlightUnaryOperator :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.UnaryOperator -> m ()
highlightUnaryOperator isBuffer operator = do
  let buffer = isBuffer `asA` Gtk.TextBuffer

  startTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.start operator))
  endTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.end operator))
  #applyTagByName buffer "unary-operator" startTextIter endTextIter


highlightBinaryOperator :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.BinaryOperator -> m ()
highlightBinaryOperator isBuffer operator = do
  let buffer = isBuffer `asA` Gtk.TextBuffer

  startTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.start operator))
  endTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.end operator))
  #applyTagByName buffer "binary-operator" startTextIter endTextIter


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
