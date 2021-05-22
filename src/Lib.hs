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
import qualified GI.Gtk as Gtk
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.GtkSource as GtkSource

import Input (Input (Input))

import qualified Result
import qualified Parser
import qualified Parsers

import Syntax (Syntax)
import qualified Syntax

import qualified Helpers


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
        ("operator", "def:operator"),
        ("error", "def:error"),
        ("parenthesis", "bracket-match")
      ]

  for_ styles $ \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    style <- fromJust <$> Helpers.getStyle defaultLanguage styleScheme styleId
    #apply style tag
    #add tagTable tag

  threadIdVar <- newMVar =<< forkIO (pure ())
  statementVar <- newMVar Nothing

  on buffer #changed $ do
    text <- fromJust <$> get buffer #text

    killThread =<< takeMVar threadIdVar
    swapMVar statementVar Nothing

    threadId <- forkIO $ do
      let statement = Parser.parse Parsers.statement (Input 0 text)

      case statement of
        Result.Success statement _ -> Gtk.postGUIASync $ do
          swapMVar statementVar (Just statement)

          (startTextIter, endTextIter) <- #getBounds buffer
          for_ styles $ \(tagName, _) -> #removeTagByName buffer tagName startTextIter endTextIter

          highlightStatement buffer statement
          highlightStatementParentheses buffer statement =<< Helpers.getInsertTextIter buffer

          set logTextBuffer [#text := ""]

        Result.Failure _ position expectations -> Gtk.postGUIASync $ do
          (startTextIter, endTextIter) <- #getBounds buffer
          #removeTagByName buffer "error" startTextIter endTextIter

          startTextIter <- #getIterAtOffset buffer (fromIntegral position)
          for_ styles $ \(tagName, _) -> #removeTagByName buffer tagName startTextIter endTextIter
          #applyTagByName buffer "error" startTextIter endTextIter

          (line, column) <- Helpers.getLineColumn startTextIter
          let prefix = "[" <> Text.pack (show line) <> ":" <> Text.pack (show column) <> "] "
          set logTextBuffer [#text := prefix <> Helpers.expectationsText expectations]

    putMVar threadIdVar threadId

  on buffer (PropertyNotify #cursorPosition) . const . void . runMaybeT $ do
    (startTextIter, endTextIter) <- #getBounds buffer
    #removeTagByName buffer "parenthesis" startTextIter endTextIter

    statement <- MaybeT (readMVar statementVar)
    highlightStatementParentheses buffer statement =<< Helpers.getInsertTextIter buffer

  logTextView <- new Gtk.TextView
    [
      #editable := False,
      #monospace := True,
      #wrapMode := Gtk.WrapModeWord,
      #buffer := logTextBuffer
    ]

  sourceView <- new GtkSource.View
    [
      #autoIndent := True,
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


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> m ()
highlightStatement isBuffer = go
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    go (Syntax.DeclareStatement varKeyword variable _) = do
      highlightWith buffer "keyword" varKeyword
      highlightWith buffer "identifier" variable

    go (Syntax.DeclareAndAssignStatement varKeyword variable _ value _) = do
      highlightWith buffer "keyword" varKeyword
      highlightWith buffer "identifier" variable
      highlightExpression buffer value

    go (Syntax.ExpressionStatement value _) = highlightExpression buffer value

    go (Syntax.IfStatement ifKeyword predicate trueBranch) = do
      highlightWith buffer "keyword" ifKeyword
      highlightExpression buffer predicate
      highlightStatement buffer trueBranch

    go (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch) = do
      highlightWith buffer "keyword" ifKeyword
      highlightExpression buffer predicate
      highlightStatement buffer trueBranch
      highlightWith buffer "keyword" elseKeyword
      highlightStatement buffer falseBranch

    go (Syntax.WhileStatement whileKeyword predicate body) = do
      highlightWith buffer "keyword" whileKeyword
      highlightExpression buffer predicate
      highlightStatement buffer body

    go (Syntax.DoWhileStatement doKeyword body whileKeyword predicate _) = do
      highlightWith buffer "keyword" doKeyword
      highlightStatement buffer body
      highlightWith buffer "keyword" whileKeyword
      highlightExpression buffer predicate

    go (Syntax.ReturnStatement returnKeyword value _) = do
      highlightWith buffer "keyword" returnKeyword
      highlightExpression buffer value

    go (Syntax.BlockStatement _ statements _) = for_ statements (highlightStatement buffer)


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> m ()
highlightExpression isBuffer = go
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    go integer @ Syntax.IntegerExpression {} = highlightWith buffer "integer" integer

    go (Syntax.IdentifierExpression identifier) = highlightWith buffer "identifier" identifier

    go (Syntax.UnaryExpression operator operand) = do
      highlightWith buffer "operator" operator
      go operand

    go (Syntax.BinaryExpression left operator right) = do
      go left
      highlightWith buffer "operator" operator
      go right

    go (Syntax.AssignExpression identifier operator value) = do
      highlightWith buffer "identifier" identifier
      highlightWith buffer "operator" operator
      go value

    go (Syntax.ParenthesizedExpression _ expression _) = go expression


highlightStatementParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> Gtk.TextIter -> m Bool
highlightStatementParentheses isBuffer statement insertTextIter = go statement
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    go Syntax.DeclareStatement {} = pure False

    go (Syntax.DeclareAndAssignStatement _ _ _ value _) = highlightExpressionParentheses buffer value insertTextIter

    go (Syntax.ExpressionStatement value _) = highlightExpressionParentheses buffer value insertTextIter

    go (Syntax.IfStatement _ predicate trueBranch) = do
      done <- highlightExpressionParentheses buffer predicate insertTextIter

      if done then
        pure True
      else
        go trueBranch

    go (Syntax.IfElseStatement _ predicate trueBranch _ falseBranch) = do
      done <- highlightExpressionParentheses buffer predicate insertTextIter

      if done then
        pure True
      else do
        done <- go trueBranch

        if done then
          pure True
        else
          go falseBranch

    go (Syntax.WhileStatement _ predicate body) = do
      done <- highlightExpressionParentheses buffer predicate insertTextIter

      if done then
        pure True
      else
        go body

    go (Syntax.DoWhileStatement _ body _ predicate _) = do
      done <- go body

      if done then
        pure True
      else
        highlightExpressionParentheses buffer predicate insertTextIter

    go (Syntax.ReturnStatement _ value _) = highlightExpressionParentheses buffer value insertTextIter

    go (Syntax.BlockStatement open statements close) = do
      done <- foldlM (\a s -> if a then pure True else go s) False statements

      if done then
        pure True
      else
        highlightParentheses buffer open close insertTextIter


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> Gtk.TextIter -> m Bool
highlightExpressionParentheses isBuffer expression insertTextIter = go expression
  where
    buffer = isBuffer `asA` Gtk.TextBuffer

    go (Syntax.IntegerExpression _ _) = pure False

    go (Syntax.IdentifierExpression _) = pure False

    go (Syntax.UnaryExpression _ operand) = go operand

    go (Syntax.BinaryExpression left _ right) = do
      done <- go left

      if done then
        pure True
      else
        go right

    go (Syntax.AssignExpression _ _ value) = go value

    go (Syntax.ParenthesizedExpression open expression close) = do
      done <- go expression

      if done then
        pure True
      else
        highlightParentheses buffer open close insertTextIter


highlightParentheses :: (Gtk.IsTextBuffer a, Syntax b, Syntax c, MonadIO m) => a -> b -> c -> Gtk.TextIter -> m Bool
highlightParentheses isBuffer open close insertTextIter = do
  let buffer = isBuffer `asA` Gtk.TextBuffer

  openStartTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.start open))
  openEndTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.end open))

  closeStartTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.start close))
  closeEndTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.end close))

  applyParenthesisTag <- foldlM (\a i -> if a then pure True else #equal insertTextIter i) False
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    #applyTagByName buffer "parenthesis" openStartTextIter openEndTextIter
    #applyTagByName buffer "parenthesis" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlightWith :: (Gtk.IsTextBuffer a, Syntax b, MonadIO m) => a -> Text -> b -> m ()
highlightWith isBuffer tagName syntax = do
  let buffer = isBuffer `asA` Gtk.TextBuffer

  startTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.start syntax))
  endTextIter <- #getIterAtOffset buffer (fromIntegral (Syntax.end syntax))
  #applyTagByName buffer tagName startTextIter endTextIter
