module Main (main) where

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
import Data.GI.Base.GType
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

      styleIds =
        [
          ("keyword", "def:keyword"),
          ("identifier", "def:identifier"),
          ("type", "def:type"),
          ("integer", "def:decimal"),
          ("operator", "def:operator"),
          ("error", "def:error"),
          ("parenthesis", "bracket-match")
        ]

  -- Create defaultLanguage and codeTreeStore, which are needed later:

  languageManager <- new GtkSource.LanguageManager []
  defaultLanguage <- fromJust <$> #getLanguage languageManager "def"

  codeTreeStore <- new Gtk.TreeStore []
  #setColumnTypes codeTreeStore [gtypeString, gtypeString, gtypeString]

  -- Build the UI:

  codeSourceView <- new GtkSource.View
    [
      #autoIndent := True,
      #highlightCurrentLine := True,
      #showLineNumbers := True,
      #tabWidth := 4,
      #monospace := True
    ]

  codeTreeView <- new Gtk.TreeView
    [
      #enableSearch := False,
      #headersVisible := False,
      #model := codeTreeStore
    ]

  logTextView <- new Gtk.TextView
    [
      #editable := False,
      #monospace := True,
      #wrapMode := Gtk.WrapModeWord
    ]

  codeSourceScrolledWindow <- new Gtk.ScrolledWindow [#child := codeSourceView]
  codeTreeScrolledWindow <- new Gtk.ScrolledWindow [#child := codeTreeView]

  innerPaned <- new Gtk.Paned [#orientation := Gtk.OrientationHorizontal]
  #pack1 innerPaned codeSourceScrolledWindow True False
  #pack2 innerPaned codeTreeScrolledWindow True False

  outerPaned <- new Gtk.Paned [#orientation := Gtk.OrientationVertical]
  #pack1 outerPaned innerPaned True False
  #pack2 outerPaned logTextView False False

  window <- new Gtk.ApplicationWindow
    [
      #application := application,
      #defaultWidth := 1280,
      #defaultHeight := 720,
      #title := "",
      #child := outerPaned
    ]

  -- Set up codeTextBuffer:

  codeTextBuffer <- unsafeCastTo GtkSource.Buffer =<< #getBuffer codeSourceView
  #setHighlightMatchingBrackets codeTextBuffer False
  #setHighlightSyntax codeTextBuffer False

  tagTable <- #getTagTable codeTextBuffer
  styleScheme <- fromJust <$> #getStyleScheme codeTextBuffer

  for_ styleIds $ \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    style <- fromJust <$> Helpers.getStyle defaultLanguage styleScheme styleId
    #apply style tag
    #add tagTable tag

  -- Set up codeTreeView:

  cellRenderer <- new Gtk.CellRendererText [#family := "monospace"]

  for_ (zip [0..] [False, False, True]) $ \(column, expand) -> do
    treeViewColumn <- new Gtk.TreeViewColumn []
    #packEnd treeViewColumn cellRenderer expand
    #addAttribute treeViewColumn cellRenderer "text" column
    #appendColumn codeTreeView treeViewColumn

  -- Set up logTextView:

  logTextBuffer <- #getBuffer logTextView

  -- Register listeners:

  threadIdVar <- newMVar =<< forkIO (pure ())
  statementVar <- newMVar Nothing

  on codeTextBuffer #changed $ do
    text <- fromJust <$> get codeTextBuffer #text

    killThread =<< takeMVar threadIdVar
    swapMVar statementVar Nothing

    threadId <- forkIO $ do
      let statement = Parser.parse Parsers.statement (Input 0 text)

      case statement of
        Result.Success statement _ -> Gtk.postGUIASync $ do
          swapMVar statementVar (Just statement)

          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          for_ styleIds $ \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter

          highlightStatement codeTextBuffer statement
          highlightStatementParentheses codeTextBuffer statement =<< Helpers.getInsertTextIter codeTextBuffer

          #clear codeTreeStore
          displayStatement codeTextBuffer codeTreeStore statement

          set logTextBuffer [#text := ""]

        Result.Failure _ position expectations -> Gtk.postGUIASync $ do
          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          #removeTagByName codeTextBuffer "error" startTextIter endTextIter

          startTextIter <- #getIterAtOffset codeTextBuffer (fromIntegral position)
          for_ styleIds $ \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter
          #applyTagByName codeTextBuffer "error" startTextIter endTextIter

          (line, column) <- Helpers.getLineColumn startTextIter
          let prefix = "[" <> Text.pack (show line) <> ":" <> Text.pack (show column) <> "] "
          set logTextBuffer [#text := prefix <> Helpers.expectationsText expectations]

    putMVar threadIdVar threadId

  on codeTextBuffer (PropertyNotify #cursorPosition) . const . void . runMaybeT $ do
    (startTextIter, endTextIter) <- #getBounds codeTextBuffer
    #removeTagByName codeTextBuffer "parenthesis" startTextIter endTextIter

    statement <- MaybeT (readMVar statementVar)
    highlightStatementParentheses codeTextBuffer statement =<< Helpers.getInsertTextIter codeTextBuffer

  -- Display the UI:

  #showAll window


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> m ()
highlightStatement isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go (Syntax.DeclareStatement t variable _) = do
      highlight textBuffer "type" t
      highlight textBuffer "identifier" variable

    go (Syntax.DeclareAndAssignStatement t variable _ value _) = do
      highlight textBuffer "type" t
      highlight textBuffer "identifier" variable
      highlightExpression textBuffer value

    go (Syntax.ExpressionStatement value _) = highlightExpression textBuffer value

    go (Syntax.IfStatement ifKeyword predicate trueBranch) = do
      highlight textBuffer "keyword" ifKeyword
      highlightExpression textBuffer predicate
      highlightStatement textBuffer trueBranch

    go (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch) = do
      highlight textBuffer "keyword" ifKeyword
      highlightExpression textBuffer predicate
      highlightStatement textBuffer trueBranch
      highlight textBuffer "keyword" elseKeyword
      highlightStatement textBuffer falseBranch

    go (Syntax.WhileStatement whileKeyword predicate body) = do
      highlight textBuffer "keyword" whileKeyword
      highlightExpression textBuffer predicate
      highlightStatement textBuffer body

    go (Syntax.DoWhileStatement doKeyword body whileKeyword predicate _) = do
      highlight textBuffer "keyword" doKeyword
      highlightStatement textBuffer body
      highlight textBuffer "keyword" whileKeyword
      highlightExpression textBuffer predicate

    go (Syntax.ReturnStatement returnKeyword value _) = do
      highlight textBuffer "keyword" returnKeyword
      highlightExpression textBuffer value

    go (Syntax.BlockStatement _ statements _) = for_ statements (highlightStatement textBuffer)


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> m ()
highlightExpression isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go integer@Syntax.IntegerExpression{} = highlight textBuffer "integer" integer

    go (Syntax.IdentifierExpression identifier) = highlight textBuffer "identifier" identifier

    go (Syntax.UnaryExpression operator operand) = do
      highlight textBuffer "operator" operator
      go operand

    go (Syntax.BinaryExpression left operator right) = do
      go left
      highlight textBuffer "operator" operator
      go right

    go (Syntax.AssignExpression identifier operator value) = do
      highlight textBuffer "identifier" identifier
      highlight textBuffer "operator" operator
      go value

    go (Syntax.ParenthesizedExpression _ expression _) = go expression


highlightStatementParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> Gtk.TextIter -> m Bool
highlightStatementParentheses isTextBuffer statement insertTextIter = go statement
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.DeclareStatement{} = pure False

    go (Syntax.DeclareAndAssignStatement _ _ _ value _) = highlightExpressionParentheses textBuffer value insertTextIter

    go (Syntax.ExpressionStatement value _) = highlightExpressionParentheses textBuffer value insertTextIter

    go (Syntax.IfStatement _ predicate trueBranch) = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else
        go trueBranch

    go (Syntax.IfElseStatement _ predicate trueBranch _ falseBranch) = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else do
        done <- go trueBranch

        if done then
          pure True
        else
          go falseBranch

    go (Syntax.WhileStatement _ predicate body) = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else
        go body

    go (Syntax.DoWhileStatement _ body _ predicate _) = do
      done <- go body

      if done then
        pure True
      else
        highlightExpressionParentheses textBuffer predicate insertTextIter

    go (Syntax.ReturnStatement _ value _) = highlightExpressionParentheses textBuffer value insertTextIter

    go (Syntax.BlockStatement open statements close) = do
      done <- foldlM (\a s -> if a then pure True else go s) False statements

      if done then
        pure True
      else
        highlightParentheses textBuffer open close insertTextIter


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> Gtk.TextIter -> m Bool
highlightExpressionParentheses isTextBuffer expression insertTextIter = go expression
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

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
        highlightParentheses textBuffer open close insertTextIter


highlightParentheses :: (Gtk.IsTextBuffer a, Syntax b, Syntax c, MonadIO m) => a -> b -> c -> Gtk.TextIter -> m Bool
highlightParentheses isTextBuffer open close insertTextIter = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer

  openStartTextIter <- #getIterAtOffset textBuffer (Syntax.start open)
  openEndTextIter <- #getIterAtOffset textBuffer (Syntax.end open)

  closeStartTextIter <- #getIterAtOffset textBuffer (Syntax.start close)
  closeEndTextIter <- #getIterAtOffset textBuffer (Syntax.end close)

  applyParenthesisTag <- foldlM (\a i -> if a then pure True else #equal insertTextIter i) False
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    #applyTagByName textBuffer "parenthesis" openStartTextIter openEndTextIter
    #applyTagByName textBuffer "parenthesis" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlight :: (Gtk.IsTextBuffer a, Syntax b, MonadIO m) => a -> Text -> b -> m ()
highlight isTextBuffer tagName syntax = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer

  startTextIter <- #getIterAtOffset textBuffer (Syntax.start syntax)
  endTextIter <- #getIterAtOffset textBuffer (Syntax.end syntax)
  #applyTagByName textBuffer tagName startTextIter endTextIter


displayStatement :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Syntax.Statement -> IO ()
displayStatement isTextBuffer isTreeStore = go Nothing
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go parentTreeIter s@(Syntax.DeclareStatement t variable terminator) = void $ do
      let (Syntax.Identifier tName _) = t
          (Syntax.Identifier variableName _) = variable

      childTreeIter <- display textBuffer treeStore parentTreeIter s "DeclareStatement" ""
      display textBuffer treeStore childTreeIter t "Identifier" tName
      display textBuffer treeStore childTreeIter variable "Identifier" variableName
      display textBuffer treeStore childTreeIter terminator "Token" ";"

    go parentTreeIter s@(Syntax.DeclareAndAssignStatement t variable equalSign value terminator) = void $ do
      let (Syntax.Identifier tName _) = t
          (Syntax.Identifier variableName _) = variable

      childTreeIter <- display textBuffer treeStore parentTreeIter s "DeclareAndAssignStatement" ""
      display textBuffer treeStore childTreeIter t "Identifier" tName
      display textBuffer treeStore childTreeIter variable "Identifier" variableName
      display textBuffer treeStore childTreeIter equalSign "Token" "="
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" ";"

    go parentTreeIter s@(Syntax.ExpressionStatement value terminator) = void $ do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "ExpressionStatement" ""
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" ";"

    go parentTreeIter s@(Syntax.IfStatement ifKeyword predicate trueBranch) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "IfStatement" ""
      display textBuffer treeStore childTreeIter ifKeyword "Token" "if"
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter trueBranch

    go parentTreeIter s@(Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "IfStatement" ""
      display textBuffer treeStore childTreeIter ifKeyword "Token" "if"
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter trueBranch
      display textBuffer treeStore childTreeIter elseKeyword "Token" "else"
      go childTreeIter falseBranch

    go parentTreeIter s@(Syntax.WhileStatement whileKeyword predicate body) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "WhileStatement" ""
      display textBuffer treeStore childTreeIter whileKeyword "Token" "while"
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter body

    go parentTreeIter s@(Syntax.DoWhileStatement doKeyword body whileKeyword predicate terminator) = void $ do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "DoWhileStatement" ""
      display textBuffer treeStore childTreeIter doKeyword "Token" "do"
      go childTreeIter body
      display textBuffer treeStore childTreeIter whileKeyword "Token" "while"
      displayExpression textBuffer treeStore childTreeIter predicate
      display textBuffer treeStore childTreeIter terminator "Token" ";"

    go parentTreeIter s@(Syntax.ReturnStatement returnKeyword value terminator) = void $ do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "ReturnStatement" ""
      display textBuffer treeStore childTreeIter returnKeyword "Token" "return"
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" ";"

    go parentTreeIter s@(Syntax.BlockStatement open statements close) = void $ do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "BlockStatement" ""
      display textBuffer treeStore childTreeIter open "Token" "{"
      for_ statements (go childTreeIter)
      display textBuffer treeStore childTreeIter close "Token" "}"


displayExpression :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression -> IO ()
displayExpression isTextBuffer isTreeStore parentTreeIter = go parentTreeIter
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go parentTreeIter e@(Syntax.IntegerExpression value _) =
      void (display textBuffer treeStore parentTreeIter e "IntegerExpression" (Text.pack (show value)))

    go parentTreeIter e@(Syntax.IdentifierExpression identifier) = void $ do
      let (Syntax.Identifier identifierName _) = identifier

      childTreeIter <- display textBuffer treeStore parentTreeIter e "IdentifierExpression" ""
      display textBuffer treeStore childTreeIter identifier "Identifier" identifierName

    go parentTreeIter e@(Syntax.UnaryExpression operator operand) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "UnaryExpression" ""
      displayUnaryOperator textBuffer treeStore childTreeIter operator
      go childTreeIter operand

    go parentTreeIter e@(Syntax.BinaryExpression left operator right) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "BinaryExpression" ""
      go childTreeIter left
      displayBinaryOperator textBuffer treeStore childTreeIter operator
      go childTreeIter right

    go parentTreeIter e@(Syntax.AssignExpression target operator value) = do
      let (Syntax.Identifier targetName _) = target

      childTreeIter <- display textBuffer treeStore parentTreeIter e "AssignExpression" ""
      display textBuffer treeStore childTreeIter target "Identifier" targetName
      displayAssignOperator textBuffer treeStore childTreeIter operator
      go childTreeIter value

    go parentTreeIter e@(Syntax.ParenthesizedExpression open expression close) = void $ do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "ParenthesizedExpression" ""
      display textBuffer treeStore childTreeIter open "Token" "("
      go childTreeIter expression
      display textBuffer treeStore childTreeIter close "Token" ")"


displayUnaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator -> IO ()
displayUnaryOperator isTextBuffer isTreeStore parentTreeIter operator =
  void (display isTextBuffer isTreeStore parentTreeIter operator label literal)
  where
    (label, literal) = case operator of
      Syntax.PlusOperator{} -> ("PlusOperator", "+")
      Syntax.MinusOperator{} -> ("MinusOperator", "-")
      Syntax.NotOperator{} -> ("NotOperator", "!")


displayBinaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator -> IO ()
displayBinaryOperator isTextBuffer isTreeStore parentTreeIter operator =
  void (display isTextBuffer isTreeStore parentTreeIter operator label literal)
  where
    (label, literal) = case operator of
      Syntax.AddOperator{} -> ("AddOperator", "+")
      Syntax.SubtractOperator{} -> ("SubtractOperator", "-")
      Syntax.MultiplyOperator{} -> ("MultiplyOperator", "*")
      Syntax.DivideOperator{} -> ("DivideOperator", "/")
      Syntax.RemainderOperator{} -> ("RemainderOperator", "%")
      Syntax.EqualOperator{} -> ("EqualOperator", "==")
      Syntax.NotEqualOperator{} -> ("NotEqualOperator", "!=")
      Syntax.LessOperator{} -> ("LessOperator", "<")
      Syntax.LessOrEqualOperator{} -> ("LessOrEqualOperator", "<=")
      Syntax.GreaterOperator{} -> ("GreaterOperator", ">")
      Syntax.GreaterOrEqualOperator{} -> ("GreaterOrEqualOperator", ">=")
      Syntax.AndOperator{} -> ("AndOperator", "&")
      Syntax.OrOperator{} -> ("OrOperator", "|")


displayAssignOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.AssignOperator -> IO ()
displayAssignOperator isTextBuffer isTreeStore parentTreeIter operator =
  void (display isTextBuffer isTreeStore parentTreeIter operator label literal)
  where
    (label, literal) = case operator of
      Syntax.AssignOperator{} -> ("AssignOperator", "=")
      Syntax.AddAssignOperator{} -> ("AddAssignOperator", "+=")
      Syntax.SubtractAssignOperator{} -> ("SubtractAssignOperator", "-=")
      Syntax.MultiplyAssignOperator{} -> ("MultiplyAssignOperator", "*=")
      Syntax.DivideAssignOperator{} -> ("DivideAssignOperator", "/=")
      Syntax.RemainderAssignOperator{} -> ("RemainderAssignOperator", "%=")


display :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax c) => a -> b -> Maybe Gtk.TreeIter -> c -> Text -> Text -> IO (Maybe Gtk.TreeIter)
display isTextBuffer isTreeStore parentTreeIter syntax label value = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer
      treeStore = isTreeStore `asA` Gtk.TreeStore

  (startLine, startColumn) <- Helpers.getLineColumn =<< #getIterAtOffset textBuffer (Syntax.start syntax)
  (endLine, endColumn) <- Helpers.getLineColumn =<< #getIterAtOffset textBuffer (Syntax.end syntax)
  let span = "[" ++ show startLine ++ ":" ++ show startColumn ++ "-" ++ show endLine ++ ":" ++ show endColumn ++ "]"

  childTreeIter <- #append treeStore parentTreeIter
  #setValue treeStore childTreeIter 0 =<< toGValue (Just span)
  #setValue treeStore childTreeIter 1 =<< toGValue (Just label)
  #setValue treeStore childTreeIter 2 =<< toGValue (Just value)

  pure (Just childTreeIter)
