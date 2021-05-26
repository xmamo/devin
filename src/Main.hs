module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import System.Environment
import System.Exit

import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as Text

import Data.GI.Base
import Data.GI.Base.GType
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.GtkSource as GtkSource

import qualified Helpers
import Input (Input (Input))
import qualified Parser
import qualified Parsers
import qualified Result
import Syntax (Syntax)
import qualified Syntax


main :: IO ()
main = do
  args <- getArgs
  application <- new Gtk.Application []
  on application #activate (onActivate application)
  status <- #run application (Just args)
  exitWith (if status == 0 then ExitSuccess else ExitFailure (fromIntegral status))


onActivate :: Gtk.IsApplication a => a -> Gio.ApplicationActivateCallback
onActivate isApplication = do
  let application = isApplication `asA` Gtk.Application

      styleIds =
        [
          ("keyword", "def:keyword"),
          ("identifier", "def:identifier"),
          ("type", "def:type"),
          ("integer", "def:decimal"),
          ("operator", "def:operator"),
          ("comment", "def:comment"),
          ("error", "def:error"),
          ("bracket", "bracket-match")
        ]

  -- Create defaultLanguage and codeTreeStore, which are needed later:

  languageManager <- new GtkSource.LanguageManager []
  defaultLanguage <- fromJust <$> #getLanguage languageManager "def"

  codeTreeStore <- new Gtk.TreeStore []
  #setColumnTypes codeTreeStore (replicate 3 gtypeString)

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

  -- Set up codeSourceView:

  codeTextBuffer <- unsafeCastTo GtkSource.Buffer =<< #getBuffer codeSourceView
  #setHighlightMatchingBrackets codeTextBuffer False
  #setHighlightSyntax codeTextBuffer False

  styleScheme <- fromJust <$> #getStyleScheme codeTextBuffer
  tagTable <- #getTagTable codeTextBuffer

  for_ styleIds $ \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    style <- fromJust <$> Helpers.getStyle defaultLanguage styleScheme styleId
    #apply style tag
    #add tagTable tag

  -- Set up codeTreeView:

  cellRenderer <- new Gtk.CellRendererText [#family := "monospace"]

  for_ (zip [0 ..] [False, False, True]) $ \(column, expand) -> do
    treeViewColumn <- new Gtk.TreeViewColumn []
    #packEnd treeViewColumn cellRenderer expand
    #addAttribute treeViewColumn cellRenderer "text" column
    #appendColumn codeTreeView treeViewColumn

  -- Set up logTextView:

  logTextBuffer <- #getBuffer logTextView

  -- Register listeners:

  threadIdVar <- newMVar =<< forkIO (pure ())
  declarationsVar <- newMVar []

  on codeTextBuffer #changed $ do
    text <- fromJust <$> get codeTextBuffer #text

    killThread =<< takeMVar threadIdVar
    swapMVar declarationsVar []

    threadId <- forkIO $ do
      let (declarations, comments) = runWriter (Parser.parseT Parsers.declarations (Input 0 text))

      case declarations of
        Result.Success declarations _ -> Gtk.postGUIASync $ do
          swapMVar declarationsVar declarations

          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          for_ styleIds $ \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter

          insertTextIter <- Helpers.getInsertTextIter codeTextBuffer

          for_ declarations $ \declaration -> do
            highlightDeclaration codeTextBuffer declaration
            highlightDeclarationParentheses codeTextBuffer declaration insertTextIter

          for_ comments (highlight codeTextBuffer "comment")

          #clear codeTreeStore
          for_ declarations (displayDeclaration codeTextBuffer codeTreeStore Nothing)

          set logTextBuffer [#text := ""]

        Result.Failure _ position expectations -> Gtk.postGUIASync $ do
          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          #removeTagByName codeTextBuffer "error" startTextIter endTextIter

          startTextIter <- #getIterAtOffset codeTextBuffer (fromIntegral position)
          for_ styleIds $ \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter
          #applyTagByName codeTextBuffer "error" startTextIter endTextIter

          (line, column) <- Helpers.getLineColumn startTextIter
          let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
          set logTextBuffer [#text := prefix <> Helpers.expectationsText expectations]

    putMVar threadIdVar threadId

  on codeTextBuffer (PropertyNotify #cursorPosition) . const . void $ do
    (startTextIter, endTextIter) <- #getBounds codeTextBuffer
    #removeTagByName codeTextBuffer "bracket" startTextIter endTextIter

    declarations <- readMVar declarationsVar
    insertTextIter <- Helpers.getInsertTextIter codeTextBuffer
    for_ declarations $ \declaration -> highlightDeclarationParentheses codeTextBuffer declaration insertTextIter

  -- Display the UI:

  #showAll window


highlightDeclaration :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Declaration -> m ()
highlightDeclaration isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go (Syntax.VariableDeclaration t variable _) = do
      highlight textBuffer "type" t
      highlight textBuffer "identifier" variable

    go (Syntax.VariableAssignDeclaration t variable _ value _) = do
      highlight textBuffer "type" t
      highlight textBuffer "identifier" variable
      highlightExpression textBuffer value

    go (Syntax.FunctionDeclaration t name _ parameters _ body) = do
      highlight textBuffer "type" t
      highlight textBuffer "identifier" name
      highlightParameters parameters
      highlightStatement textBuffer body

    highlightParameters Nothing = pure ()

    highlightParameters (Just ((t, name), rest)) = do
      highlight textBuffer "type" t
      highlight textBuffer "type" name

      for_ rest $ \(_, t, name) -> do
        highlight textBuffer "type" t
        highlight textBuffer "type" name


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> m ()
highlightStatement isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go (Syntax.ExpressionStatement value _) = highlightExpression textBuffer value

    go (Syntax.IfStatement ifKeyword predicate trueBranch) = do
      highlight textBuffer "keyword" ifKeyword
      highlightExpression textBuffer predicate
      go trueBranch

    go (Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch) = do
      highlight textBuffer "keyword" ifKeyword
      highlightExpression textBuffer predicate
      go trueBranch
      highlight textBuffer "keyword" elseKeyword
      go falseBranch

    go (Syntax.WhileStatement whileKeyword predicate body) = do
      highlight textBuffer "keyword" whileKeyword
      highlightExpression textBuffer predicate
      go body

    go (Syntax.DoWhileStatement doKeyword body whileKeyword predicate _) = do
      highlight textBuffer "keyword" doKeyword
      go body
      highlight textBuffer "keyword" whileKeyword
      highlightExpression textBuffer predicate

    go (Syntax.ReturnStatement returnKeyword value _) = do
      highlight textBuffer "keyword" returnKeyword
      highlightExpression textBuffer value

    go (Syntax.BlockStatement _ elements _) = for_ elements highlightElement

    highlightElement (Left declaration) = highlightDeclaration textBuffer declaration
    highlightElement (Right statement) = go statement


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> m ()
highlightExpression isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go e@(Syntax.IntegerExpression _ _) = highlight textBuffer "integer" e

    go (Syntax.IdentifierExpression identifier) = highlight textBuffer "identifier" identifier

    go (Syntax.CallExpression target _ arguments _) = do
      highlight textBuffer "identifier" target
      highlightArguments arguments

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

    highlightArguments Nothing = pure ()

    highlightArguments (Just (first, rest)) = do
      go first
      for_ rest (go . snd)


highlightDeclarationParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Declaration -> Gtk.TextIter -> m Bool
highlightDeclarationParentheses isTextBuffer declaration insertTextIter = go declaration
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go (Syntax.VariableDeclaration _ _ _) = pure False

    go (Syntax.VariableAssignDeclaration _ _ _ value _) = highlightExpressionParentheses textBuffer value insertTextIter

    go (Syntax.FunctionDeclaration _ _ open _ close body) = do
      done <- highlightParentheses textBuffer open close insertTextIter

      if done then
        pure True
      else
        highlightStatementParentheses textBuffer body insertTextIter


highlightStatementParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> Gtk.TextIter -> m Bool
highlightStatementParentheses isTextBuffer statement insertTextIter = go statement
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

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

    go (Syntax.BlockStatement open elements close) = do
      done <- foldlM (\a e -> if a then pure True else highlightElementParentheses e) False elements

      if done then
        pure True
      else
        highlightParentheses textBuffer open close insertTextIter

    highlightElementParentheses (Left declaration) =
      highlightDeclarationParentheses textBuffer declaration insertTextIter

    highlightElementParentheses (Right statement) = go statement


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> Gtk.TextIter -> m Bool
highlightExpressionParentheses isTextBuffer expression insertTextIter = go expression
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go (Syntax.IntegerExpression _ _) = pure False

    go (Syntax.IdentifierExpression _) = pure False

    go (Syntax.CallExpression _ open arguments close) = do
      done <- highlightArgumentParentheses arguments

      if done then
        pure True
      else
        highlightParentheses textBuffer open close insertTextIter

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

    highlightArgumentParentheses Nothing = pure False

    highlightArgumentParentheses (Just (first, rest)) = do
      done <- go first

      if done then
        pure True
      else
        foldlM (\a (_, e) -> if a then pure True else go e) False rest


highlightParentheses :: (Gtk.IsTextBuffer a, Syntax b, Syntax c, MonadIO m) => a -> b -> c -> Gtk.TextIter -> m Bool
highlightParentheses isTextBuffer open close insertTextIter = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer

  openStartTextIter <- #getIterAtOffset textBuffer (Syntax.start open)
  openEndTextIter <- #getIterAtOffset textBuffer (Syntax.end open)

  closeStartTextIter <- #getIterAtOffset textBuffer (Syntax.start close)
  closeEndTextIter <- #getIterAtOffset textBuffer (Syntax.end close)

  applyParenthesisTag <- foldlM (\a ti -> if a then pure True else #equal insertTextIter ti) False
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    #applyTagByName textBuffer "bracket" openStartTextIter openEndTextIter
    #applyTagByName textBuffer "bracket" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlight :: (Gtk.IsTextBuffer a, Syntax b, MonadIO m) => a -> Text -> b -> m ()
highlight isTextBuffer tagName syntax = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer

  startTextIter <- #getIterAtOffset textBuffer (Syntax.start syntax)
  endTextIter <- #getIterAtOffset textBuffer (Syntax.end syntax)
  #applyTagByName textBuffer tagName startTextIter endTextIter


displayDeclaration ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Declaration -> IO (Maybe Gtk.TreeIter)

displayDeclaration isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go parentTreeIter d@(Syntax.VariableDeclaration t variable terminator) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter d "VariableDeclaration" False
      display textBuffer treeStore childTreeIter t "Identifier" True
      display textBuffer treeStore childTreeIter variable "Identifier" True
      display textBuffer treeStore childTreeIter terminator "Token" True
      pure childTreeIter

    go parentTreeIter d@(Syntax.VariableAssignDeclaration t variable equalSign value terminator) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter d "VariableAssignDeclaration" False
      display textBuffer treeStore childTreeIter t "Identifier" True
      display textBuffer treeStore childTreeIter variable "Identifier" True
      display textBuffer treeStore childTreeIter equalSign "Token" True
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" True
      pure childTreeIter

    go parentTreeIter d@(Syntax.FunctionDeclaration name t open parameters close body) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter d "FunctionDeclaration" False
      display textBuffer treeStore childTreeIter name "Identifier" True
      display textBuffer treeStore childTreeIter t "Identifier" True
      display textBuffer treeStore childTreeIter open "Token" True
      displayParameters childTreeIter parameters
      display textBuffer treeStore childTreeIter close "Token" True
      displayStatement textBuffer treeStore childTreeIter body
      pure childTreeIter

    displayParameters _ Nothing = pure ()

    displayParameters parentTreeIter (Just ((t, name), rest)) = do
      display textBuffer treeStore parentTreeIter t "Identifier" True
      display textBuffer treeStore parentTreeIter name "Identifier" True

      for_ rest $ \(separator, t, name) -> do
        display textBuffer treeStore parentTreeIter separator "Token" True
        display textBuffer treeStore parentTreeIter t "Identifier" True
        display textBuffer treeStore parentTreeIter name "Identifier" True


displayStatement ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Statement -> IO (Maybe Gtk.TreeIter)

displayStatement isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go parentTreeIter s@(Syntax.ExpressionStatement value terminator) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "ExpressionStatement" False
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" True
      pure childTreeIter

    go parentTreeIter s@(Syntax.IfStatement ifKeyword predicate trueBranch) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "IfStatement" False
      display textBuffer treeStore childTreeIter ifKeyword "Token" True
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter trueBranch
      pure childTreeIter

    go parentTreeIter s@(Syntax.IfElseStatement ifKeyword predicate trueBranch elseKeyword falseBranch) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "IfElseStatement" False
      display textBuffer treeStore childTreeIter ifKeyword "Token" True
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter trueBranch
      display textBuffer treeStore childTreeIter elseKeyword "Token" True
      go childTreeIter falseBranch
      pure childTreeIter

    go parentTreeIter s@(Syntax.WhileStatement whileKeyword predicate body) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "WhileStatement" False
      display textBuffer treeStore childTreeIter whileKeyword "Token" True
      displayExpression textBuffer treeStore childTreeIter predicate
      go childTreeIter body
      pure childTreeIter

    go parentTreeIter s@(Syntax.DoWhileStatement doKeyword body whileKeyword predicate terminator) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "DoWhileStatement" False
      display textBuffer treeStore childTreeIter doKeyword "Token" True
      go childTreeIter body
      display textBuffer treeStore childTreeIter whileKeyword "Token" True
      displayExpression textBuffer treeStore childTreeIter predicate
      display textBuffer treeStore childTreeIter terminator "Token" True
      pure childTreeIter

    go parentTreeIter s@(Syntax.ReturnStatement returnKeyword value terminator) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "ReturnStatement" False
      display textBuffer treeStore childTreeIter returnKeyword "Token" True
      displayExpression textBuffer treeStore childTreeIter value
      display textBuffer treeStore childTreeIter terminator "Token" True
      pure childTreeIter

    go parentTreeIter s@(Syntax.BlockStatement open elements close) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter s "BlockStatement" False
      display textBuffer treeStore childTreeIter open "Token" True
      for_ elements (displayElement childTreeIter)
      display textBuffer treeStore childTreeIter close "Token" True
      pure childTreeIter

    displayElement parentTreeIter (Left declaration) =
      displayDeclaration textBuffer treeStore parentTreeIter declaration

    displayElement parentTreeIter (Right statement) = go parentTreeIter statement


displayExpression ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression -> IO (Maybe Gtk.TreeIter)

displayExpression isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go parentTreeIter e@(Syntax.IntegerExpression _ _) =
      display textBuffer treeStore parentTreeIter e "IntegerExpression" True

    go parentTreeIter e@(Syntax.IdentifierExpression identifier) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "IdentifierExpression" False
      display textBuffer treeStore childTreeIter identifier "Identifier" True
      pure childTreeIter

    go parentTreeIter e@(Syntax.CallExpression target open arguments close) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "CallExpression" False
      display textBuffer treeStore childTreeIter target "Identifier" True
      display textBuffer treeStore childTreeIter open "Token" True
      displayArguments childTreeIter arguments
      display textBuffer treeStore childTreeIter close "Token" True
      pure childTreeIter

    go parentTreeIter e@(Syntax.UnaryExpression operator operand) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "UnaryExpression" False
      displayUnaryOperator textBuffer treeStore childTreeIter operator
      go childTreeIter operand
      pure childTreeIter

    go parentTreeIter e@(Syntax.BinaryExpression left operator right) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "BinaryExpression" False
      go childTreeIter left
      displayBinaryOperator textBuffer treeStore childTreeIter operator
      go childTreeIter right
      pure childTreeIter

    go parentTreeIter e@(Syntax.AssignExpression target operator value) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "AssignExpression" False
      display textBuffer treeStore childTreeIter target "Identifier" True
      displayAssignOperator textBuffer treeStore childTreeIter operator
      go childTreeIter value
      pure childTreeIter

    go parentTreeIter e@(Syntax.ParenthesizedExpression open expression close) = do
      childTreeIter <- display textBuffer treeStore parentTreeIter e "ParenthesizedExpression" False
      display textBuffer treeStore childTreeIter open "Token" True
      go childTreeIter expression
      display textBuffer treeStore childTreeIter close "Token" True
      pure childTreeIter

    displayArguments _ Nothing = pure ()

    displayArguments parentTreeIter (Just (first, rest)) = do
      go parentTreeIter first

      for_ rest $ \(separator, argument) -> do
        display textBuffer treeStore parentTreeIter separator "Token" True
        go parentTreeIter argument


displayUnaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator -> IO (Maybe Gtk.TreeIter)

displayUnaryOperator isTextBuffer isTreeStore parentTreeIter operator =
  display isTextBuffer isTreeStore parentTreeIter operator label True
  where
    label = case operator of
      Syntax.PlusOperator _ -> "PlusOperator"
      Syntax.MinusOperator _ -> "MinusOperator"
      Syntax.NotOperator _ -> "NotOperator"

displayBinaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator -> IO (Maybe Gtk.TreeIter)

displayBinaryOperator isTextBuffer isTreeStore parentTreeIter operator =
  display isTextBuffer isTreeStore parentTreeIter operator label True
  where
    label = case operator of
      Syntax.AddOperator _ -> "AddOperator"
      Syntax.SubtractOperator _ -> "SubtractOperator"
      Syntax.MultiplyOperator _ -> "MultiplyOperator"
      Syntax.DivideOperator _ -> "DivideOperator"
      Syntax.RemainderOperator _ -> "RemainderOperator"
      Syntax.EqualOperator _ -> "EqualOperator"
      Syntax.NotEqualOperator _ -> "NotEqualOperator"
      Syntax.LessOperator _ -> "LessOperator"
      Syntax.LessOrEqualOperator _ -> "LessOrEqualOperator"
      Syntax.GreaterOperator _ -> "GreaterOperator"
      Syntax.GreaterOrEqualOperator _ -> "GreaterOrEqualOperator"
      Syntax.AndOperator _ -> "AndOperator"
      Syntax.OrOperator _ -> "OrOperator"

displayAssignOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.AssignOperator -> IO (Maybe Gtk.TreeIter)

displayAssignOperator isTextBuffer isTreeStore parentTreeIter operator =
  display isTextBuffer isTreeStore parentTreeIter operator label True
  where
    label = case operator of
      Syntax.AssignOperator _ -> "AssignOperator"
      Syntax.AddAssignOperator _ -> "AddAssignOperator"
      Syntax.SubtractAssignOperator _ -> "SubtractAssignOperator"
      Syntax.MultiplyAssignOperator _ -> "MultiplyAssignOperator"
      Syntax.DivideAssignOperator _ -> "DivideAssignOperator"
      Syntax.RemainderAssignOperator _ -> "RemainderAssignOperator"

display ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax c) =>
  a -> b -> Maybe Gtk.TreeIter -> c -> Text -> Bool -> IO (Maybe Gtk.TreeIter)

display isTextBuffer isTreeStore parentTreeIter syntax label isLeaf = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer
      treeStore = isTreeStore `asA` Gtk.TreeStore

  startTextIter <- #getIterAtOffset textBuffer (Syntax.start syntax)
  endTextIter <- #getIterAtOffset textBuffer (Syntax.end syntax)

  (startLine, startColumn) <- Helpers.getLineColumn startTextIter
  (endLine, endColumn) <- Helpers.getLineColumn endTextIter
  let span = "[" ++ show startLine ++ ":" ++ show startColumn ++ "-" ++ show endLine ++ ":" ++ show endColumn ++ "]"

  childTreeIter <- #append treeStore parentTreeIter

  #setValue treeStore childTreeIter 0 =<< toGValue (Just span)
  #setValue treeStore childTreeIter 1 =<< toGValue (Just label)

  when isLeaf $ do
    slice <- #getSlice textBuffer startTextIter endTextIter True
    #setValue treeStore childTreeIter 2 =<< toGValue (Just slice)

  pure (Just childTreeIter)
