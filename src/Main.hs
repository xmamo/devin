module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Traversable
import System.Environment
import System.Exit

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
import Span (Span)
import qualified Span
import Syntax (Syntax)
import qualified Syntax
import qualified Type


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
          ("number", "def:number"),
          ("operator", "def:operator"),
          ("comment", "def:comment"),
          ("error", "def:error"),
          ("bracket", "bracket-match")
        ]

  -- Create defaultLanguage and codeTreeStore, which are needed later:

  languageManager <- new GtkSource.LanguageManager []
  defaultLanguage <- fromJust <$> #getLanguage languageManager "def"

  codeTreeStore <- new Gtk.TreeStore []
  #setColumnTypes codeTreeStore [gtypeString, gtypeString]

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
  logScrolledWindow <- new Gtk.ScrolledWindow [#child := logTextView]

  innerPaned <- new Gtk.Paned [#orientation := Gtk.OrientationHorizontal]
  #pack1 innerPaned codeSourceScrolledWindow True False
  #pack2 innerPaned codeTreeScrolledWindow True False

  outerPaned <- new Gtk.Paned [#orientation := Gtk.OrientationVertical]
  #pack1 outerPaned innerPaned True False
  #pack2 outerPaned logScrolledWindow False False

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

  for_ styleIds \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    style <- fromJust <$> Helpers.getStyle defaultLanguage styleScheme styleId
    #apply style tag
    #add tagTable tag

  -- Set up codeTreeView:

  cellRenderer <- new Gtk.CellRendererText [#family := "monospace"]

  for_ [(0, False), (1, True)] \(column, expand) -> do
    treeViewColumn <- new Gtk.TreeViewColumn []
    #packEnd treeViewColumn cellRenderer expand
    #addAttribute treeViewColumn cellRenderer "text" column
    #appendColumn codeTreeView treeViewColumn

  -- Set up logTextView:

  logTextBuffer <- #getBuffer logTextView

  -- Register listeners:

  threadIdVar <- newMVar =<< forkIO (pure ())
  declarationsVar <- newMVar []

  on codeTextBuffer #changed do
    text <- fromMaybe "" <$> get codeTextBuffer #text

    killThread =<< takeMVar threadIdVar
    swapMVar declarationsVar []

    putMVar threadIdVar =<< forkIO case Parser.parse Parsers.declarations (Input 0 text) of
      Result.Success (declarations, comments) _ -> do
        Gtk.postGUIASync do
          swapMVar declarationsVar declarations

          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          for_ styleIds \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter

          insertTextIter <- Helpers.getInsertTextIter codeTextBuffer

          for_ declarations \declaration -> do
            highlightDeclaration codeTextBuffer declaration
            highlightDeclarationParentheses codeTextBuffer declaration insertTextIter

          for_ comments (highlight codeTextBuffer "comment" . Syntax.span)

          #clear codeTreeStore
          for_ declarations (displayDeclaration codeTextBuffer codeTreeStore Nothing)

          set logTextBuffer [#text := ""]

        let errors = Type.checkDeclarations Type.defaultEnvironment declarations

        Gtk.postGUIASync do
          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          #removeTagByName codeTextBuffer "error" startTextIter endTextIter

          log <- for errors \error -> do
            highlight codeTextBuffer "error" (Type.span error)

            startTextIter <- #getIterAtOffset codeTextBuffer (Type.start error)
            (line, column) <- Helpers.getLineColumn startTextIter
            let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
            pure (prefix <> Type.description error)

          set logTextBuffer [#text := Text.intercalate "\n" log]

      Result.Failure _ position expectations -> Gtk.postGUIASync do
        (startTextIter, endTextIter) <- #getBounds codeTextBuffer
        #removeTagByName codeTextBuffer "error" startTextIter endTextIter

        startTextIter <- #getIterAtOffset codeTextBuffer (fromIntegral position)
        for_ styleIds \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter
        #applyTagByName codeTextBuffer "error" startTextIter endTextIter

        (line, column) <- Helpers.getLineColumn startTextIter
        let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
        set logTextBuffer [#text := prefix <> Helpers.expectationsText expectations]

  on codeTextBuffer (PropertyNotify #cursorPosition) . const $ void do
    (startTextIter, endTextIter) <- #getBounds codeTextBuffer
    #removeTagByName codeTextBuffer "bracket" startTextIter endTextIter

    declarations <- readMVar declarationsVar
    insertTextIter <- Helpers.getInsertTextIter codeTextBuffer
    for_ declarations \declaration -> highlightDeclarationParentheses codeTextBuffer declaration insertTextIter

  -- Display the UI:

  #showAll window


highlightDeclaration :: Gtk.IsTextBuffer a => a -> Syntax.Declaration b -> IO ()
highlightDeclaration isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.VariableDeclaration {varKeyword, variableId, typeId, value} = do
      highlight textBuffer "keyword" (Syntax.span varKeyword)
      highlight textBuffer "identifier" (Syntax.span variableId)
      highlight textBuffer "type" (Syntax.span typeId)
      highlightExpression textBuffer value

    go Syntax.FunctionDeclaration {defKeyword, functionId, parameters, result, body} = do
      highlight textBuffer "keyword" (Syntax.span defKeyword)
      highlight textBuffer "identifier" (Syntax.span functionId)
      highlightParameters parameters
      highlightResult result
      highlightStatement textBuffer body

    highlightParameters Nothing = pure ()

    highlightParameters (Just ((id, _, typeId), rest)) =
      for_ ((id, typeId) : [(id, typeId) | (_, id, _, typeId) <- rest]) \(id, typeId) -> do
        highlight textBuffer "identifier" (Syntax.span id)
        highlight textBuffer "type" (Syntax.span typeId)

    highlightResult Nothing = pure ()
    highlightResult (Just (_, returnTypeId)) = highlight textBuffer "type" (Syntax.span returnTypeId)


highlightStatement :: Gtk.IsTextBuffer a => a -> Syntax.Statement b -> IO ()
highlightStatement isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.ExpressionStatement {value} = highlightExpression textBuffer value

    go Syntax.IfStatement {ifKeyword, predicate, trueBranch} = do
      highlight textBuffer "keyword" (Syntax.span ifKeyword)
      highlightExpression textBuffer predicate
      go trueBranch

    go Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} = do
      highlight textBuffer "keyword" (Syntax.span ifKeyword)
      highlightExpression textBuffer predicate
      go trueBranch
      highlight textBuffer "keyword" (Syntax.span elseKeyword)
      go falseBranch

    go Syntax.WhileStatement {whileKeyword, predicate, body} = do
      highlight textBuffer "keyword" (Syntax.span whileKeyword)
      highlightExpression textBuffer predicate
      go body

    go Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate} = do
      highlight textBuffer "keyword" (Syntax.span doKeyword)
      go body
      highlight textBuffer "keyword" (Syntax.span whileKeyword)
      highlightExpression textBuffer predicate

    go Syntax.ReturnStatement {returnKeyword, result} = do
      highlight textBuffer "keyword" (Syntax.span returnKeyword)
      for_ result (highlightExpression textBuffer)

    go Syntax.BlockStatement {elements} = for_ elements highlightElement

    highlightElement (Left declaration) = highlightDeclaration textBuffer declaration
    highlightElement (Right statement) = go statement


highlightExpression :: Gtk.IsTextBuffer a => a -> Syntax.Expression b -> IO ()
highlightExpression isTextBuffer = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.LiteralExpression {literal} = highlight textBuffer "number" (Syntax.span literal)

    go Syntax.VariableExpression {variableId} = highlight textBuffer "identifier" (Syntax.span variableId)

    go Syntax.CallExpression {targetId, arguments} = do
      highlight textBuffer "identifier" (Syntax.span targetId)
      highlightArguments arguments

    go Syntax.UnaryExpression {unary, operand} = do
      highlight textBuffer "operator" (Syntax.span unary)
      go operand

    go Syntax.BinaryExpression {left, binary, right} = do
      go left
      highlight textBuffer "operator" (Syntax.span binary)
      go right

    go Syntax.AssignExpression {targetId, assign, value} = do
      highlight textBuffer "identifier" (Syntax.span targetId)
      highlight textBuffer "operator" (Syntax.span assign)
      go value

    go Syntax.ParenthesizedExpression {inner} = go inner

    highlightArguments Nothing = pure ()
    highlightArguments (Just (first, rest)) = for_ (first : map snd rest) go


highlightDeclarationParentheses :: Gtk.IsTextBuffer a => a -> Syntax.Declaration b -> Gtk.TextIter -> IO Bool
highlightDeclarationParentheses isTextBuffer declaration insertTextIter = go declaration
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.VariableDeclaration {value} = highlightExpressionParentheses textBuffer value insertTextIter

    go Syntax.FunctionDeclaration {open, close, body} = do
      done <- highlightParentheses textBuffer open close insertTextIter

      if done then
        pure True
      else
        highlightStatementParentheses textBuffer body insertTextIter


highlightStatementParentheses :: Gtk.IsTextBuffer a => a -> Syntax.Statement b -> Gtk.TextIter -> IO Bool
highlightStatementParentheses isTextBuffer statement insertTextIter = go statement
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.ExpressionStatement {value} = highlightExpressionParentheses textBuffer value insertTextIter

    go Syntax.IfStatement {predicate, trueBranch} = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else
        go trueBranch

    go Syntax.IfElseStatement {predicate, trueBranch, falseBranch} = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else do
        done <- go trueBranch

        if done then
          pure True
        else
          go falseBranch

    go Syntax.WhileStatement {predicate, body} = do
      done <- highlightExpressionParentheses textBuffer predicate insertTextIter

      if done then
        pure True
      else
        go body

    go Syntax.DoWhileStatement {body, predicate} = do
      done <- go body

      if done then
        pure True
      else
        highlightExpressionParentheses textBuffer predicate insertTextIter

    go Syntax.ReturnStatement {result = Just result} = highlightExpressionParentheses textBuffer result insertTextIter

    go Syntax.ReturnStatement {result = Nothing} = pure False

    go Syntax.BlockStatement {open, elements, close} = do
      done <- foldlM (\a e -> if a then pure True else highlightElementParentheses e) False elements

      if done then
        pure True
      else
        highlightParentheses textBuffer open close insertTextIter

    highlightElementParentheses (Left declaration) =
      highlightDeclarationParentheses textBuffer declaration insertTextIter

    highlightElementParentheses (Right statement) = go statement


highlightExpressionParentheses :: Gtk.IsTextBuffer a => a -> Syntax.Expression b -> Gtk.TextIter -> IO Bool
highlightExpressionParentheses isTextBuffer expression insertTextIter = go expression
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer

    go Syntax.LiteralExpression {} = pure False

    go Syntax.VariableExpression {} = pure False

    go Syntax.CallExpression {open, arguments, close} = do
      done <- highlightArgumentParentheses arguments

      if done then
        pure True
      else
        highlightParentheses textBuffer open close insertTextIter

    go Syntax.UnaryExpression {operand} = go operand

    go Syntax.BinaryExpression {left, right} = do
      done <- go left

      if done then
        pure True
      else
        go right

    go Syntax.AssignExpression {value} = go value

    go Syntax.ParenthesizedExpression {open, inner, close} = do
      done <- go inner

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


highlightParentheses :: (Gtk.IsTextBuffer a, Syntax b, Syntax c) => a -> b -> c -> Gtk.TextIter -> IO Bool
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


highlight :: Gtk.IsTextBuffer a => a -> Text -> Span -> IO ()
highlight isTextBuffer tagName span = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer
  startTextIter <- #getIterAtOffset textBuffer (Span.start span)
  endTextIter <- #getIterAtOffset textBuffer (Span.end span)
  #applyTagByName textBuffer tagName startTextIter endTextIter


displayDeclaration ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Declaration c -> IO (Maybe Gtk.TreeIter)
displayDeclaration isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go treeIter d @ Syntax.VariableDeclaration {varKeyword, variableId, colon, typeId, equalSign, value, semicolon} = do
      treeIter' <- display textBuffer treeStore treeIter d "VariableAssignDeclaration" False
      display textBuffer treeStore treeIter' varKeyword "Token" True
      display textBuffer treeStore treeIter' variableId "Identifier" True
      display textBuffer treeStore treeIter' colon "Token" True
      display textBuffer treeStore treeIter' typeId "Identifier" True
      display textBuffer treeStore treeIter' equalSign "Token" True
      displayExpression textBuffer treeStore treeIter' value
      display textBuffer treeStore treeIter' semicolon "Token" True
      pure treeIter'

    go treeIter d @ Syntax.FunctionDeclaration {defKeyword, functionId, open, parameters, close, result, body} = do
      treeIter' <- display textBuffer treeStore treeIter d "FunctionDeclaration" False
      display textBuffer treeStore treeIter' defKeyword "Token" True
      display textBuffer treeStore treeIter' functionId "Identifier" True
      display textBuffer treeStore treeIter' open "Token" True
      displayParameters treeIter' parameters
      display textBuffer treeStore treeIter' close "Token" True
      displayResult treeIter' result
      displayStatement textBuffer treeStore treeIter' body
      pure treeIter'

    displayParameters _ Nothing = pure ()

    displayParameters treeIter (Just ((id, colon, typeId), rest)) = do
      display textBuffer treeStore treeIter id "Identifier" True
      display textBuffer treeStore treeIter colon "Token" True
      display textBuffer treeStore treeIter typeId "Identifier" True

      for_ rest \(comma, id, colon, typeId) -> do
        display textBuffer treeStore treeIter comma "Token" True
        display textBuffer treeStore treeIter id "Identifier" True
        display textBuffer treeStore treeIter colon "Token" True
        display textBuffer treeStore treeIter typeId "Identifier" True

    displayResult _ Nothing = pure ()

    displayResult treeIter (Just (arrow, returnTypeId)) = void do
      display textBuffer treeStore treeIter arrow "Token" True
      display textBuffer treeStore treeIter returnTypeId "Identifier" True


displayStatement ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Statement c -> IO (Maybe Gtk.TreeIter)
displayStatement isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go treeIter s @ Syntax.ExpressionStatement {value, semicolon} = do
      treeIter' <- display textBuffer treeStore treeIter s "ExpressionStatement" False
      displayExpression textBuffer treeStore treeIter' value
      display textBuffer treeStore treeIter' semicolon "Token" True
      pure treeIter'

    go treeIter s @ Syntax.IfStatement {ifKeyword, predicate, trueBranch} = do
      treeIter' <- display textBuffer treeStore treeIter s "IfStatement" False
      display textBuffer treeStore treeIter' ifKeyword "Token" True
      displayExpression textBuffer treeStore treeIter' predicate
      go treeIter' trueBranch
      pure treeIter'

    go treeIter s @ Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} = do
      treeIter' <- display textBuffer treeStore treeIter s "IfElseStatement" False
      display textBuffer treeStore treeIter' ifKeyword "Token" True
      displayExpression textBuffer treeStore treeIter' predicate
      go treeIter' trueBranch
      display textBuffer treeStore treeIter' elseKeyword "Token" True
      go treeIter' falseBranch
      pure treeIter'

    go treeIter s @ Syntax.WhileStatement {whileKeyword, predicate, body} = do
      treeIter' <- display textBuffer treeStore treeIter s "WhileStatement" False
      display textBuffer treeStore treeIter' whileKeyword "Token" True
      displayExpression textBuffer treeStore treeIter' predicate
      go treeIter' body
      pure treeIter'

    go treeIter s @ Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate, semicolon} = do
      treeIter' <- display textBuffer treeStore treeIter s "DoWhileStatement" False
      display textBuffer treeStore treeIter' doKeyword "Token" True
      go treeIter' body
      display textBuffer treeStore treeIter' whileKeyword "Token" True
      displayExpression textBuffer treeStore treeIter' predicate
      display textBuffer treeStore treeIter' semicolon "Token" True
      pure treeIter'

    go treeIter s @ Syntax.ReturnStatement {returnKeyword, result, semicolon} = do
      treeIter' <- display textBuffer treeStore treeIter s "ReturnStatement" False
      display textBuffer treeStore treeIter' returnKeyword "Token" True
      for_ result (displayExpression textBuffer treeStore treeIter')
      display textBuffer treeStore treeIter' semicolon "Token" True
      pure treeIter'

    go treeIter s @ Syntax.BlockStatement {open, elements, close} = do
      treeIter' <- display textBuffer treeStore treeIter s "BlockStatement" False
      display textBuffer treeStore treeIter' open "Token" True
      for_ elements (displayElement treeIter')
      display textBuffer treeStore treeIter' close "Token" True
      pure treeIter'

    displayElement treeIter (Left declaration) = displayDeclaration textBuffer treeStore treeIter declaration
    displayElement treeIter (Right statement) = go treeIter statement


displayExpression ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression c -> IO (Maybe Gtk.TreeIter)
displayExpression isTextBuffer isTreeStore = go
  where
    textBuffer = isTextBuffer `asA` Gtk.TextBuffer
    treeStore = isTreeStore `asA` Gtk.TreeStore

    go treeIter e @ Syntax.LiteralExpression {literal} = do
      treeIter' <- display textBuffer treeStore treeIter e "IntegerExpression" False
      displayLiteral textBuffer treeStore treeIter' literal
      pure treeIter'

    go treeIter e @ Syntax.VariableExpression {variableId} = do
      treeIter' <- display textBuffer treeStore treeIter e "VariableExpression" False
      display textBuffer treeStore treeIter' variableId "Identifier" True
      pure treeIter'

    go treeIter e @ Syntax.CallExpression {targetId, open, arguments, close} = do
      treeIter' <- display textBuffer treeStore treeIter e "CallExpression" False
      display textBuffer treeStore treeIter' targetId "Identifier" True
      display textBuffer treeStore treeIter' open "Token" True
      displayArguments treeIter' arguments
      display textBuffer treeStore treeIter' close "Token" True
      pure treeIter'

    go treeIter e @ Syntax.UnaryExpression {unary, operand} = do
      treeIter' <- display textBuffer treeStore treeIter e "UnaryExpression" False
      displayUnaryOperator textBuffer treeStore treeIter' unary
      go treeIter' operand
      pure treeIter'

    go treeIter e @ Syntax.BinaryExpression {left, binary, right} = do
      treeIter' <- display textBuffer treeStore treeIter e "BinaryExpression" False
      go treeIter' left
      displayBinaryOperator textBuffer treeStore treeIter' binary
      go treeIter' right
      pure treeIter'

    go treeIter e @ Syntax.AssignExpression {targetId, assign, value} = do
      treeIter' <- display textBuffer treeStore treeIter e "AssignExpression" False
      display textBuffer treeStore treeIter' targetId "Identifier" True
      displayAssignOperator textBuffer treeStore treeIter' assign
      go treeIter' value
      pure treeIter'

    go treeIter e @ Syntax.ParenthesizedExpression {open, inner, close} = do
      treeIter' <- display textBuffer treeStore treeIter e "ParenthesizedExpression" False
      display textBuffer treeStore treeIter' open "Token" True
      go treeIter' inner
      display textBuffer treeStore treeIter' close "Token" True
      pure treeIter'

    displayArguments _ Nothing = pure ()

    displayArguments treeIter (Just (first, rest)) = do
      go treeIter first

      for_ rest \(comma, argument) -> do
        display textBuffer treeStore treeIter comma "Token" True
        go treeIter argument


displayUnaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator -> IO (Maybe Gtk.TreeIter)
displayUnaryOperator isTextBuffer isTreeStore treeIter unary =
  display isTextBuffer isTreeStore treeIter unary label True
  where
    label = case unary of
      Syntax.PlusOperator _ -> "PlusOperator"
      Syntax.MinusOperator _ -> "MinusOperator"
      Syntax.NotOperator _ -> "NotOperator"


displayBinaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator -> IO (Maybe Gtk.TreeIter)
displayBinaryOperator isTextBuffer isTreeStore treeIter binary =
  display isTextBuffer isTreeStore treeIter binary label True
  where
    label = case binary of
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
displayAssignOperator isTextBuffer isTreeStore treeIter assign =
  display isTextBuffer isTreeStore treeIter assign label True
  where
    label = case assign of
      Syntax.AssignOperator _ -> "AssignOperator"
      Syntax.AddAssignOperator _ -> "AddAssignOperator"
      Syntax.SubtractAssignOperator _ -> "SubtractAssignOperator"
      Syntax.MultiplyAssignOperator _ -> "MultiplyAssignOperator"
      Syntax.DivideAssignOperator _ -> "DivideAssignOperator"
      Syntax.RemainderAssignOperator _ -> "RemainderAssignOperator"


displayLiteral ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Literal -> IO (Maybe Gtk.TreeIter)
displayLiteral isTextBuffer isTreeStore treeIter = \case
  l @ Syntax.IntegerLiteral {} -> display isTextBuffer isTreeStore treeIter l "IntegerLiteral" True
  l @ Syntax.RationalLiteral {} -> display isTextBuffer isTreeStore treeIter l "RationalLiteral" True


display ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax c) =>
  a -> b -> Maybe Gtk.TreeIter -> c -> Text -> Bool -> IO (Maybe Gtk.TreeIter)
display isTextBuffer isTreeStore treeIter syntax label isLeaf = do
  let textBuffer = isTextBuffer `asA` Gtk.TextBuffer
      treeStore = isTreeStore `asA` Gtk.TreeStore

  treeIter' <- #append treeStore treeIter

  if isLeaf then do
    startTextIter <- #getIterAtOffset textBuffer (Syntax.start syntax)
    endTextIter <- #getIterAtOffset textBuffer (Syntax.end syntax)
    slice <- #getSlice textBuffer startTextIter endTextIter True
    #set treeStore treeIter' [0, 1] =<< traverse toGValue [Just label, Just slice]
  else
    #set treeStore treeIter' [0, 1] =<< traverse toGValue [Just label, Nothing]

  pure (Just treeIter')
