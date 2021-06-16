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
onActivate application = do
  let styleIds =
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
            highlightDeclarationParentheses codeTextBuffer insertTextIter declaration

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
    for_ declarations (highlightDeclarationParentheses codeTextBuffer insertTextIter)

  -- Display the UI:

  #showAll window


highlightDeclaration :: Gtk.IsTextBuffer a => a -> Syntax.Declaration b -> IO ()
highlightDeclaration textBuffer = \case
  Syntax.VariableDeclaration {varKeyword, variableId, typeInfo, value} -> do
    highlight textBuffer "keyword" (Syntax.span varKeyword)
    highlight textBuffer "identifier" (Syntax.span variableId)
    maybe (pure ()) (\(_, typeId) -> highlight textBuffer "type" (Syntax.span typeId)) typeInfo
    highlightExpression textBuffer value

  Syntax.FunctionDeclaration {defKeyword, functionId, parameters, returnInfo, body} -> do
    highlight textBuffer "keyword" (Syntax.span defKeyword)
    highlight textBuffer "identifier" (Syntax.span functionId)
    highlightParameters parameters
    maybe (pure ()) (\(_, returnTypeId) -> highlight textBuffer "type" (Syntax.span returnTypeId)) returnInfo
    highlightStatement textBuffer body

  where
    highlightParameters Nothing = pure ()

    highlightParameters (Just ((id, colon, typeId), rest)) =
      for_ ((undefined, id, colon, typeId) : rest) \(_, id, _, typeId) -> do
        highlight textBuffer "identifier" (Syntax.span id)
        highlight textBuffer "type" (Syntax.span typeId)


highlightStatement :: Gtk.IsTextBuffer a => a -> Syntax.Statement b -> IO ()
highlightStatement textBuffer = \case
  Syntax.ExpressionStatement {value} -> highlightExpression textBuffer value

  Syntax.IfStatement {ifKeyword, predicate, trueBranch} -> do
    highlight textBuffer "keyword" (Syntax.span ifKeyword)
    highlightExpression textBuffer predicate
    highlightStatement textBuffer trueBranch

  Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    highlight textBuffer "keyword" (Syntax.span ifKeyword)
    highlightExpression textBuffer predicate
    highlightStatement textBuffer trueBranch
    highlight textBuffer "keyword" (Syntax.span elseKeyword)
    highlightStatement textBuffer falseBranch

  Syntax.WhileStatement {whileKeyword, predicate, body} -> do
    highlight textBuffer "keyword" (Syntax.span whileKeyword)
    highlightExpression textBuffer predicate
    highlightStatement textBuffer body

  Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate} -> do
    highlight textBuffer "keyword" (Syntax.span doKeyword)
    highlightStatement textBuffer body
    highlight textBuffer "keyword" (Syntax.span whileKeyword)
    highlightExpression textBuffer predicate

  Syntax.ReturnStatement {returnKeyword, result} -> do
    highlight textBuffer "keyword" (Syntax.span returnKeyword)
    maybe (pure ()) (highlightExpression textBuffer) result

  Syntax.BlockStatement {elements} -> for_ elements \case
    Left declaration -> highlightDeclaration textBuffer declaration
    Right statement -> highlightStatement textBuffer statement


highlightExpression :: Gtk.IsTextBuffer a => a -> Syntax.Expression b -> IO ()
highlightExpression textBuffer expression = case expression of
  Syntax.IntegerExpression {} -> highlight textBuffer "number" (Syntax.span expression)

  Syntax.RationalExpression {} -> highlight textBuffer "number" (Syntax.span expression)

  Syntax.VariableExpression {variableId} -> highlight textBuffer "identifier" (Syntax.span variableId)

  Syntax.CallExpression {targetId, arguments} -> do
    highlight textBuffer "identifier" (Syntax.span targetId)

    case arguments of
      Nothing -> pure ()
      Just (first, rest) -> for_ ((undefined, first) : rest) (highlightExpression textBuffer . snd)

  Syntax.UnaryExpression {unary, operand} -> do
    highlight textBuffer "operator" (Syntax.span unary)
    highlightExpression textBuffer operand

  Syntax.BinaryExpression {left, binary, right} -> do
    highlightExpression textBuffer left
    highlight textBuffer "operator" (Syntax.span binary)
    highlightExpression textBuffer right

  Syntax.AssignExpression {targetId, assign, value} -> do
    highlight textBuffer "identifier" (Syntax.span targetId)
    highlight textBuffer "operator" (Syntax.span assign)
    highlightExpression textBuffer value

  Syntax.ParenthesizedExpression {inner} -> highlightExpression textBuffer inner


highlightDeclarationParentheses :: Gtk.IsTextBuffer a => a -> Gtk.TextIter -> Syntax.Declaration b -> IO Bool
highlightDeclarationParentheses textBuffer insertTextIter = \case
  Syntax.VariableDeclaration {value} -> highlightExpressionParentheses textBuffer insertTextIter value

  Syntax.FunctionDeclaration {open, close, body} -> Helpers.orM
    [
      highlightParentheses textBuffer insertTextIter open close,
      highlightStatementParentheses textBuffer insertTextIter body
    ]


highlightStatementParentheses :: Gtk.IsTextBuffer a => a -> Gtk.TextIter -> Syntax.Statement b -> IO Bool
highlightStatementParentheses textBuffer insertTextIter = \case
  Syntax.ExpressionStatement {value} -> highlightExpressionParentheses textBuffer insertTextIter value

  Syntax.IfStatement {predicate, trueBranch} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter trueBranch
    ]

  Syntax.IfElseStatement {predicate, trueBranch, falseBranch} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter trueBranch,
      highlightStatementParentheses textBuffer insertTextIter falseBranch
    ]

  Syntax.WhileStatement {predicate, body} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter body
    ]

  Syntax.DoWhileStatement {body, predicate} -> Helpers.orM
    [
      highlightStatementParentheses textBuffer insertTextIter body,
      highlightExpressionParentheses textBuffer insertTextIter predicate
    ]

  Syntax.ReturnStatement {result = Just result} -> highlightExpressionParentheses textBuffer insertTextIter result

  Syntax.ReturnStatement {result = Nothing} -> pure False

  Syntax.BlockStatement {open, elements, close} -> Helpers.orM
    [
      Helpers.anyM highlightElementParentheses elements,
      highlightParentheses textBuffer insertTextIter open close
    ]

  where
    highlightElementParentheses (Left declaration) =
      highlightDeclarationParentheses textBuffer insertTextIter declaration

    highlightElementParentheses (Right statement) = highlightStatementParentheses textBuffer insertTextIter statement


highlightExpressionParentheses :: Gtk.IsTextBuffer a => a -> Gtk.TextIter -> Syntax.Expression b -> IO Bool
highlightExpressionParentheses textBuffer insertTextIter = \case
  Syntax.IntegerExpression {} -> pure False

  Syntax.RationalExpression {} -> pure False

  Syntax.VariableExpression {} -> pure False

  Syntax.CallExpression {open, arguments = Nothing, close} -> highlightParentheses textBuffer insertTextIter open close

  Syntax.CallExpression {open, arguments = Just (first, rest), close} -> Helpers.orM
    [
      Helpers.anyM (highlightExpressionParentheses textBuffer insertTextIter . snd) ((undefined, first) : rest),
      highlightParentheses textBuffer insertTextIter open close
    ]

  Syntax.UnaryExpression {operand} -> highlightExpressionParentheses textBuffer insertTextIter operand

  Syntax.BinaryExpression {left, right} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter left,
      highlightExpressionParentheses textBuffer insertTextIter right
    ]

  Syntax.AssignExpression {value} -> highlightExpressionParentheses textBuffer insertTextIter value

  Syntax.ParenthesizedExpression {open, inner, close} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter inner,
      highlightParentheses textBuffer insertTextIter open close
    ]


highlightParentheses :: (Gtk.IsTextBuffer a, Syntax b, Syntax c) => a -> Gtk.TextIter -> b -> c -> IO Bool
highlightParentheses textBuffer' insertTextIter open close = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer

  openStartTextIter <- #getIterAtOffset textBuffer (Syntax.start open)
  openEndTextIter <- #getIterAtOffset textBuffer (Syntax.end open)

  closeStartTextIter <- #getIterAtOffset textBuffer (Syntax.start close)
  closeEndTextIter <- #getIterAtOffset textBuffer (Syntax.end close)

  applyParenthesisTag <- Helpers.anyM (#equal insertTextIter)
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    #applyTagByName textBuffer "bracket" openStartTextIter openEndTextIter
    #applyTagByName textBuffer "bracket" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlight :: Gtk.IsTextBuffer a => a -> Text -> Span -> IO ()
highlight textBuffer' tagName span = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer
  startTextIter <- #getIterAtOffset textBuffer (Span.start span)
  endTextIter <- #getIterAtOffset textBuffer (Span.end span)
  #applyTagByName textBuffer tagName startTextIter endTextIter


displayDeclaration ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Declaration c -> IO (Maybe Gtk.TreeIter)
displayDeclaration textBuffer treeStore treeIter declaration = case declaration of
  Syntax.VariableDeclaration {varKeyword, variableId, typeInfo, equalSign, value, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter declaration "VariableAssignDeclaration" False
    display textBuffer treeStore treeIter' varKeyword "Token" True
    display textBuffer treeStore treeIter' variableId "Identifier" True
    displayTypeInfo treeIter' typeInfo
    display textBuffer treeStore treeIter' equalSign "Token" True
    displayExpression textBuffer treeStore treeIter' value
    display textBuffer treeStore treeIter' semicolon "Token" True
    pure treeIter'

  Syntax.FunctionDeclaration {defKeyword, functionId, open, parameters, close, returnInfo, body} -> do
    treeIter' <- display textBuffer treeStore treeIter declaration "FunctionDeclaration" False
    display textBuffer treeStore treeIter' defKeyword "Token" True
    display textBuffer treeStore treeIter' functionId "Identifier" True
    display textBuffer treeStore treeIter' open "Token" True
    displayParameters treeIter' parameters
    display textBuffer treeStore treeIter' close "Token" True
    displayResult treeIter' returnInfo
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  where
    displayTypeInfo _ Nothing = pure ()

    displayTypeInfo treeIter (Just (colon, typeId)) = void do
      display textBuffer treeStore treeIter colon "Token" True
      display textBuffer treeStore treeIter typeId "Identifier" True

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
displayStatement textBuffer treeStore treeIter statement = case statement of
  Syntax.ExpressionStatement {value, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "ExpressionStatement" False
    displayExpression textBuffer treeStore treeIter' value
    display textBuffer treeStore treeIter' semicolon "Token" True
    pure treeIter'

  Syntax.IfStatement {ifKeyword, predicate, trueBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "IfStatement" False
    display textBuffer treeStore treeIter' ifKeyword "Token" True
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    pure treeIter'

  Syntax.IfElseStatement {ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "IfElseStatement" False
    display textBuffer treeStore treeIter' ifKeyword "Token" True
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    display textBuffer treeStore treeIter' elseKeyword "Token" True
    displayStatement textBuffer treeStore treeIter' falseBranch
    pure treeIter'

  Syntax.WhileStatement {whileKeyword, predicate, body} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "WhileStatement" False
    display textBuffer treeStore treeIter' whileKeyword "Token" True
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  Syntax.DoWhileStatement {doKeyword, body, whileKeyword, predicate, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "DoWhileStatement" False
    display textBuffer treeStore treeIter' doKeyword "Token" True
    displayStatement textBuffer treeStore treeIter' body
    display textBuffer treeStore treeIter' whileKeyword "Token" True
    displayExpression textBuffer treeStore treeIter' predicate
    display textBuffer treeStore treeIter' semicolon "Token" True
    pure treeIter'

  Syntax.ReturnStatement {returnKeyword, result, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "ReturnStatement" False
    display textBuffer treeStore treeIter' returnKeyword "Token" True
    maybe (pure ()) (void . displayExpression textBuffer treeStore treeIter') result
    display textBuffer treeStore treeIter' semicolon "Token" True
    pure treeIter'

  Syntax.BlockStatement {open, elements, close} -> do
    treeIter' <- display textBuffer treeStore treeIter statement "BlockStatement" False
    display textBuffer treeStore treeIter' open "Token" True
    for_ elements (displayElement treeIter')
    display textBuffer treeStore treeIter' close "Token" True
    pure treeIter'

  where
    displayElement treeIter (Left declaration) = displayDeclaration textBuffer treeStore treeIter declaration
    displayElement treeIter (Right statement) = displayStatement textBuffer treeStore treeIter statement


displayExpression ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression c -> IO (Maybe Gtk.TreeIter)
displayExpression textBuffer treeStore treeIter expression = case expression of
  Syntax.IntegerExpression {} -> display textBuffer treeStore treeIter expression "IntegerExpression" True

  Syntax.RationalExpression {} -> display textBuffer treeStore treeIter expression "RationalExpression" True

  Syntax.VariableExpression {variableId} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "VariableExpression" False
    display textBuffer treeStore treeIter' variableId "Identifier" True
    pure treeIter'

  Syntax.CallExpression {targetId, open, arguments, close} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "CallExpression" False
    display textBuffer treeStore treeIter' targetId "Identifier" True
    display textBuffer treeStore treeIter' open "Token" True
    displayArguments treeIter' arguments
    display textBuffer treeStore treeIter' close "Token" True
    pure treeIter'

  Syntax.UnaryExpression {unary, operand} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "UnaryExpression" False
    displayUnaryOperator textBuffer treeStore treeIter' unary
    displayExpression textBuffer treeStore treeIter' operand
    pure treeIter'

  Syntax.BinaryExpression {left, binary, right} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "BinaryExpression" False
    displayExpression textBuffer treeStore treeIter' left
    displayBinaryOperator textBuffer treeStore treeIter' binary
    displayExpression textBuffer treeStore treeIter' right
    pure treeIter'

  Syntax.AssignExpression {targetId, assign, value} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "AssignExpression" False
    display textBuffer treeStore treeIter' targetId "Identifier" True
    displayAssignOperator textBuffer treeStore treeIter' assign
    displayExpression textBuffer treeStore treeIter' value
    pure treeIter'

  Syntax.ParenthesizedExpression {open, inner, close} -> do
    treeIter' <- display textBuffer treeStore treeIter expression "ParenthesizedExpression" False
    display textBuffer treeStore treeIter' open "Token" True
    displayExpression textBuffer treeStore treeIter' inner
    display textBuffer treeStore treeIter' close "Token" True
    pure treeIter'

  where
    displayArguments _ Nothing = pure ()

    displayArguments treeIter (Just (first, rest)) = do
      displayExpression textBuffer treeStore treeIter first

      for_ rest \(comma, argument) -> do
        display textBuffer treeStore treeIter comma "Token" True
        displayExpression textBuffer treeStore treeIter argument


displayUnaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator c -> IO (Maybe Gtk.TreeIter)
displayUnaryOperator textBuffer treeStore treeIter unary = case unary of
  Syntax.PlusOperator {} -> display textBuffer treeStore treeIter unary "PlusOperator" True
  Syntax.MinusOperator {} -> display textBuffer treeStore treeIter unary "MinusOperator" True
  Syntax.NotOperator {} -> display textBuffer treeStore treeIter unary "NotOperator" True


displayBinaryOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator c -> IO (Maybe Gtk.TreeIter)
displayBinaryOperator textBuffer treeStore treeIter binary = case binary of
  Syntax.AddOperator {} -> display textBuffer treeStore treeIter binary "AddOperator" True
  Syntax.SubtractOperator {} -> display textBuffer treeStore treeIter binary "SubtractOperator" True
  Syntax.MultiplyOperator {} -> display textBuffer treeStore treeIter binary "MultiplyOperator" True
  Syntax.DivideOperator {} -> display textBuffer treeStore treeIter binary "DivideOperator" True
  Syntax.RemainderOperator {} -> display textBuffer treeStore treeIter binary "RemainderOperator" True
  Syntax.EqualOperator {} -> display textBuffer treeStore treeIter binary "EqualOperator" True
  Syntax.NotEqualOperator {} -> display textBuffer treeStore treeIter binary "NotEqualOperator" True
  Syntax.LessOperator {} -> display textBuffer treeStore treeIter binary "LessOperator" True
  Syntax.LessOrEqualOperator {} -> display textBuffer treeStore treeIter binary "LessOrEqualOperator" True
  Syntax.GreaterOperator {} -> display textBuffer treeStore treeIter binary "GreaterOperator" True
  Syntax.GreaterOrEqualOperator {} -> display textBuffer treeStore treeIter binary "GreaterOrEqualOperator" True
  Syntax.AndOperator {} -> display textBuffer treeStore treeIter binary "AndOperator" True
  Syntax.OrOperator {} -> display textBuffer treeStore treeIter binary "OrOperator" True


displayAssignOperator ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) =>
  a -> b -> Maybe Gtk.TreeIter -> Syntax.AssignOperator c -> IO (Maybe Gtk.TreeIter)
displayAssignOperator textBuffer treeStore treeIter assign = case assign of
  Syntax.AssignOperator {} -> display textBuffer treeStore treeIter assign "AssignOperator" True
  Syntax.AddAssignOperator {} -> display textBuffer treeStore treeIter assign "AddAssignOperator" True
  Syntax.SubtractAssignOperator {} -> display textBuffer treeStore treeIter assign "SubtractAssignOperator" True
  Syntax.MultiplyAssignOperator {} -> display textBuffer treeStore treeIter assign "MultiplyAssignOperator" True
  Syntax.DivideAssignOperator {} -> display textBuffer treeStore treeIter assign "DivideAssignOperator" True
  Syntax.RemainderAssignOperator {} -> display textBuffer treeStore treeIter assign "RemainderAssignOperator" True


display ::
  (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax c) =>
  a -> b -> Maybe Gtk.TreeIter -> c -> Text -> Bool -> IO (Maybe Gtk.TreeIter)
display textBuffer' treeStore' treeIter syntax label isLeaf = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer
      treeStore = treeStore' `asA` Gtk.TreeStore

  treeIter' <- #append treeStore treeIter

  if isLeaf then do
    startTextIter <- #getIterAtOffset textBuffer (Syntax.start syntax)
    endTextIter <- #getIterAtOffset textBuffer (Syntax.end syntax)
    slice <- #getSlice textBuffer startTextIter endTextIter True
    #set treeStore treeIter' [0, 1] =<< traverse toGValue [Just label, Just slice]
  else
    #set treeStore treeIter' [0, 1] =<< traverse toGValue [Just label, Nothing]

  pure (Just treeIter')
