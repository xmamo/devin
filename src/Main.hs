module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
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
import qualified Parser
import Parser.Input (Input (Input))
import qualified Parser.Result as Result
import qualified Parsers
import Span (Span)
import qualified Span
import qualified Syntax
import Type (Type)
import qualified Type
import qualified Typer
import qualified Typer.Environment as Environment
import qualified Typer.Error as Error
import qualified Typers


main :: IO ()
main = Gtk.applicationNew Nothing [] >>= \case
  Just application -> do
    args <- getArgs
    Gio.onApplicationActivate application (onActivate application)
    status <- Gio.applicationRun application (Just args)
    exitWith (if status == 0 then ExitSuccess else ExitFailure (fromIntegral status))

  Nothing -> pure ()


onActivate :: Gtk.IsApplication a => a -> Gio.ApplicationActivateCallback
onActivate application = do
  let noTextTagTable = Nothing :: Maybe Gtk.TextTagTable
  let noAdjustment = Nothing :: Maybe Gtk.Adjustment

  let styles =
        [
          ("bracket", "bracket-match"),
          ("keyword", "def:keyword"),
          ("identifier", "def:identifier"),
          ("type", "def:type"),
          ("number", "def:number"),
          ("operator", "def:operator"),
          ("comment", "def:comment"),
          ("error", "def:error")
        ]

  -- Create defaultLanguage, codeTextBuffer and codeTreeStore, which are needed later:

  languageManager <- GtkSource.languageManagerGetDefault
  defaultLanguage <- GtkSource.languageManagerGetLanguage languageManager "def"

  codeTextBuffer <- GtkSource.bufferNew noTextTagTable
  GtkSource.bufferSetHighlightMatchingBrackets codeTextBuffer False
  GtkSource.bufferSetHighlightSyntax codeTextBuffer False

  codeTreeStore <- Gtk.treeStoreNew (replicate 3 gtypeString)

  -- Build the UI:

  codeSourceView <- GtkSource.viewNewWithBuffer codeTextBuffer
  Gtk.textViewSetMonospace codeSourceView True
  GtkSource.viewSetAutoIndent codeSourceView True
  GtkSource.viewSetHighlightCurrentLine codeSourceView True
  GtkSource.viewSetShowLineNumbers codeSourceView True
  GtkSource.viewSetTabWidth codeSourceView 4

  codeTreeView <- Gtk.treeViewNewWithModel codeTreeStore
  Gtk.treeViewSetGridLines codeTreeView Gtk.TreeViewGridLinesVertical
  Gtk.treeViewSetEnableSearch codeTreeView False
  Gtk.treeViewSetHeadersVisible codeTreeView False

  logTextView <- Gtk.textViewNew
  Gtk.textViewSetEditable logTextView False
  Gtk.textViewSetMonospace logTextView True
  Gtk.textViewSetWrapMode logTextView Gtk.WrapModeWord

  codeSourceScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd codeSourceScrolledWindow codeSourceView

  codeTreeScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd codeTreeScrolledWindow codeTreeView

  logScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd logScrolledWindow logTextView

  paned1 <- Gtk.panedNew Gtk.OrientationHorizontal
  Gtk.panedPack1 paned1 codeSourceScrolledWindow True False
  Gtk.panedPack2 paned1 codeTreeScrolledWindow True False

  paned2 <- Gtk.panedNew Gtk.OrientationVertical
  Gtk.panedPack1 paned2 paned1 True False
  Gtk.panedPack2 paned2 logScrolledWindow False False

  window <- Gtk.applicationWindowNew application
  Gtk.windowSetTitle window ""
  Gtk.windowSetDefaultSize window 1280 720
  Gtk.containerAdd window paned2

  -- Set up codeSourceView:

  styleScheme <- GtkSource.bufferGetStyleScheme codeTextBuffer
  tagTable <- Gtk.textBufferGetTagTable codeTextBuffer

  for_ styles \(tagName, styleId) -> do
    tag <- GtkSource.tagNew (Just tagName)
    Gtk.textTagTableAdd tagTable tag

    style <- case (styleScheme, defaultLanguage) of
      (Just styleScheme, Just language) -> Helpers.getStyle language styleScheme styleId
      (Just styleScheme, Nothing) -> GtkSource.styleSchemeGetStyle styleScheme styleId
      (Nothing, _) -> pure Nothing

    case style of
      Just style -> GtkSource.styleApply style tag
      Nothing -> pure ()

  -- Set up codeTreeView:

  treeSelection <- Gtk.treeViewGetSelection codeTreeView
  Gtk.treeSelectionSetMode treeSelection Gtk.SelectionModeNone

  cellRenderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily cellRenderer "monospace"

  for_ [0 .. 2] \column -> do
    treeViewColumn <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnPackStart treeViewColumn cellRenderer False
    Gtk.treeViewColumnAddAttribute treeViewColumn cellRenderer "text" column
    Gtk.treeViewAppendColumn codeTreeView treeViewColumn

  -- Set up logTextView:

  logTextBuffer <- Gtk.textViewGetBuffer logTextView

  -- Register listeners:

  parseThreadIdMVar <- newMVar =<< forkIO (pure ())
  declarationsVar <- newMVar []

  Gtk.onTextBufferChanged codeTextBuffer do
    (startTextIter, endTextIter) <- Gtk.textBufferGetBounds codeTextBuffer
    text <- Gtk.textBufferGetText codeTextBuffer startTextIter endTextIter True

    killThread =<< takeMVar parseThreadIdMVar
    swapMVar declarationsVar []

    putMVar parseThreadIdMVar =<< forkIO case Parser.run Parsers.declarations (Input 0 text) of
      Result.Success (declarations, comments) _ -> do
        swapMVar declarationsVar declarations

        Gtk.postGUIASync do
          (startTextIter, endTextIter) <- Gtk.textBufferGetBounds codeTextBuffer
          insertTextIter <- Helpers.getInsertTextIter codeTextBuffer

          for_ styles \(tagName, _) ->
            Gtk.textBufferRemoveTagByName codeTextBuffer tagName startTextIter endTextIter

          for_ declarations \declaration -> do
            highlightDeclaration codeTextBuffer declaration
            highlightDeclarationParentheses codeTextBuffer insertTextIter declaration

          for_ comments $
            highlight codeTextBuffer "comment"

          Gtk.textBufferSetText logTextBuffer "" 0

        let checker = Typers.checkDeclarations declarations
        let (declarations', _, errors) = Typer.run checker Environment.predefined

        Gtk.postGUIASync do
          (startTextIter, endTextIter) <- Gtk.textBufferGetBounds codeTextBuffer
          Gtk.textBufferRemoveTagByName codeTextBuffer "error" startTextIter endTextIter

          Gtk.treeStoreClear codeTreeStore
          for_ declarations' (displayDeclaration codeTextBuffer codeTreeStore Nothing)
          Gtk.treeViewExpandAll codeTreeView

          logEntries <- for errors \error -> do
            highlight codeTextBuffer "error" error

            startTextIter <- Gtk.textBufferGetIterAtOffset codeTextBuffer (Span.start error)
            (line, column) <- Helpers.getLineColumn startTextIter
            let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
            pure (prefix <> Error.description error)

          let log = Text.intercalate "\n" logEntries
          Gtk.textBufferSetText logTextBuffer log (Helpers.utf8Length log)

      Result.Failure _ position expectations -> Gtk.postGUIASync do
        (startTextIter, endTextIter) <- Gtk.textBufferGetBounds codeTextBuffer
        Gtk.textBufferRemoveTagByName codeTextBuffer "error" startTextIter endTextIter

        startTextIter <- Gtk.textBufferGetIterAtOffset codeTextBuffer (fromIntegral position)

        for_ styles \(tagName, _) ->
          Gtk.textBufferRemoveTagByName codeTextBuffer tagName startTextIter endTextIter

        Gtk.textBufferApplyTagByName codeTextBuffer "error" startTextIter endTextIter

        (line, column) <- Helpers.getLineColumn startTextIter
        let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
        let log = prefix <> Helpers.expectationsText expectations
        Gtk.textBufferSetText logTextBuffer log (Helpers.utf8Length log)

  on codeTextBuffer (PropertyNotify Gtk.textBufferCursorPosition) $ const do
    (startTextIter, endTextIter) <- Gtk.textBufferGetBounds codeTextBuffer
    Gtk.textBufferRemoveTagByName codeTextBuffer "bracket" startTextIter endTextIter

    declarations <- readMVar declarationsVar
    insertTextIter <- Helpers.getInsertTextIter codeTextBuffer
    for_ declarations (highlightDeclarationParentheses codeTextBuffer insertTextIter)

  -- Display the UI:

  Gtk.widgetShowAll window


highlightDeclaration :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Declaration -> m ()
highlightDeclaration textBuffer = \case
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo, value} -> do
    highlight textBuffer "keyword" varKeyword
    highlight textBuffer "identifier" variableId
    maybe (pure ()) (highlight textBuffer "type" . snd) typeInfo
    highlightExpression textBuffer value

  Syntax.FunctionDeclaration{defKeyword, functionId, parameters, returnInfo, body} -> do
    highlight textBuffer "keyword" defKeyword
    highlight textBuffer "identifier" functionId
    for_ parameters \(id, _, typeId) -> highlight textBuffer "identifier" id *> highlight textBuffer "type" typeId
    maybe (pure ()) (highlight textBuffer "type" . snd) returnInfo
    highlightStatement textBuffer body


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> m ()
highlightStatement textBuffer = \case
  Syntax.ExpressionStatement{expression} -> highlightExpression textBuffer expression

  Syntax.IfStatement{ifKeyword, predicate, trueBranch} -> do
    highlight textBuffer "keyword" ifKeyword
    highlightExpression textBuffer predicate
    highlightStatement textBuffer trueBranch

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    highlight textBuffer "keyword" ifKeyword
    highlightExpression textBuffer predicate
    highlightStatement textBuffer trueBranch
    highlight textBuffer "keyword" elseKeyword
    highlightStatement textBuffer falseBranch

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    highlight textBuffer "keyword" whileKeyword
    highlightExpression textBuffer predicate
    highlightStatement textBuffer body

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate} -> do
    highlight textBuffer "keyword" doKeyword
    highlightStatement textBuffer body
    highlight textBuffer "keyword" whileKeyword
    highlightExpression textBuffer predicate

  Syntax.ReturnStatement{returnKeyword, result} -> do
    highlight textBuffer "keyword" returnKeyword
    maybe (pure ()) (highlightExpression textBuffer) result

  Syntax.BlockStatement{elements} ->
    for_ elements (either (highlightDeclaration textBuffer) (highlightStatement textBuffer))


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Expression -> m ()
highlightExpression textBuffer expression = case expression of
  Syntax.IntegerExpression{} ->
    highlight textBuffer "number" expression

  Syntax.RationalExpression{} ->
    highlight textBuffer "number" expression

  Syntax.VariableExpression{variableId} ->
    highlight textBuffer "identifier" variableId

  Syntax.CallExpression{targetId, arguments} -> do
    highlight textBuffer "identifier" targetId
    for_ arguments (highlightExpression textBuffer)

  Syntax.UnaryExpression{unary, operand} -> do
    highlight textBuffer "operator" unary
    highlightExpression textBuffer operand

  Syntax.BinaryExpression{left, binary, right} -> do
    highlightExpression textBuffer left
    highlight textBuffer "operator" binary
    highlightExpression textBuffer right

  Syntax.AssignExpression{variableId, assign, value} -> do
    highlight textBuffer "identifier" variableId
    highlight textBuffer "operator" assign
    highlightExpression textBuffer value

  Syntax.ParenthesizedExpression{inner} ->
    highlightExpression textBuffer inner


highlightDeclarationParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Syntax.Declaration -> m Bool
highlightDeclarationParentheses textBuffer insertTextIter = \case
  Syntax.VariableDeclaration{value} ->
    highlightExpressionParentheses textBuffer insertTextIter value

  Syntax.FunctionDeclaration{open, close, body} -> Helpers.orM
    [
      highlightParentheses textBuffer insertTextIter open close,
      highlightStatementParentheses textBuffer insertTextIter body
    ]


highlightStatementParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Syntax.Statement -> m Bool
highlightStatementParentheses textBuffer insertTextIter = \case
  Syntax.ExpressionStatement{expression} ->
    highlightExpressionParentheses textBuffer insertTextIter expression

  Syntax.IfStatement{predicate, trueBranch} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter trueBranch
    ]

  Syntax.IfElseStatement{predicate, trueBranch, falseBranch} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter trueBranch,
      highlightStatementParentheses textBuffer insertTextIter falseBranch
    ]

  Syntax.WhileStatement{predicate, body} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter predicate,
      highlightStatementParentheses textBuffer insertTextIter body
    ]

  Syntax.DoWhileStatement{body, predicate} -> Helpers.orM
    [
      highlightStatementParentheses textBuffer insertTextIter body,
      highlightExpressionParentheses textBuffer insertTextIter predicate
    ]

  Syntax.ReturnStatement{result = Nothing} -> pure False

  Syntax.ReturnStatement{result = Just result} ->
    highlightExpressionParentheses textBuffer insertTextIter result

  Syntax.BlockStatement{open, elements, close} -> Helpers.orM
    [
      Helpers.anyM highlightElementParentheses elements,
      highlightParentheses textBuffer insertTextIter open close
    ]

  where
    highlightElementParentheses = \case
      Left declaration -> highlightDeclarationParentheses textBuffer insertTextIter declaration
      Right statement -> highlightStatementParentheses textBuffer insertTextIter statement


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Syntax.Expression -> m Bool
highlightExpressionParentheses textBuffer insertTextIter = \case
  Syntax.IntegerExpression{} -> pure False

  Syntax.RationalExpression{} -> pure False

  Syntax.VariableExpression{} -> pure False

  Syntax.CallExpression{open, arguments, close} -> Helpers.orM
    [
      Helpers.anyM (highlightExpressionParentheses textBuffer insertTextIter) arguments,
      highlightParentheses textBuffer insertTextIter open close
    ]

  Syntax.UnaryExpression{operand} ->
    highlightExpressionParentheses textBuffer insertTextIter operand

  Syntax.BinaryExpression{left, right} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter left,
      highlightExpressionParentheses textBuffer insertTextIter right
    ]

  Syntax.AssignExpression{value} ->
    highlightExpressionParentheses textBuffer insertTextIter value

  Syntax.ParenthesizedExpression{open, inner, close} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter inner,
      highlightParentheses textBuffer insertTextIter open close
    ]


highlightParentheses :: (Gtk.IsTextBuffer a, Span b, Span c, MonadIO m) => a -> Gtk.TextIter -> b -> c -> m Bool
highlightParentheses textBuffer insertTextIter open close = do
  openStartTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.start open)
  openEndTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.end open)

  closeStartTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.start close)
  closeEndTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.end close)

  applyParenthesisTag <- Helpers.anyM (Gtk.textIterEqual insertTextIter)
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    Gtk.textBufferApplyTagByName textBuffer "bracket" openStartTextIter openEndTextIter
    Gtk.textBufferApplyTagByName textBuffer "bracket" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlight :: (Gtk.IsTextBuffer a, Span b, MonadIO m) => a -> Text -> b -> m ()
highlight textBuffer tagName span = do
  startTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.start span)
  endTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.end span)
  Gtk.textBufferApplyTagByName textBuffer tagName startTextIter endTextIter


displayDeclaration :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Declaration -> IO (Maybe Gtk.TreeIter)
displayDeclaration textBuffer treeStore treeIter declaration = case declaration of
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo, equalSign, value, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing declaration
    display textBuffer treeStore treeIter' Nothing varKeyword
    display textBuffer treeStore treeIter' (Just variableId.t) variableId
    displayTypeInfo treeIter' typeInfo
    display textBuffer treeStore treeIter' Nothing equalSign
    displayExpression textBuffer treeStore treeIter' value
    display textBuffer treeStore treeIter' Nothing semicolon
    pure treeIter'

  Syntax.FunctionDeclaration{defKeyword, functionId, open, parameters, commas, close, returnInfo, body} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing declaration
    display textBuffer treeStore treeIter' Nothing defKeyword
    display textBuffer treeStore treeIter' (Just functionId.t) functionId
    display textBuffer treeStore treeIter' Nothing open
    displayParametersAndCommas treeIter' parameters commas
    display textBuffer treeStore treeIter' Nothing close
    displayReturnInfo treeIter' returnInfo
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  where
    displayTypeInfo treeIter (Just (colon, typeId)) = void do
      display textBuffer treeStore treeIter Nothing colon
      display textBuffer treeStore treeIter Nothing typeId

    displayTypeInfo _ Nothing = pure ()

    displayParametersAndCommas treeIter parameters [] =
      for_ parameters (displayParameter treeIter)

    displayParametersAndCommas treeIter [] commas =
      for_ commas (display textBuffer treeStore treeIter Nothing)

    displayParametersAndCommas treeIter (parameter : parameters) (comma : commas) = do
      displayParameter treeIter parameter
      display textBuffer treeStore treeIter Nothing comma
      displayParametersAndCommas treeIter parameters commas

    displayParameter treeIter (id, colon, typeId) = do
      display textBuffer treeStore treeIter (Just id.t) id
      display textBuffer treeStore treeIter Nothing colon
      display textBuffer treeStore treeIter Nothing typeId

    displayReturnInfo treeIter (Just (arrow, typeId)) = void do
      display textBuffer treeStore treeIter Nothing arrow
      display textBuffer treeStore treeIter Nothing typeId

    displayReturnInfo _ Nothing = pure ()


displayStatement :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Statement -> IO (Maybe Gtk.TreeIter)
displayStatement textBuffer treeStore treeIter statement = case statement of
  Syntax.ExpressionStatement{expression, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    displayExpression textBuffer treeStore treeIter' expression
    display textBuffer treeStore treeIter' Nothing semicolon
    pure treeIter'

  Syntax.IfStatement{ifKeyword, predicate, trueBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing ifKeyword
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    pure treeIter'

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing ifKeyword
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    display textBuffer treeStore treeIter' Nothing elseKeyword
    displayStatement textBuffer treeStore treeIter' falseBranch
    pure treeIter'

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing whileKeyword
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing doKeyword
    displayStatement textBuffer treeStore treeIter' body
    display textBuffer treeStore treeIter' Nothing whileKeyword
    displayExpression textBuffer treeStore treeIter' predicate
    display textBuffer treeStore treeIter' Nothing semicolon
    pure treeIter'

  Syntax.ReturnStatement{returnKeyword, result, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing returnKeyword
    maybe (pure Nothing) (displayExpression textBuffer treeStore treeIter') result
    display textBuffer treeStore treeIter' Nothing semicolon
    pure treeIter'

  Syntax.BlockStatement{open, elements, close} -> do
    treeIter' <- display textBuffer treeStore treeIter Nothing statement
    display textBuffer treeStore treeIter' Nothing open
    for_ elements (displayElement treeIter')
    display textBuffer treeStore treeIter' Nothing close
    pure treeIter'

  where
    displayElement treeIter = \case
      Left declaration -> displayDeclaration textBuffer treeStore treeIter declaration
      Right statement -> displayStatement textBuffer treeStore treeIter statement


displayExpression :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression -> IO (Maybe Gtk.TreeIter)
displayExpression textBuffer treeStore treeIter expression = case expression of
  Syntax.IntegerExpression{t} ->
    display textBuffer treeStore treeIter (Just t) expression

  Syntax.RationalExpression{t} ->
    display textBuffer treeStore treeIter (Just t) expression

  Syntax.VariableExpression{variableId, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    display textBuffer treeStore treeIter' (Just variableId.t) variableId
    pure treeIter'

  Syntax.CallExpression{targetId, open, arguments, commas, close, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    display textBuffer treeStore treeIter' (Just targetId.t) targetId
    display textBuffer treeStore treeIter' Nothing open
    displayArgumentsAndCommas treeIter' arguments commas
    display textBuffer treeStore treeIter' Nothing close
    pure treeIter'

  Syntax.UnaryExpression{unary, operand, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    displayUnaryOperator textBuffer treeStore treeIter' unary
    displayExpression textBuffer treeStore treeIter' operand
    pure treeIter'

  Syntax.BinaryExpression{left, binary, right, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    displayExpression textBuffer treeStore treeIter' left
    displayBinaryOperator textBuffer treeStore treeIter' binary
    displayExpression textBuffer treeStore treeIter' right
    pure treeIter'

  Syntax.AssignExpression{variableId, assign, value, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    display textBuffer treeStore treeIter' (Just variableId.t) variableId
    displayAssignOperator textBuffer treeStore treeIter' assign
    displayExpression textBuffer treeStore treeIter' value
    pure treeIter'

  Syntax.ParenthesizedExpression{open, inner, close, t} -> do
    treeIter' <- display textBuffer treeStore treeIter (Just t) expression
    display textBuffer treeStore treeIter' Nothing open
    displayExpression textBuffer treeStore treeIter' inner
    display textBuffer treeStore treeIter' Nothing close
    pure treeIter'

  where
    displayArgumentsAndCommas treeIter arguments [] =
      for_ arguments (displayExpression textBuffer treeStore treeIter)

    displayArgumentsAndCommas treeIter [] commas =
      for_ commas (display textBuffer treeStore treeIter Nothing)

    displayArgumentsAndCommas treeIter (argument : arguments) (comma : commas) = do
      displayExpression textBuffer treeStore treeIter argument
      display textBuffer treeStore treeIter Nothing comma
      displayArgumentsAndCommas treeIter arguments commas


displayUnaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator -> IO (Maybe Gtk.TreeIter)
displayUnaryOperator textBuffer treeStore treeIter unary =
  display textBuffer treeStore treeIter (Just unary.t) unary


displayBinaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator -> IO (Maybe Gtk.TreeIter)
displayBinaryOperator textBuffer treeStore treeIter binary =
  display textBuffer treeStore treeIter (Just binary.t) binary


displayAssignOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.AssignOperator -> IO (Maybe Gtk.TreeIter)
displayAssignOperator textBuffer treeStore treeIter assign =
  display textBuffer treeStore treeIter (Just assign.t) assign


display :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax.Node c) => a -> b -> Maybe Gtk.TreeIter -> Maybe Type -> c -> IO (Maybe Gtk.TreeIter)
display textBuffer treeStore treeIter t node = do
  let label = Just (Syntax.label node)
  let typeId = Type.label <$> t

  treeIter' <- Gtk.treeStoreAppend treeStore treeIter

  if Syntax.isLeaf node then do
    startTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.start node)
    endTextIter <- Gtk.textBufferGetIterAtOffset textBuffer (Span.end node)
    slice <- Gtk.textBufferGetSlice textBuffer startTextIter endTextIter True
    Gtk.treeStoreSet treeStore treeIter' [0, 1, 2] =<< for [label, Just slice, typeId] toGValue
  else
    Gtk.treeStoreSet treeStore treeIter' [0, 1, 2] =<< for [label, Nothing, typeId] toGValue

  pure (Just treeIter')
