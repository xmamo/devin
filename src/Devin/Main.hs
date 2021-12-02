module Devin.Main (main) where

import Control.Concurrent
import Data.Foldable
import Data.Traversable
import System.Environment
import System.Exit
import Control.Monad.IO.Class

import Control.Monad.Extra
import Data.List.Extra

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Map as Map

import Data.GI.Base
import Data.GI.Base.GType
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.GtkSource as GtkSource

import Devin.Helpers
import qualified Devin.Parser as Parser
import Devin.Parser.Input
import Devin.Parser.Result
import qualified Devin.Parsers as Parsers
import Devin.Range
import Devin.Syntax
import Devin.Type (Type)
import qualified Devin.Type as Type
import Devin.Typer
import qualified Devin.Typer.Environment as Environment
import qualified Devin.Typer.Error as Error
import Devin.Typers as Typers
import Devin.Evaluator
import Devin.Evaluator.Result
import Devin.Evaluator.State (State)
import qualified Devin.Evaluator.State as State
import Devin.Evaluators
import qualified Devin.Value as Value


main :: IO ()
main = whenJustM (Gtk.applicationNew Nothing []) $ \application -> do
  args <- getArgs
  Gio.onApplicationActivate application (onActivate application)
  status <- Gio.applicationRun application (Just args)
  exitWith (if status == 0 then ExitSuccess else ExitFailure (fromIntegral status))


onActivate :: Gtk.IsApplication a => a -> Gio.ApplicationActivateCallback
onActivate application = do
  let noTable = Nothing :: Maybe Gtk.TextTagTable
  let noAdjustment = Nothing :: Maybe Gtk.Adjustment

  let styles =
        [
          ("highlight", "search-match"),
          ("bracket", "bracket-match"),
          ("keyword", "def:keyword"),
          ("identifier", "def:identifier"),
          ("type", "def:type"),
          ("number", "def:number"),
          ("operator", "def:operator"),
          ("comment", "def:comment"),
          ("error", "def:error")
        ]

  -- Create defaultLanguage, codeBuffer, codeTreeStore, stateListStore and
  -- cellRenderer, which are needed later:

  languageManager <- GtkSource.languageManagerGetDefault
  defaultLanguage <- GtkSource.languageManagerGetLanguage languageManager "def"

  codeBuffer <- GtkSource.bufferNew noTable
  GtkSource.bufferSetHighlightMatchingBrackets codeBuffer False
  GtkSource.bufferSetHighlightSyntax codeBuffer False

  codeTreeStore <- Gtk.treeStoreNew [gtypeString, gtypeString, gtypeString]
  stateListStore <- Gtk.listStoreNew [gtypeString, gtypeString]

  cellRenderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily cellRenderer "monospace"

  -- Build the UI:

  playButton <- Gtk.buttonNewFromIconName (Just "media-playback-start") 1
  stepOverButton <- Gtk.buttonNewFromIconName (Just "go-next") 1
  stepIntoButton <- Gtk.buttonNewFromIconName (Just "go-down") 1

  actionBar <- Gtk.actionBarNew
  Gtk.actionBarPackStart actionBar playButton
  Gtk.actionBarPackStart actionBar stepOverButton
  Gtk.actionBarPackStart actionBar stepIntoButton

  codeSourceView <- GtkSource.viewNewWithBuffer codeBuffer
  Gtk.textViewSetMonospace codeSourceView True
  GtkSource.viewSetAutoIndent codeSourceView True
  GtkSource.viewSetHighlightCurrentLine codeSourceView True
  GtkSource.viewSetShowLineNumbers codeSourceView True
  GtkSource.viewSetTabWidth codeSourceView 4

  codeTreeView <- Gtk.treeViewNewWithModel codeTreeStore
  Gtk.treeViewSetGridLines codeTreeView Gtk.TreeViewGridLinesVertical
  Gtk.treeViewSetEnableSearch codeTreeView False
  Gtk.treeViewSetHeadersVisible codeTreeView False

  stateTreeView <- Gtk.treeViewNewWithModel stateListStore
  Gtk.treeViewSetGridLines stateTreeView Gtk.TreeViewGridLinesVertical
  Gtk.treeViewSetEnableSearch stateTreeView False
  Gtk.treeViewSetHeadersVisible stateTreeView False

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

  box <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.boxPackStart box actionBar False False 0
  Gtk.boxPackStart box paned2 True True 0

  window <- Gtk.applicationWindowNew application
  Gtk.windowSetTitle window ""
  Gtk.windowSetDefaultSize window 1280 720
  Gtk.containerAdd window box

  -- Set up codeSourceView:

  scheme <- GtkSource.bufferGetStyleScheme codeBuffer
  tagTable <- Gtk.textBufferGetTagTable codeBuffer

  for_ styles $ \(tagName, styleId) -> do
    tag <- GtkSource.tagNew (Just tagName)
    Gtk.textTagTableAdd tagTable tag

    style <- case (scheme, defaultLanguage) of
      (Just scheme, Just language) -> getStyle language scheme styleId
      (Just scheme, Nothing) -> GtkSource.styleSchemeGetStyle scheme styleId
      (Nothing, _) -> pure Nothing

    whenJust style $ \style ->
      GtkSource.styleApply style tag

  -- Set up codeTreeView:

  codeSelection <- Gtk.treeViewGetSelection codeTreeView
  Gtk.treeSelectionSetMode codeSelection Gtk.SelectionModeNone

  for_ [0 .. 2] $ \column -> do
    treeColumn <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnPackStart treeColumn cellRenderer False
    Gtk.treeViewColumnAddAttribute treeColumn cellRenderer "text" column
    Gtk.treeViewAppendColumn codeTreeView treeColumn

  -- Set up stateTreeView:

  stateSelection <- Gtk.treeViewGetSelection stateTreeView
  Gtk.treeSelectionSetMode stateSelection Gtk.SelectionModeNone

  for_ [0 .. 1] $ \column -> do
    treeColumn <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnPackStart treeColumn cellRenderer False
    Gtk.treeViewColumnAddAttribute treeColumn cellRenderer "text" column
    Gtk.treeViewAppendColumn stateTreeView treeColumn

  -- Set up logTextView:

  logBuffer <- Gtk.textViewGetBuffer logTextView

  -- Register listeners:

  parseThreadIdVar <- newMVar =<< forkIO (pure ())
  devinVar <- newEmptyMVar
  devinVar' <- newEmptyMVar

  Gtk.onButtonClicked playButton $ void $ do
    Gtk.widgetSetSensitive playButton False
    devin' <- readMVar devinVar'
    Gtk.containerRemove codeTreeScrolledWindow codeTreeView
    Gtk.containerAdd codeTreeScrolledWindow stateTreeView
    Gtk.widgetShow stateTreeView

    let go evaluator state = do
          (result, state') <- runEvaluator evaluator devin' state

          case result of
            Done _ -> Gtk.postGUIASync $ do
              Gtk.containerRemove codeTreeScrolledWindow stateTreeView
              Gtk.containerAdd codeTreeScrolledWindow codeTreeView
              Gtk.widgetShow codeTreeView

            Step node evaluator' -> do
              Gtk.postGUIASync $ do
                (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
                Gtk.textBufferRemoveTagByName codeBuffer "highlight" startIter endIter
                highlightNode codeBuffer "highlight" node

                Gtk.listStoreClear stateListStore
                displayState stateListStore state'

              threadDelay 3000000
              go evaluator' state'

    forkIO (go (evaluateDevin devin') State.predefined)

  Gtk.onTextBufferChanged codeBuffer $ do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    text <- Gtk.textBufferGetText codeBuffer startIter endIter True

    let action = case Parser.run Parsers.devin (Input 0 text) of
          Success (devin, comments) _ -> do
            putMVar devinVar devin

            Gtk.postGUIASync $ do
              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              insertMark <- Gtk.textBufferGetInsert codeBuffer
              insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark

              for_ styles $ \(tagName, _) ->
                Gtk.textBufferRemoveTagByName codeBuffer tagName startIter endIter

              highlightDevin codeBuffer devin
              highlightDevinParentheses codeBuffer insertIter devin
              for_ comments (highlightNode codeBuffer "comment")

              Gtk.textBufferSetText logBuffer "" 0

            let (devin', _, errors) = runTyper (checkDevin devin) Environment.predefined
            tryTakeMVar devinVar'
            putMVar devinVar' devin'

            Gtk.postGUIASync $ do
              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              Gtk.textBufferRemoveTagByName codeBuffer "error" startIter endIter

              Gtk.treeStoreClear codeTreeStore
              displayDevin codeBuffer codeTreeStore Nothing devin'
              Gtk.treeViewExpandAll codeTreeView

              logEntries <- for errors $ \error -> do
                highlightNode codeBuffer "error" error

                startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
                (line, column) <- getLineColumn startIter
                let prefix = "[" <> Text.pack (show line) <> ":" <> Text.pack (show column) <> "] "
                pure (prefix <> Error.description error)

              let log = Text.intercalate "\n" logEntries
              Gtk.textBufferSetText logBuffer log (-1)

          Failure _ position expectations -> Gtk.postGUIASync $ do
            (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
            Gtk.textBufferRemoveTagByName codeBuffer "error" startIter endIter

            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (fromIntegral position)

            for_ styles $ \(tagName, _) ->
              Gtk.textBufferRemoveTagByName codeBuffer tagName startIter endIter

            Gtk.textBufferApplyTagByName codeBuffer "error" startIter endIter

            (line, column) <- getLineColumn startIter
            let prefix = "[" <> Text.pack (show line) <> ":" <> Text.pack (show column) <> "] "
            let log = prefix <> expectationsText expectations
            Gtk.textBufferSetText logBuffer log (-1)

    killThread =<< takeMVar parseThreadIdVar
    tryTakeMVar devinVar
    putMVar parseThreadIdVar =<< forkIO action

  on codeBuffer (PropertyNotify Gtk.textBufferCursorPosition) $ \_ -> do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    Gtk.textBufferRemoveTagByName codeBuffer "bracket" startIter endIter

    whenJustM (tryReadMVar devinVar) $ \devin -> void $ do
      insertMark <- Gtk.textBufferGetInsert codeBuffer
      insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
      highlightDevinParentheses codeBuffer insertIter devin

  -- Display the UI:

  Gtk.widgetShowAll window


highlightDevin :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Devin -> m ()
highlightDevin buffer Devin{declarations} =
  for_ declarations (highlightDeclaration buffer)


highlightDeclaration :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Declaration -> m ()
highlightDeclaration buffer declaration = case declaration of
  VariableDeclaration{varKeyword, variableId, right} -> do
    highlightNode buffer "keyword" varKeyword
    highlightNode buffer "identifier" variableId
    highlightExpression buffer right

  FunctionDeclaration{defKeyword, functionId, parameters, returnInfo, body} -> do
    highlightNode buffer "keyword" defKeyword
    highlightNode buffer "identifier" functionId

    for_ parameters $ \(id, _, typeId) -> do
      highlightNode buffer "identifier" id
      highlightNode buffer "type" typeId

    whenJust returnInfo (highlightNode buffer "type" . snd)
    highlightStatement buffer body


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Statement -> m ()
highlightStatement buffer statement = case statement of
  DeclarationStatement{declaration} ->
    highlightDeclaration buffer declaration

  ExpressionStatement{value} ->
    highlightExpression buffer value

  IfStatement{ifKeyword, predicate, trueBranch} -> do
    highlightNode buffer "keyword" ifKeyword
    highlightExpression buffer predicate
    highlightStatement buffer trueBranch

  IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    highlightNode buffer "keyword" ifKeyword
    highlightExpression buffer predicate
    highlightStatement buffer trueBranch
    highlightNode buffer "keyword" elseKeyword
    highlightStatement buffer falseBranch

  WhileStatement{whileKeyword, predicate, body} -> do
    highlightNode buffer "keyword" whileKeyword
    highlightExpression buffer predicate
    highlightStatement buffer body

  DoWhileStatement{doKeyword, body, whileKeyword, predicate} -> do
    highlightNode buffer "keyword" doKeyword
    highlightStatement buffer body
    highlightNode buffer "keyword" whileKeyword
    highlightExpression buffer predicate

  ReturnStatement{returnKeyword, result} -> do
    highlightNode buffer "keyword" returnKeyword
    whenJust result (highlightExpression buffer)

  BlockStatement{statements} ->
    for_ statements (highlightStatement buffer)


highlightExpression :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Expression -> m ()
highlightExpression buffer expression = case expression of
  IntegerExpression{} ->
    highlightNode buffer "number" expression

  RationalExpression{} ->
    highlightNode buffer "number" expression

  VariableExpression{variableId} ->
    highlightNode buffer "identifier" variableId

  CallExpression{targetId, arguments} -> do
    highlightNode buffer "identifier" targetId
    for_ arguments (highlightExpression buffer)

  UnaryExpression{unary, operand} -> do
    highlightNode buffer "operator" unary
    highlightExpression buffer operand

  BinaryExpression{left, binary, right} -> do
    highlightExpression buffer left
    highlightNode buffer "operator" binary
    highlightExpression buffer right

  AssignExpression{variableId, assign, right} -> do
    highlightNode buffer "identifier" variableId
    highlightNode buffer "operator" assign
    highlightExpression buffer right

  ParenthesizedExpression{inner} ->
    highlightExpression buffer inner


highlightDevinParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Devin -> m Bool
highlightDevinParentheses buffer insertIter Devin{declarations} =
  anyM (highlightDeclarationParentheses buffer insertIter) declarations


highlightDeclarationParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Declaration -> m Bool
highlightDeclarationParentheses buffer insertIter declaration = case declaration of
  VariableDeclaration{right} ->
    highlightExpressionParentheses buffer insertIter right

  FunctionDeclaration{open, close, body} -> orM
    [
      highlightParentheses buffer insertIter open close,
      highlightStatementParentheses buffer insertIter body
    ]


highlightStatementParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Statement -> m Bool
highlightStatementParentheses buffer insertIter statement = case statement of
  ExpressionStatement{value} ->
    highlightExpressionParentheses buffer insertIter value

  DeclarationStatement{declaration} ->
    highlightDeclarationParentheses buffer insertIter declaration

  IfStatement{predicate, trueBranch} -> orM
    [
      highlightExpressionParentheses buffer insertIter predicate,
      highlightStatementParentheses buffer insertIter trueBranch
    ]

  IfElseStatement{predicate, trueBranch, falseBranch} -> orM
    [
      highlightExpressionParentheses buffer insertIter predicate,
      highlightStatementParentheses buffer insertIter trueBranch,
      highlightStatementParentheses buffer insertIter falseBranch
    ]

  WhileStatement{predicate, body} -> orM
    [
      highlightExpressionParentheses buffer insertIter predicate,
      highlightStatementParentheses buffer insertIter body
    ]

  DoWhileStatement{body, predicate} -> orM
    [
      highlightStatementParentheses buffer insertIter body,
      highlightExpressionParentheses buffer insertIter predicate
    ]

  ReturnStatement{result = Nothing} -> pure False

  ReturnStatement{result = Just result} ->
    highlightExpressionParentheses buffer insertIter result

  BlockStatement{open, statements, close} -> orM
    [
      anyM (highlightStatementParentheses buffer insertIter) statements,
      highlightParentheses buffer insertIter open close
    ]


highlightExpressionParentheses :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Gtk.TextIter -> Expression -> m Bool
highlightExpressionParentheses buffer insertIter expression = case expression of
  IntegerExpression{} -> pure False

  RationalExpression{} -> pure False

  VariableExpression{} -> pure False

  CallExpression{open, arguments, close} -> orM
    [
      anyM (highlightExpressionParentheses buffer insertIter) arguments,
      highlightParentheses buffer insertIter open close
    ]

  UnaryExpression{operand} ->
    highlightExpressionParentheses buffer insertIter operand

  BinaryExpression{left, right} -> orM
    [
      highlightExpressionParentheses buffer insertIter left,
      highlightExpressionParentheses buffer insertIter right
    ]

  AssignExpression{right} ->
    highlightExpressionParentheses buffer insertIter right

  ParenthesizedExpression{open, inner, close} -> orM
    [
      highlightExpressionParentheses buffer insertIter inner,
      highlightParentheses buffer insertIter open close
    ]


highlightParentheses :: (Gtk.IsTextBuffer a, Range b, Range c, MonadIO m) => a -> Gtk.TextIter -> b -> c -> m Bool
highlightParentheses buffer insertIter open close = do
  openStartIter <- Gtk.textBufferGetIterAtOffset buffer (start open)
  openEndIter <- Gtk.textBufferGetIterAtOffset buffer (end open)
  closeEndIter <- Gtk.textBufferGetIterAtOffset buffer (end close)
  closeStartIter <- Gtk.textBufferGetIterAtOffset buffer (start close)

  applyParenthesisTag <- anyM (Gtk.textIterEqual insertIter)
    [openStartIter, openEndIter, closeEndIter, closeStartIter]

  if applyParenthesisTag then do
    Gtk.textBufferApplyTagByName buffer "bracket" openStartIter openEndIter
    Gtk.textBufferApplyTagByName buffer "bracket" closeStartIter closeEndIter
    pure True
  else
    pure False


highlightNode :: (Gtk.IsTextBuffer a, Range b, MonadIO m) => a -> Text -> b -> m ()
highlightNode buffer tagName range = do
  startIter <- Gtk.textBufferGetIterAtOffset buffer (start range)
  endIter <- Gtk.textBufferGetIterAtOffset buffer (end range)
  Gtk.textBufferApplyTagByName buffer tagName startIter endIter


displayDevin :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Devin -> IO (Maybe Gtk.TreeIter)
displayDevin buffer treeStore iter devin = do
  iter' <- displayNode buffer treeStore iter Nothing devin
  for_ devin.declarations (displayDeclaration buffer treeStore iter')
  pure iter'


displayDeclaration :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Declaration -> IO (Maybe Gtk.TreeIter)
displayDeclaration buffer treeStore iter declaration = case declaration of
  VariableDeclaration{varKeyword, variableId, equalSign, right, semicolon} -> do
    iter' <- displayNode buffer treeStore iter Nothing declaration
    displayNode buffer treeStore iter' Nothing varKeyword
    displayNode buffer treeStore iter' (Just variableId.t) variableId
    displayNode buffer treeStore iter' Nothing equalSign
    displayExpression buffer treeStore iter' right
    displayNode buffer treeStore iter' Nothing semicolon
    pure iter'

  FunctionDeclaration{defKeyword, functionId, open, parameters, commas, close, returnInfo, body} -> do
    iter' <- displayNode buffer treeStore iter Nothing declaration
    displayNode buffer treeStore iter' Nothing defKeyword
    displayNode buffer treeStore iter' (Just functionId.t) functionId
    displayNode buffer treeStore iter' Nothing open
    displayParametersAndCommas iter' parameters commas
    displayNode buffer treeStore iter' Nothing close
    displayReturnInfo iter' returnInfo
    displayStatement buffer treeStore iter' body
    pure iter'

  where
    displayParametersAndCommas iter parameters [] =
      for_ parameters (displayParameter iter)

    displayParametersAndCommas iter [] commas =
      for_ commas (displayNode buffer treeStore iter Nothing)

    displayParametersAndCommas iter (parameter : parameters) (comma : commas) = do
      displayParameter iter parameter
      displayNode buffer treeStore iter Nothing comma
      displayParametersAndCommas iter parameters commas

    displayParameter iter (id, colon, typeId) = do
      displayNode buffer treeStore iter (Just id.t) id
      displayNode buffer treeStore iter Nothing colon
      displayNode buffer treeStore iter Nothing typeId

    displayReturnInfo iter (Just (arrow, typeId)) = void $ do
      displayNode buffer treeStore iter Nothing arrow
      displayNode buffer treeStore iter Nothing typeId

    displayReturnInfo _ Nothing = pure ()


displayStatement :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Statement -> IO (Maybe Gtk.TreeIter)
displayStatement buffer treeStore iter statement = case statement of
  DeclarationStatement{declaration} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayDeclaration buffer treeStore iter' declaration
    pure iter'

  ExpressionStatement{value, semicolon} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayExpression buffer treeStore iter' value
    displayNode buffer treeStore iter' Nothing semicolon
    pure iter'

  IfStatement{ifKeyword, predicate, trueBranch} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing ifKeyword
    displayExpression buffer treeStore iter' predicate
    displayStatement buffer treeStore iter' trueBranch
    pure iter'

  IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing ifKeyword
    displayExpression buffer treeStore iter' predicate
    displayStatement buffer treeStore iter' trueBranch
    displayNode buffer treeStore iter' Nothing elseKeyword
    displayStatement buffer treeStore iter' falseBranch
    pure iter'

  WhileStatement{whileKeyword, predicate, body} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing whileKeyword
    displayExpression buffer treeStore iter' predicate
    displayStatement buffer treeStore iter' body
    pure iter'

  DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing doKeyword
    displayStatement buffer treeStore iter' body
    displayNode buffer treeStore iter' Nothing whileKeyword
    displayExpression buffer treeStore iter' predicate
    displayNode buffer treeStore iter' Nothing semicolon
    pure iter'

  ReturnStatement{returnKeyword, result, semicolon} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing returnKeyword
    maybe (pure Nothing) (displayExpression buffer treeStore iter') result
    displayNode buffer treeStore iter' Nothing semicolon
    pure iter'

  BlockStatement{open, statements, close} -> do
    iter' <- displayNode buffer treeStore iter Nothing statement
    displayNode buffer treeStore iter' Nothing open
    for_ statements (displayStatement buffer treeStore iter')
    displayNode buffer treeStore iter' Nothing close
    pure iter'


displayExpression :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Expression -> IO (Maybe Gtk.TreeIter)
displayExpression buffer treeStore iter expression = case expression of
  IntegerExpression{t} ->
    displayNode buffer treeStore iter (Just t) expression

  RationalExpression{t} ->
    displayNode buffer treeStore iter (Just t) expression

  VariableExpression{variableId, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayNode buffer treeStore iter' (Just variableId.t) variableId
    pure iter'

  CallExpression{targetId, open, arguments, commas, close, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayNode buffer treeStore iter' (Just targetId.t) targetId
    displayNode buffer treeStore iter' Nothing open
    displayArgumentsAndCommas iter' arguments commas
    displayNode buffer treeStore iter' Nothing close
    pure iter'

  UnaryExpression{unary, operand, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayUnaryOperator buffer treeStore iter' unary
    displayExpression buffer treeStore iter' operand
    pure iter'

  BinaryExpression{left, binary, right, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayExpression buffer treeStore iter' left
    displayBinaryOperator buffer treeStore iter' binary
    displayExpression buffer treeStore iter' right
    pure iter'

  AssignExpression{variableId, assign, right, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayNode buffer treeStore iter' (Just variableId.t) variableId
    displayAssignOperator buffer treeStore iter' assign
    displayExpression buffer treeStore iter' right
    pure iter'

  ParenthesizedExpression{open, inner, close, t} -> do
    iter' <- displayNode buffer treeStore iter (Just t) expression
    displayNode buffer treeStore iter' Nothing open
    displayExpression buffer treeStore iter' inner
    displayNode buffer treeStore iter' Nothing close
    pure iter'

  where
    displayArgumentsAndCommas iter arguments [] =
      for_ arguments (displayExpression buffer treeStore iter)

    displayArgumentsAndCommas iter [] commas =
      for_ commas (displayNode buffer treeStore iter Nothing)

    displayArgumentsAndCommas iter (argument : arguments) (comma : commas) = do
      displayExpression buffer treeStore iter argument
      displayNode buffer treeStore iter Nothing comma
      displayArgumentsAndCommas iter arguments commas


displayUnaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> UnaryOperator -> IO (Maybe Gtk.TreeIter)
displayUnaryOperator buffer treeStore iter unary =
  displayNode buffer treeStore iter (Just unary.t) unary


displayBinaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> BinaryOperator -> IO (Maybe Gtk.TreeIter)
displayBinaryOperator buffer treeStore iter binary =
  displayNode buffer treeStore iter (Just binary.t) binary


displayAssignOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> AssignOperator -> IO (Maybe Gtk.TreeIter)
displayAssignOperator buffer treeStore iter assign =
  displayNode buffer treeStore iter (Just assign.t) assign


displayNode :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Range c, Node c) => a -> b -> Maybe Gtk.TreeIter -> Maybe Type -> c -> IO (Maybe Gtk.TreeIter)
displayNode buffer treeStore iter t node = do
  iter' <- Gtk.treeStoreAppend treeStore iter

  if isLeaf node then do
    startIter <- Gtk.textBufferGetIterAtOffset buffer (start node)
    endIter <- Gtk.textBufferGetIterAtOffset buffer (end node)
    slice <- Gtk.textBufferGetSlice buffer startIter endIter True
    gvalues <- for [Just (label node), Just slice, Type.pretty <$> t] toGValue
    Gtk.treeStoreSet treeStore iter' [0 .. 2] gvalues
  else do
    gvalues <- for [Just (label node), Nothing, Type.pretty <$> t] toGValue
    Gtk.treeStoreSet treeStore iter' [0 .. 2] gvalues

  pure (Just iter')


displayState :: Gtk.IsListStore a => a -> State -> IO ()
displayState listStore state = case state of
  [] -> pure ()

  (_, variables) : parents | Map.null variables ->
    displayState listStore parents

  (_, variables) : parents -> do
    Map.foldMapWithKey f variables

    when (notNull parents) $ do
      let noText = Nothing :: Maybe Text
      iter <- Gtk.listStoreAppend listStore
      gvalues <- for [noText, noText] toGValue
      Gtk.listStoreSet listStore iter [0 .. 1] gvalues

    displayState listStore parents

    where
      f name value = do
        iter <- Gtk.listStoreAppend listStore
        gvalues <- for [Just name, Just (Value.pretty value)] toGValue
        Gtk.listStoreSet listStore iter [0 .. 1] gvalues
