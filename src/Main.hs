module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
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
main = do
  args <- getArgs
  application <- new Gtk.Application []
  on application #activate (onActivate application)
  status <- #run application (Just args)
  exitWith (if status == 0 then ExitSuccess else ExitFailure (fromIntegral status))


onActivate :: Gtk.IsApplication a => a -> Gio.ApplicationActivateCallback
onActivate application = do
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
  defaultLanguage <- #getLanguage languageManager "def"

  codeTextBuffer <- new GtkSource.Buffer
    [
      #highlightMatchingBrackets := False,
      #highlightSyntax := False
    ]

  codeTreeStore <- new Gtk.TreeStore []
  #setColumnTypes codeTreeStore [gtypeString, gtypeString, gtypeString]

  -- Build the UI:

  codeSourceView <- new GtkSource.View
    [
      #buffer := codeTextBuffer,
      #monospace := True,
      #autoIndent := True,
      #highlightCurrentLine := True,
      #showLineNumbers := True,
      #tabWidth := 4
    ]

  codeTreeView <- new Gtk.TreeView
    [
      #enableGridLines := Gtk.TreeViewGridLinesVertical,
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

  styleScheme <- #getStyleScheme codeTextBuffer
  tagTable <- #getTagTable codeTextBuffer

  for_ styles \(tagName, styleId) -> do
    tag <- new GtkSource.Tag [#name := tagName]
    #add tagTable tag

    style <- case (styleScheme, defaultLanguage) of
      (Just styleScheme, Just language) -> Helpers.getStyle language styleScheme styleId
      (Just styleScheme, Nothing) -> #getStyle styleScheme styleId
      (Nothing, _) -> pure Nothing

    case style of
      Just style -> #apply style tag
      Nothing -> pure ()

  -- Set up codeTreeView:

  treeSelection <- #getSelection codeTreeView
  #setMode treeSelection Gtk.SelectionModeNone

  cellRenderer <- new Gtk.CellRendererText [#family := "monospace"]

  for_ [0, 1, 2] \column -> do
    treeViewColumn <- new Gtk.TreeViewColumn []
    #packStart treeViewColumn cellRenderer False
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
        swapMVar declarationsVar declarations

        Gtk.postGUIASync do
          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          insertTextIter <- Helpers.getInsertTextIter codeTextBuffer

          for_ styles \(tagName, _) ->
            #removeTagByName codeTextBuffer tagName startTextIter endTextIter

          for_ declarations \declaration -> do
            highlightDeclaration codeTextBuffer declaration
            highlightDeclarationParentheses codeTextBuffer insertTextIter declaration

          for_ comments $
            highlight codeTextBuffer "comment"

          set logTextBuffer [#text := ""]

        let checker = Typers.checkDeclarations declarations
            (declarations', _, errors) = Typer.run checker Environment.predefined

        Gtk.postGUIASync do
          (startTextIter, endTextIter) <- #getBounds codeTextBuffer
          #removeTagByName codeTextBuffer "error" startTextIter endTextIter

          #clear codeTreeStore
          for_ declarations' (displayDeclaration codeTextBuffer codeTreeStore Nothing)
          #expandAll codeTreeView

          log <- for errors \error -> do
            highlight codeTextBuffer "error" error

            startTextIter <- #getIterAtOffset codeTextBuffer (Span.start error)
            (line, column) <- Helpers.getLineColumn startTextIter
            let prefix = Text.pack ("[" ++ show line ++ ":" ++ show column ++ "] ")
            pure (prefix <> Error.description error)

          set logTextBuffer [#text := Text.intercalate "\n" log]

      Result.Failure _ position expectations -> Gtk.postGUIASync do
        (startTextIter, endTextIter) <- #getBounds codeTextBuffer
        #removeTagByName codeTextBuffer "error" startTextIter endTextIter

        startTextIter <- #getIterAtOffset codeTextBuffer (fromIntegral position)
        for_ styles \(tagName, _) -> #removeTagByName codeTextBuffer tagName startTextIter endTextIter
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
    highlightParameters parameters
    maybe (pure ()) (highlight textBuffer "type" . snd) returnInfo
    highlightStatement textBuffer body

  where
    highlightParameters Nothing = pure ()
    highlightParameters (Just (id, colon, typeId, rest)) =
      for_ ((undefined, id, colon, typeId) : rest) \(_, id, _, typeId) -> do
        highlight textBuffer "identifier" id
        highlight textBuffer "type" typeId


highlightStatement :: (Gtk.IsTextBuffer a, MonadIO m) => a -> Syntax.Statement -> m ()
highlightStatement textBuffer = \case
  Syntax.ExpressionStatement{value} -> highlightExpression textBuffer value

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

    case arguments of
      Nothing -> pure ()
      Just (first, rest) -> for_ ((undefined, first) : rest) (highlightExpression textBuffer . snd)

  Syntax.UnaryExpression{unary, operand} -> do
    highlight textBuffer "operator" unary
    highlightExpression textBuffer operand

  Syntax.BinaryExpression{left, binary, right} -> do
    highlightExpression textBuffer left
    highlight textBuffer "operator" binary
    highlightExpression textBuffer right

  Syntax.AssignExpression{targetId, assign, value} -> do
    highlight textBuffer "identifier" targetId
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
  Syntax.ExpressionStatement{value} ->
    highlightExpressionParentheses textBuffer insertTextIter value

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

  Syntax.ReturnStatement{result} ->
    maybe (pure False) (highlightExpressionParentheses textBuffer insertTextIter) result

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

  Syntax.CallExpression{open, arguments = Nothing, close} ->
    highlightParentheses textBuffer insertTextIter open close

  Syntax.CallExpression{open, arguments = Just (first, rest), close} -> Helpers.orM
    [
      highlightExpressionParentheses textBuffer insertTextIter first,
      Helpers.anyM (highlightExpressionParentheses textBuffer insertTextIter . snd) rest,
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
highlightParentheses textBuffer' insertTextIter open close = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer

  openStartTextIter <- #getIterAtOffset textBuffer (Span.start open)
  openEndTextIter <- #getIterAtOffset textBuffer (Span.end open)

  closeStartTextIter <- #getIterAtOffset textBuffer (Span.start close)
  closeEndTextIter <- #getIterAtOffset textBuffer (Span.end close)

  applyParenthesisTag <- Helpers.anyM (#equal insertTextIter)
    [openStartTextIter, openEndTextIter, closeEndTextIter, closeStartTextIter]

  if applyParenthesisTag then do
    #applyTagByName textBuffer "bracket" openStartTextIter openEndTextIter
    #applyTagByName textBuffer "bracket" closeStartTextIter closeEndTextIter
    pure True
  else
    pure False


highlight :: (Gtk.IsTextBuffer a, Span b, MonadIO m) => a -> Text -> b -> m ()
highlight textBuffer' tagName span = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer
  startTextIter <- #getIterAtOffset textBuffer (Span.start span)
  endTextIter <- #getIterAtOffset textBuffer (Span.end span)
  #applyTagByName textBuffer tagName startTextIter endTextIter


displayDeclaration :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Declaration -> IO (Maybe Gtk.TreeIter)
displayDeclaration textBuffer treeStore treeIter declaration = case declaration of
  Syntax.VariableDeclaration{varKeyword, variableId, typeInfo, equalSign, value, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter declaration Nothing
    display textBuffer treeStore treeIter' varKeyword Nothing
    display textBuffer treeStore treeIter' variableId (Just variableId.t)
    displayTypeInfo treeIter' typeInfo
    display textBuffer treeStore treeIter' equalSign Nothing
    displayExpression textBuffer treeStore treeIter' value
    display textBuffer treeStore treeIter' semicolon Nothing
    pure treeIter'

  Syntax.FunctionDeclaration{defKeyword, functionId, open, parameters, close, returnInfo, body} -> do
    treeIter' <- display textBuffer treeStore treeIter declaration Nothing
    display textBuffer treeStore treeIter' defKeyword Nothing
    display textBuffer treeStore treeIter' functionId (Just functionId.t)
    display textBuffer treeStore treeIter' open Nothing
    displayParameters treeIter' parameters
    display textBuffer treeStore treeIter' close Nothing
    displayResult treeIter' returnInfo
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  where
    displayTypeInfo _ Nothing = pure ()

    displayTypeInfo treeIter (Just (colon, typeId)) = void do
      display textBuffer treeStore treeIter colon Nothing
      display textBuffer treeStore treeIter typeId (Just typeId.t)

    displayParameters _ Nothing = pure ()

    displayParameters treeIter (Just (id, colon, typeId, rest)) = do
      display textBuffer treeStore treeIter id (Just id.t)
      display textBuffer treeStore treeIter colon Nothing
      display textBuffer treeStore treeIter typeId (Just typeId.t)

      for_ rest \(comma, id, colon, typeId) -> do
        display textBuffer treeStore treeIter comma Nothing
        display textBuffer treeStore treeIter id (Just id.t)
        display textBuffer treeStore treeIter colon Nothing
        display textBuffer treeStore treeIter typeId (Just typeId.t)

    displayResult _ Nothing = pure ()

    displayResult treeIter (Just (arrow, typeId)) = void do
      display textBuffer treeStore treeIter arrow Nothing
      display textBuffer treeStore treeIter typeId (Just typeId.t)


displayStatement :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Statement -> IO (Maybe Gtk.TreeIter)
displayStatement textBuffer treeStore treeIter statement = case statement of
  Syntax.ExpressionStatement{value, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    displayExpression textBuffer treeStore treeIter' value
    display textBuffer treeStore treeIter' semicolon Nothing
    pure treeIter'

  Syntax.IfStatement{ifKeyword, predicate, trueBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' ifKeyword Nothing
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    pure treeIter'

  Syntax.IfElseStatement{ifKeyword, predicate, trueBranch, elseKeyword, falseBranch} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' ifKeyword Nothing
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' trueBranch
    display textBuffer treeStore treeIter' elseKeyword Nothing
    displayStatement textBuffer treeStore treeIter' falseBranch
    pure treeIter'

  Syntax.WhileStatement{whileKeyword, predicate, body} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' whileKeyword Nothing
    displayExpression textBuffer treeStore treeIter' predicate
    displayStatement textBuffer treeStore treeIter' body
    pure treeIter'

  Syntax.DoWhileStatement{doKeyword, body, whileKeyword, predicate, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' doKeyword Nothing
    displayStatement textBuffer treeStore treeIter' body
    display textBuffer treeStore treeIter' whileKeyword Nothing
    displayExpression textBuffer treeStore treeIter' predicate
    display textBuffer treeStore treeIter' semicolon Nothing
    pure treeIter'

  Syntax.ReturnStatement{returnKeyword, result, semicolon} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' returnKeyword Nothing
    maybe (pure ()) (void . displayExpression textBuffer treeStore treeIter') result
    display textBuffer treeStore treeIter' semicolon Nothing
    pure treeIter'

  Syntax.BlockStatement{open, elements, close} -> do
    treeIter' <- display textBuffer treeStore treeIter statement Nothing
    display textBuffer treeStore treeIter' open Nothing
    for_ elements (displayElement treeIter')
    display textBuffer treeStore treeIter' close Nothing
    pure treeIter'

  where
    displayElement treeIter = \case
      Left declaration -> displayDeclaration textBuffer treeStore treeIter declaration
      Right statement -> displayStatement textBuffer treeStore treeIter statement


displayExpression :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.Expression -> IO (Maybe Gtk.TreeIter)
displayExpression textBuffer treeStore treeIter expression = case expression of
  Syntax.IntegerExpression{t} ->
    display textBuffer treeStore treeIter expression (Just t)

  Syntax.RationalExpression{t} ->
    display textBuffer treeStore treeIter expression (Just t)

  Syntax.VariableExpression{variableId, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    display textBuffer treeStore treeIter' variableId (Just variableId.t)
    pure treeIter'

  Syntax.CallExpression{targetId, open, arguments, close, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    display textBuffer treeStore treeIter' targetId (Just targetId.t)
    display textBuffer treeStore treeIter' open Nothing
    displayArguments treeIter' arguments
    display textBuffer treeStore treeIter' close Nothing
    pure treeIter'

  Syntax.UnaryExpression{unary, operand, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    displayUnaryOperator textBuffer treeStore treeIter' unary
    displayExpression textBuffer treeStore treeIter' operand
    pure treeIter'

  Syntax.BinaryExpression{left, binary, right, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    displayExpression textBuffer treeStore treeIter' left
    displayBinaryOperator textBuffer treeStore treeIter' binary
    displayExpression textBuffer treeStore treeIter' right
    pure treeIter'

  Syntax.AssignExpression{targetId, assign, value, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    display textBuffer treeStore treeIter' targetId (Just targetId.t)
    displayAssignOperator textBuffer treeStore treeIter' assign
    displayExpression textBuffer treeStore treeIter' value
    pure treeIter'

  Syntax.ParenthesizedExpression{open, inner, close, t} -> do
    treeIter' <- display textBuffer treeStore treeIter expression (Just t)
    display textBuffer treeStore treeIter' open Nothing
    displayExpression textBuffer treeStore treeIter' inner
    display textBuffer treeStore treeIter' close Nothing
    pure treeIter'

  where
    displayArguments _ Nothing = pure ()
    displayArguments treeIter (Just (first, rest)) = do
      displayExpression textBuffer treeStore treeIter first

      for_ rest \(comma, argument) -> do
        display textBuffer treeStore treeIter comma Nothing
        displayExpression textBuffer treeStore treeIter argument


displayUnaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.UnaryOperator -> IO (Maybe Gtk.TreeIter)
displayUnaryOperator textBuffer treeStore treeIter unary =
  display textBuffer treeStore treeIter unary (Just unary.t)


displayBinaryOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.BinaryOperator -> IO (Maybe Gtk.TreeIter)
displayBinaryOperator textBuffer treeStore treeIter binary =
  display textBuffer treeStore treeIter binary (Just binary.t)


displayAssignOperator :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b) => a -> b -> Maybe Gtk.TreeIter -> Syntax.AssignOperator -> IO (Maybe Gtk.TreeIter)
displayAssignOperator textBuffer treeStore treeIter assign =
  display textBuffer treeStore treeIter assign (Just assign.t)


display :: (Gtk.IsTextBuffer a, Gtk.IsTreeStore b, Syntax.Node c) => a -> b -> Maybe Gtk.TreeIter -> c -> Maybe Type -> IO (Maybe Gtk.TreeIter)
display textBuffer' treeStore' treeIter node t = do
  let textBuffer = textBuffer' `asA` Gtk.TextBuffer
      treeStore = treeStore' `asA` Gtk.TreeStore
      label = Just (Syntax.label node)
      typeId = Type.label <$> t

  treeIter' <- #append treeStore treeIter

  if Syntax.isLeaf node then do
    startTextIter <- #getIterAtOffset textBuffer (Span.start node)
    endTextIter <- #getIterAtOffset textBuffer (Span.end node)
    slice <- #getSlice textBuffer startTextIter endTextIter True
    #set treeStore treeIter' [0, 1, 2] =<< for [label, Just slice, typeId] toGValue
  else
    #set treeStore treeIter' [0, 1, 2] =<< for [label, Nothing, typeId] toGValue

  pure (Just treeIter')
