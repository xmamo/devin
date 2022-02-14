{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (getLine)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.String
import System.Environment
import System.Exit

import Data.Text (Text)
import Data.Text qualified as Text

import Text.Parsec.Error

import Control.Monad.Extra

import GI.GObject qualified as G
import GI.Gio qualified as G
import Data.GI.Gtk qualified as Gtk
import Data.GI.Gtk.BuildFn qualified as Gtk
import Data.GI.Gtk.Threading qualified as Gtk
import GI.GtkSource qualified as GtkSource

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Interval
import Devin.Parsec hiding (Error)
import Devin.Parsers qualified as Parsers
import Devin.Syntax
import Devin.Typer
import Devin.Typers

import Devin.Highlight
import Devin.Highlight.Braces
import Devin.Highlight.Syntax
import Devin.Tree


main :: IO ()
main = do
  setEnv "LC_ALL" "POSIX"

  Gtk.applicationNew Nothing [] >>= \case
    Nothing -> exitFailure

    Just application -> do
      G.onApplicationActivate application (onActivate application)
      status <- G.applicationRun application Nothing
      when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))


onActivate :: Gtk.IsApplication a => a -> G.ApplicationActivateCallback
onActivate application = do
  syntaxTreeStore <- Gtk.forestStoreNew []
  stateStore <- Gtk.forestStoreNew []
  logStore <- Gtk.seqStoreNew []

  -- Build the UI:

  stopButton <- Gtk.buttonNewFromIconName (Just "media-playback-stop-symbolic") 2
  Gtk.widgetSetSensitive stopButton False

  playButton <- Gtk.buttonNewFromIconName (Just "media-playback-start-symbolic") 2
  Gtk.widgetSetSensitive playButton False

  actionBar <- Gtk.actionBarNew
  Gtk.actionBarPackStart actionBar stopButton
  Gtk.actionBarPackStart actionBar playButton

  codeBuffer <- GtkSource.bufferNew (Nothing @Gtk.TextTagTable)
  GtkSource.bufferSetHighlightSyntax codeBuffer False
  GtkSource.bufferSetHighlightMatchingBrackets codeBuffer False

  codeTextView <- GtkSource.viewNewWithBuffer codeBuffer
  GtkSource.viewSetShowLineNumbers codeTextView True
  GtkSource.viewSetHighlightCurrentLine codeTextView True
  GtkSource.viewSetAutoIndent codeTextView True
  GtkSource.viewSetTabWidth codeTextView 4
  Gtk.textViewSetMonospace codeTextView True

  syntaxTreeView <- Gtk.treeViewNewWithModel syntaxTreeStore
  Gtk.treeViewSetHeadersVisible syntaxTreeView False
  Gtk.treeViewSetEnableSearch syntaxTreeView False
  Gtk.treeViewSetGridLines syntaxTreeView Gtk.TreeViewGridLinesVertical
  setUpTwoColumns syntaxTreeStore syntaxTreeView

  stateView <- Gtk.treeViewNewWithModel stateStore
  Gtk.treeViewSetHeadersVisible stateView False
  Gtk.treeViewSetEnableSearch stateView False
  Gtk.treeViewSetGridLines stateView Gtk.TreeViewGridLinesVertical
  setUpTwoColumns stateStore stateView

  logView <- Gtk.treeViewNewWithModel logStore
  Gtk.treeViewSetHeadersVisible logView False
  Gtk.treeViewSetEnableSearch logView False
  setUpTwoColumns logStore logView

  scrolledWindow1 <- Gtk.scrolledWindowNew (Nothing @Gtk.Adjustment) (Nothing @Gtk.Adjustment)
  Gtk.containerAdd scrolledWindow1 codeTextView

  scrolledWindow2 <- Gtk.scrolledWindowNew (Nothing @Gtk.Adjustment) (Nothing @Gtk.Adjustment)
  Gtk.containerAdd scrolledWindow2 syntaxTreeView

  scrolledWindow3 <- Gtk.scrolledWindowNew (Nothing @Gtk.Adjustment) (Nothing @Gtk.Adjustment)
  Gtk.containerAdd scrolledWindow3 logView

  paned1 <- Gtk.panedNew Gtk.OrientationHorizontal
  Gtk.panedPack1 paned1 scrolledWindow1 True False
  Gtk.panedPack2 paned1 scrolledWindow2 True False

  paned2 <- Gtk.panedNew Gtk.OrientationVertical
  Gtk.panedPack1 paned2 paned1 True False
  Gtk.panedPack2 paned2 scrolledWindow3 False False

  box <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.boxPackStart box actionBar False False 0
  Gtk.boxPackStart box paned2 True True 0

  window <- Gtk.applicationWindowNew application
  Gtk.windowSetTitle window Text.empty
  Gtk.windowSetDefaultSize window 1280 720
  Gtk.containerAdd window box

  -- Set up some stuff needed for later:

  scheme <- GtkSource.bufferGetStyleScheme codeBuffer
  tags <- generateTags scheme

  tagTable <- Gtk.textBufferGetTagTable codeBuffer
  applyTags tags tagTable

  syntaxTreeVar <- newEmptyMVar
  evaluationThreadIdVar <- newEmptyMVar
  playButtonClickCallbackVar <- newEmptyMVar

  -- Set up callback for codeBuffer "changed" signals.
  -- The callback takes care of syntax highlighting.

  Gtk.onTextBufferChanged codeBuffer $ do
    -- The newly inserted code might be syntactically invalid.
    -- Preemptively discard the old syntax tree (if any) and disable the "play" button.
    tryTakeMVar syntaxTreeVar
    Gtk.widgetSetSensitive playButton False

    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    text <- Gtk.textBufferGetText codeBuffer startIter endIter True

    case runParser (liftA2 (,) Parsers.devin getState) [] "" (0, text) of
      -- Parser failure:
      Left parseError -> do
        let offset = toOffset (errorPos parseError) text
        let messages = errorMessages parseError

        -- Remove previuous "error" highlighting, if any
        Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

        -- Highlight as "error", starting from the parseError offset to the end
        startIter <- Gtk.textBufferGetIterAtOffset codeBuffer offset
        Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

        -- Update the error log; clear the syntax tree preview
        (line, column) <- getLineColumn startIter
        Gtk.seqStoreClear logStore
        Gtk.seqStoreAppend logStore (showLineColumn (line, column), showMessages messages)
        Gtk.forestStoreClear syntaxTreeStore

      -- Parser success:
      Right (devin, comments) -> do
        let Devin {declarations} = devin
        putMVar syntaxTreeVar devin

        -- Remove any previous highlighting
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (bracketTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (keywordTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (variableIdTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (functionIdTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (typeTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (numberTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (operatorTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (commentTag tags) startIter endIter
        Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

        -- Highlight syntax
        highlightDevin tags codeBuffer devin
        for_ comments (highlightInterval (commentTag tags) codeBuffer)

        -- Highlight matching braces
        insertMark <- Gtk.textBufferGetInsert codeBuffer
        insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
        highlightDevinBraces tags codeBuffer insertIter devin

        -- Update the syntax tree preview
        rootPath <- Gtk.treePathNew
        Gtk.forestStoreClear syntaxTreeStore
        Gtk.forestStoreInsertForest syntaxTreeStore rootPath 0 (map declarationTree declarations)
        Gtk.treeViewExpandAll syntaxTreeView

        -- Clear any previous error messages
        Gtk.seqStoreClear logStore

        case runTyper (checkDevin devin) predefinedEnvironment of
          -- Typer success:
          ((), environment, []) -> do
            -- Enable the play button if "main()" has been found
            let (hasMain, _, _) = runTyper checkHasMain environment
            Gtk.widgetSetSensitive playButton hasMain
            where
              checkHasMain = lookupFunctionSignature "main" >>= \case
                Just (([], _), _) -> pure True
                _ -> pure False

          -- Typer failure:
          ((), _, errors) -> for_ errors $ \error -> do
            -- Highlight the part of code which caused the failure as "error"
            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
            endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
            Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

            -- Append the error desctiption to the log
            (line, column) <- getLineColumn startIter
            Gtk.seqStoreAppend logStore (showLineColumn (line, column), Text.pack (display error))

  -- Set up callback for codeBuffer "notify::cursor-position" signals.
  -- The callback takes care of highlighting matching braces.

  G.onObjectNotify codeBuffer (Just "cursor-position") $ \_ -> do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    Gtk.textBufferRemoveTag codeBuffer (bracketTag tags) startIter endIter

    whenJustM (tryReadMVar syntaxTreeVar) $ \devin -> do
      insertMark <- Gtk.textBufferGetInsert codeBuffer
      insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
      highlightDevinBraces tags codeBuffer insertIter devin
      pure ()

  -- The play button exhibits different behavior depending on context:
  -- 1. Initially, its function is to start the evaluation process;
  -- 2. Once it has been started, pressing the button again will advance the evaluation by one step.
  -- initialPlayButtonCallback holds the action corresponding to (1).

  let initialPlayButtonCallback = whenJustM (tryReadMVar syntaxTreeVar) $ \devin -> do
        Gtk.widgetSetSensitive stopButton True
        Gtk.textViewSetEditable codeTextView False
        Gtk.containerRemove scrolledWindow2 syntaxTreeView
        Gtk.containerAdd scrolledWindow2 stateView
        Gtk.widgetShowAll scrolledWindow2

        state <- makePredefinedState
        putMVar evaluationThreadIdVar =<< forkIO (go (evaluateDevin devin) state)

      go evaluator state = do
        (result, state') <- runEvaluatorStep evaluator state

        case result of
          Done _ -> do
            takeMVar evaluationThreadIdVar
            Gtk.postGUIASync cleanup

          -- Don't pause on some intermediate steps:
          Step (BeforeDeclaration FunctionDeclaration {}) evaluator' -> go evaluator' state'
          Step (AfterDeclaration FunctionDeclaration {}) evaluator' -> go evaluator' state'
          Step (BeforeStatement (DeclarationStatement _)) evaluator' -> go evaluator' state'
          Step (AfterStatement (DeclarationStatement _)) evaluator' -> go evaluator' state'
          Step (BeforeExpression _) evaluator' -> go evaluator' state'
          Step (AfterExpression _) evaluator' -> go evaluator' state'

          Step onSyntax evaluator' -> do
            Gtk.postGUIASync $ do
              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter
              highlightInterval (highlightTag tags) codeBuffer onSyntax

              forest <- stateForest state'
              rootPath <- Gtk.treePathNew
              Gtk.forestStoreClear stateStore
              Gtk.forestStoreInsertForest stateStore rootPath 0 forest
              Gtk.treeViewExpandAll stateView

            tryTakeMVar playButtonClickCallbackVar
            putMVar playButtonClickCallbackVar (go evaluator' state')

          Error error -> do
            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
            endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
            Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

            (line, column) <- getLineColumn startIter
            let s = showsLineColumn (line, column) . showChar '\t' . displays error

            -- Display the error message.
            -- gi-gtk doesn't provide bindings for gtk_message_dialog_new() and related functions.
            -- To get around this limitation, we use GtkBuilder to load the MessageDialog from an
            -- XML string. This is ugly, but it works.

            let string = Text.pack $
                  "<interface>\n\
                  \  <object class=\"GtkMessageDialog\" id=\"dialog\">\n\
                  \    <property name=\"title\">Error</property>\n\
                  \    <property name=\"message-type\">error</property>\n\
                  \    <property name=\"buttons\">close</property>\n\
                  \    <property name=\"text\">" ++ s "</property>\n\
                  \  </object>\n\
                  \</interface>"

            builder <- Gtk.builderNewFromString string -1
            dialog <- Gtk.buildWithBuilder (Gtk.getObject Gtk.MessageDialog "dialog") builder
            Gtk.windowSetTransientFor dialog (Just window)
            Gtk.dialogRun dialog
            Gtk.widgetDestroy dialog
            Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter
            takeMVar evaluationThreadIdVar
            cleanup

      cleanup = do
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

        Gtk.textViewSetEditable codeTextView True
        Gtk.widgetSetSensitive stopButton False
        Gtk.containerRemove scrolledWindow2 stateView
        Gtk.containerAdd scrolledWindow2 syntaxTreeView
        Gtk.widgetShowAll scrolledWindow2

        tryTakeMVar playButtonClickCallbackVar
        putMVar playButtonClickCallbackVar initialPlayButtonCallback

  -- Most of the functionality for the "play" and "stop" buttons has already been written in
  -- the previous part.

  Gtk.onButtonClicked stopButton $ do
    killThread =<< takeMVar evaluationThreadIdVar
    cleanup

  Gtk.onButtonClicked playButton $ tryReadMVar playButtonClickCallbackVar >>= \case
    Just callback -> callback
    Nothing -> initialPlayButtonCallback

  -- Display the UI:

  Gtk.widgetShowAll window


setUpTwoColumns ::
  (Gtk.IsTreeModel (model (Text, Text)), Gtk.IsTypedTreeModel model, Gtk.IsTreeView a, MonadIO m) =>
  model (Text, Text) -> a -> m ()
setUpTwoColumns model treeView = for_ [fst, snd] $ \f -> do
  cellRenderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily cellRenderer "monospace"

  let g x = Gtk.setCellRendererTextText cellRenderer (f x)
  treeViewColumn <- Gtk.treeViewColumnNew
  Gtk.cellLayoutSetDataFunction treeViewColumn cellRenderer model g
  Gtk.cellLayoutPackStart treeViewColumn cellRenderer True

  Gtk.treeViewAppendColumn treeView treeViewColumn


getLineColumn :: (Num a, MonadIO m) => Gtk.TextIter -> m (a, a)
getLineColumn iter = do
  line <- getLine iter
  column <- getColumn iter
  pure (line, column)


getLine :: (Num a, MonadIO m) => Gtk.TextIter -> m a
getLine iter = do
  line <- Gtk.textIterGetLine iter
  pure (fromIntegral line + 1)


getColumn :: (Num a, MonadIO m) => Gtk.TextIter -> m a
getColumn iter = do
  iter' <- Gtk.textIterCopy iter
  Gtk.textIterSetLineOffset iter' 0
  go iter' 1
  where
    go iter' column = do
      result <- Gtk.textIterCompare iter' iter

      if result >= 0 then do
        pure column
      else do
        Gtk.textIterForwardCursorPosition iter'
        go iter' (column + 1)


showLineColumn :: (Num a, Show a, IsString b) => (a, a) -> b
showLineColumn (line, column) = fromString (showsLineColumn (line, column) "")


showsLineColumn :: (Num a, Show a) => (a, a) -> ShowS
showsLineColumn (line, column) =
  showChar '[' . shows line . showChar ':' . shows column . showChar ']'


showMessages :: IsString a => [Message] -> a
showMessages messages =
  case showErrorMessages "or" "Unknown parse error" "Expecting" "Unexpected" "end of input" m of
    ('\n' : string) -> fromString string
    string -> fromString string

  where
    m = filter f messages

    f (Expect _) = True
    f (Message _) = True
    f _ = False
