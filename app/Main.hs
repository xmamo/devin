{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Prelude hiding (getLine)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.IORef
import Data.Maybe
import Data.String
import System.Environment
import System.Exit

import qualified Data.Text as Text

import Text.Parsec.Error

import Control.Monad.Extra
import Data.List.Extra

import qualified GI.GObject as G
import qualified GI.Gio as G
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource
import Data.GI.Gtk.BuildFn
import Data.GI.Gtk.ModelView.CellLayout
import Data.GI.Gtk.ModelView.ForestStore
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.Threading

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Interval
import Devin.Parsec hiding (Error)
import Devin.Parsers
import Devin.Syntax
import Devin.Typer
import Devin.Typers

import Devin.Debug.Evaluator
import Devin.Debug.Syntax
import Devin.Highlight
import Devin.Highlight.Braces
import Devin.Highlight.Syntax


main :: IO ()
main = Gtk.applicationNew Nothing [G.ApplicationFlagsDefaultFlags] >>= \case
  Nothing -> exitFailure

  Just application -> do
    G.onApplicationActivate application (onActivate application)
    status <- G.applicationRun application Nothing
    when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))


onActivate :: Gtk.IsApplication a => a -> G.ApplicationActivateCallback
onActivate application = do
  setCurrentThreadAsGUIThread

  let noTextTagTable = Nothing @Gtk.TextTagTable
  let noAdjustment = Nothing @Gtk.Adjustment

  -- Build the UI:

  stopButton <- Gtk.buttonNewFromIconName (Just "media-playback-stop-symbolic") 1
  Gtk.widgetSetSensitive stopButton False

  playButton <- Gtk.buttonNewFromIconName (Just "media-playback-start-symbolic") 1
  Gtk.widgetSetSensitive playButton False

  headerBar <- Gtk.headerBarNew
  Gtk.headerBarSetTitle headerBar (Just "Devin")
  Gtk.headerBarSetShowCloseButton headerBar True
  Gtk.containerAdd headerBar stopButton
  Gtk.containerAdd headerBar playButton

  codeBuffer <- GtkSource.bufferNew noTextTagTable
  GtkSource.bufferSetHighlightSyntax codeBuffer False
  GtkSource.bufferSetHighlightMatchingBrackets codeBuffer False

  codeTextView <- GtkSource.viewNewWithBuffer codeBuffer
  GtkSource.viewSetShowLineNumbers codeTextView True
  GtkSource.viewSetAutoIndent codeTextView True
  GtkSource.viewSetTabWidth codeTextView 4
  Gtk.textViewSetMonospace codeTextView True

  syntaxTreeStore <- forestStoreNew []
  syntaxTreeView <- Gtk.treeViewNewWithModel syntaxTreeStore
  Gtk.treeViewSetHeadersVisible syntaxTreeView False
  Gtk.treeViewSetEnableSearch syntaxTreeView False
  Gtk.treeViewSetGridLines syntaxTreeView Gtk.TreeViewGridLinesVertical

  stateStore <- forestStoreNew []
  stateView <- Gtk.treeViewNewWithModel stateStore
  Gtk.treeViewSetHeadersVisible stateView False
  Gtk.treeViewSetEnableSearch stateView False
  Gtk.treeViewSetGridLines stateView Gtk.TreeViewGridLinesVertical

  logStore <- seqStoreNew []
  logView <- Gtk.treeViewNewWithModel logStore
  Gtk.treeViewSetHeadersVisible logView False
  Gtk.treeViewSetEnableSearch logView False

  codeScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd codeScrolledWindow codeTextView

  rightScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd rightScrolledWindow syntaxTreeView

  logScrolledWindow <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd logScrolledWindow logView

  horizontalPaned <- Gtk.panedNew Gtk.OrientationHorizontal
  Gtk.panedPack1 horizontalPaned codeScrolledWindow True False
  Gtk.panedPack2 horizontalPaned rightScrolledWindow True False

  verticalPaned <- Gtk.panedNew Gtk.OrientationVertical
  Gtk.panedPack1 verticalPaned horizontalPaned True False
  Gtk.panedPack2 verticalPaned logScrolledWindow False False

  window <- Gtk.applicationWindowNew application
  Gtk.windowSetDefaultSize window 1024 576
  Gtk.windowSetTitlebar window (Just headerBar)
  Gtk.containerAdd window verticalPaned

  -- Set up columns for syntaxTreeView, stateView, logView:

  renderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily renderer "monospace"

  treeViewAppendColumnWithDataFunction syntaxTreeView syntaxTreeStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (fst row)

  treeViewAppendColumnWithDataFunction syntaxTreeView syntaxTreeStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (snd row)

  treeViewAppendColumnWithDataFunction stateView stateStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (fst row)

  treeViewAppendColumnWithDataFunction stateView stateStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (snd row)

  treeViewAppendColumnWithDataFunction logView logStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (fst row)

  treeViewAppendColumnWithDataFunction logView logStore renderer $
    \row -> Gtk.setCellRendererTextText renderer (snd row)

  -- Set up the the tag table. This is needed for syntax highlighting.

  scheme <- GtkSource.bufferGetStyleScheme codeBuffer
  tags <- generateTags scheme
  tagTable <- Gtk.textBufferGetTagTable codeBuffer
  applyTags tags tagTable

  -- Set up codeBuffer callbacks for "changed" and "notify::cursor-position"
  -- signals. The former callback signals that parsing, type checking, and
  -- syntax highlighting need to be performed again; the latter takes care of
  -- highlighting matching braces.

  syntaxTreeRef <- newIORef Nothing
  parseAndTypeCheckCond <- newEmptyMVar

  Gtk.onTextBufferChanged codeBuffer $ do
    Gtk.widgetSetSensitive playButton False
    tryPutMVar parseAndTypeCheckCond ()
    pure ()

  G.onObjectNotify codeBuffer (Just "cursor-position") $ \_ -> do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    Gtk.textBufferRemoveTag codeBuffer (bracketTag tags) startIter endIter

    whenJustM (readIORef syntaxTreeRef) $ \syntaxTree -> do
      insertMark <- Gtk.textBufferGetInsert codeBuffer
      insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
      highlightDevinBraces tags codeBuffer insertIter syntaxTree
      pure ()

  -- Set up playButton and stopButton callbacks for "clicked" signals.
  --
  -- The play button exhibits different behavior depending on context:
  -- initially, its function is to start the debugging process; pressing it
  -- again will advance debugging to the next breakpoint.

  evaluatorAndStateRef <- newIORef Nothing
  debuggerCond <- newEmptyMVar

  Gtk.onButtonClicked playButton $ do
    Gtk.widgetSetSensitive playButton False
    Gtk.widgetSetSensitive stopButton True

    whenM (isNothing <$> readIORef evaluatorAndStateRef) $ do
      Gtk.textViewSetEditable codeTextView False
      Gtk.containerRemove rightScrolledWindow syntaxTreeView
      Gtk.containerAdd rightScrolledWindow stateView
      Gtk.widgetShowAll rightScrolledWindow

      Just syntaxTree <- readIORef syntaxTreeRef
      let evaluator = evalDevin syntaxTree
      state <- makePredefinedState
      writeIORef evaluatorAndStateRef (Just (evaluator, state))

    tryPutMVar debuggerCond ()
    pure ()

  Gtk.onButtonClicked stopButton $ do
    Gtk.widgetSetSensitive playButton False
    Gtk.widgetSetSensitive stopButton False
    writeIORef evaluatorAndStateRef Nothing
    tryPutMVar debuggerCond ()
    pure ()

  -- Start the parse and type check worker thread:

  forkIO $ forever $ do
    readMVar parseAndTypeCheckCond

    text <- postGUISync $ do
      (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
      text <- Gtk.textIterGetText startIter endIter
      takeMVar parseAndTypeCheckCond
      pure text

    -- Run the parser
    let parser = liftA2 (,) devin getState
    parserResult <- runParserT parser [] "" (0, text)

    whenJustM (lookupEnv "DEVIN_PARSER_DELAY") $ \delay ->
      threadDelay (round (1000000.0 * read delay))

    case parserResult of
      -- Parser failure:
      Left parseError -> do
        let offset = toOffset (errorPos parseError) text
        let messages = errorMessages parseError

        postGUIASync $ do
          writeIORef syntaxTreeRef Nothing

          -- Remove previous error highlighting, if any
          (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
          Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

          -- Highlight as error, starting from the parseError offset
          startIter <- Gtk.textBufferGetIterAtOffset codeBuffer offset
          Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

          -- Update the error log; clear the syntax tree preview
          (line, column) <- getLineColumn startIter
          let datum = (showLineColumn (line, column), showMessages messages)
          seqStoreClear logStore
          seqStoreAppend logStore datum
          forestStoreClear syntaxTreeStore

      -- Parser success:
      Right (syntaxTree, comments) -> do
        postGUIASync $ do
          writeIORef syntaxTreeRef (Just syntaxTree)

          -- Remove any previous highlighting
          (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
          Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (bracketTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (keywordTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (varIdTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (funIdTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (typeTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (numberTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (operatorTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (commentTag tags) startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

          -- Highlight syntax
          highlightDevin tags codeBuffer syntaxTree
          for_ comments (highlightInterval (commentTag tags) codeBuffer)

          -- Highlight matching braces
          insertMark <- Gtk.textBufferGetInsert codeBuffer
          insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
          highlightDevinBraces tags codeBuffer insertIter syntaxTree

          -- Update the syntax tree preview
          let forest = map definitionTree (definitions syntaxTree)
          rootPath <- Gtk.treePathNew
          forestStoreClear syntaxTreeStore
          forestStoreInsertForest syntaxTreeStore rootPath -1 forest
          Gtk.treeViewExpandAll syntaxTreeView

          -- Clear the log
          seqStoreClear logStore

        -- Run the type checker
        let typer = checkDevin syntaxTree
        let typerResult = runTyper typer predefinedEnv

        whenJustM (lookupEnv "DEVIN_TYPER_DELAY") $ \delay ->
          threadDelay (round (1000000.0 * read delay))

        case typerResult of
          -- Type checker success:
          ((), env, []) -> do
            let (hasMain, _, _) = runTyper checkHasMain env

            postGUIASync $
              Gtk.widgetSetSensitive playButton hasMain

            where
              checkHasMain = lookupFunSignature "main" >>= \case
                Just (([], _), _) -> pure True
                _ -> pure False

          -- Type checker failure:
          ((), _, errors) -> postGUIASync $ for_ errors $ \error -> do
            -- Highlight the part of code which caused the failure
            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
            endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
            Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

            -- Append the error description to the log
            (line, column) <- getLineColumn startIter
            let datum = (showLineColumn (line, column), Text.pack (display error))
            seqStoreAppend logStore datum

  -- Start the debugger worker thread:

  forkIO $ forever $ do
    readMVar debuggerCond

    evaluatorAndState <- postGUISync $ do
      evaluatorAndState <- readIORef evaluatorAndStateRef
      takeMVar debuggerCond
      pure evaluatorAndState

    case evaluatorAndState of
      Nothing -> postGUIASync $ do
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

        forestStoreClear stateStore

        Gtk.widgetSetSensitive playButton True
        Gtk.widgetSetSensitive stopButton False
        Gtk.textViewSetEditable codeTextView True
        Gtk.containerRemove rightScrolledWindow stateView
        Gtk.containerAdd rightScrolledWindow syntaxTreeView
        Gtk.widgetShowAll rightScrolledWindow

      Just (evaluator, state) -> do
        (result, state') <- runEvaluatorStep evaluator state

        whenJustM (lookupEnv "DEVIN_EVALUATOR_DELAY") $ \delay ->
          threadDelay (round (1000000.0 * read delay))

        case result of
          Done _ -> postGUIASync $ do
            writeIORef evaluatorAndStateRef Nothing
            tryPutMVar debuggerCond ()
            pure ()

          Yield statement evaluator' | BreakpointStatement {} <- statement -> do
            forest <- stateForest state'

            postGUIASync $ do
              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter
              highlightInterval (highlightTag tags) codeBuffer statement

              rootPath <- Gtk.treePathNew
              forestStoreClear stateStore
              forestStoreInsertForest stateStore rootPath -1 forest
              Gtk.treeViewExpandAll stateView

              writeIORef evaluatorAndStateRef (Just (evaluator', state'))
              Gtk.widgetSetSensitive playButton True

          Yield _ evaluator' -> postGUIASync $
            whenM (isJust <$> readIORef evaluatorAndStateRef) $ do
              writeIORef evaluatorAndStateRef (Just (evaluator', state'))
              tryPutMVar debuggerCond ()
              pure ()

          Error error -> do
            forest <- stateForest state'

            postGUIASync $ do
              Gtk.widgetSetSensitive stopButton False

              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

              startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
              endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
              Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

              rootPath <- Gtk.treePathNew
              forestStoreClear stateStore
              forestStoreInsertForest stateStore rootPath -1 forest
              Gtk.treeViewExpandAll stateView

              -- Display the error message.
              --
              -- gi-gtk doesn't provide bindings for gtk_message_dialog_new().
              -- To get around this limitation, we use Gtk.Builder to load the
              -- Gtk.MessageDialog from an XML string.

              (line, column) <- getLineColumn startIter
              let text = showsLineColumn (line, column) (' ' : display error)
              let text' = escapeXml ("<tt>" ++ escapeXml text ++ "</tt>")

              let string =
                    "<interface>\n\
                    \  <object class=\"GtkMessageDialog\" id=\"dialog\">\n\
                    \    <property name=\"title\">Error</property>\n\
                    \    <property name=\"message-type\">error</property>\n\
                    \    <property name=\"buttons\">close</property>\n\
                    \    <property name=\"use-markup\">true</property>\n\
                    \    <property name=\"text\">" ++ text' ++ "</property>\n\
                    \  </object>\n\
                    \</interface>\0"

              let buildFn = getObject Gtk.MessageDialog "dialog"
              builder <- Gtk.builderNewFromString (Text.pack string) -1
              dialog <- buildWithBuilder buildFn builder
              Gtk.windowSetTransientFor dialog (Just window)
              Gtk.dialogRun dialog

              -- Cleanup:

              Gtk.widgetDestroy dialog
              Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter
              writeIORef evaluatorAndStateRef Nothing
              tryPutMVar debuggerCond ()
              pure ()

  -- Display the UI:

  Gtk.widgetShowAll window


treeViewAppendColumnWithDataFunction ::
  (Gtk.IsTreeView a, IsTypedTreeModel model, Gtk.IsTreeModel (model row), Gtk.IsCellRenderer cell) =>
  a -> model row -> cell -> (row -> IO ()) -> IO Int32
treeViewAppendColumnWithDataFunction view model renderer f = do
  column <- Gtk.treeViewColumnNew
  Gtk.cellLayoutPackStart column renderer True
  cellLayoutSetDataFunction column renderer model f
  Gtk.treeViewAppendColumn view column


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

  flip loopM 1 $ \column -> do
    result <- Gtk.textIterCompare iter' iter

    if result >= 0 then
      pure (Right column)
    else do
      Gtk.textIterForwardCursorPosition iter'
      pure (Left (column + 1))


showLineColumn :: (Num a, Show a, IsString b) => (a, a) -> b
showLineColumn (line, column) = fromString (showsLineColumn (line, column) "")


showsLineColumn :: (Num a, Show a) => (a, a) -> ShowS
showsLineColumn (line, column) =
  showChar '[' . shows line . showChar ':' . shows column . showChar ']'


showMessages :: IsString a => [Message] -> a
showMessages messages =
  let s1 = "or"
      s2 = "Unknown parse error"
      s3 = "Expecting"
      s4 = "Unexpected"
      s5 = "end of input"

   in case showErrorMessages s1 s2 s3 s4 s5 (filter f messages) of
        ('\n' : string) -> fromString string
        string -> fromString string

  where
    f (Expect _) = True
    f (Message _) = True
    f _ = False


escapeXml :: String -> String
escapeXml s =
  let s' = replace "&" "&amp;" s
      s'' = replace "\0" "&#0;" s'
      s''' = replace "<" "&lt;" s''
      s'''' = replace ">" "&gt;" s'''
   in s''''
