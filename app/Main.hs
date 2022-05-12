{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (getLine)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.String
import System.Environment
import System.Exit

import Text.Parsec.Error

import Control.Monad.Extra
import Data.List.Extra

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Interval
import Devin.Parsec hiding (Error)
import qualified Devin.Parsers as Parsers
import Devin.Syntax
import Devin.Typer
import Devin.Typers

import Data.Text (Text)
import qualified Data.Text as Text

import qualified GI.GObject as G
import qualified GI.Gio as G
import qualified Data.GI.Gtk as Gtk
import qualified Data.GI.Gtk.BuildFn as Gtk
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.GtkSource as GtkSource

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

  let stopIconName = "media-playback-stop-symbolic"
  let playIconName = "media-playback-start-symbolic"
  let noTextTagTable = Nothing :: Maybe Gtk.TextTagTable
  let noAdjustment = Nothing :: Maybe Gtk.Adjustment

  -- Build the UI:

  stopButton <- Gtk.buttonNewFromIconName (Just stopIconName) 2
  Gtk.widgetSetSensitive stopButton False

  playButton <- Gtk.buttonNewFromIconName (Just playIconName) 2
  Gtk.widgetSetSensitive playButton False

  actionBar <- Gtk.actionBarNew
  Gtk.actionBarPackStart actionBar stopButton
  Gtk.actionBarPackStart actionBar playButton

  codeBuffer <- GtkSource.bufferNew noTextTagTable
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

  scrolledWindow1 <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd scrolledWindow1 codeTextView

  scrolledWindow2 <- Gtk.scrolledWindowNew noAdjustment noAdjustment
  Gtk.containerAdd scrolledWindow2 syntaxTreeView

  scrolledWindow3 <- Gtk.scrolledWindowNew noAdjustment noAdjustment
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

  syntaxTreeRef <- newIORef Nothing
  parserThreadIdRef <- newIORef =<< forkIO (pure ())
  typerThreadIdRef <- newIORef =<< forkIO (pure ())
  evaluatorThreadIdVar <- newIORef =<< forkIO (pure ())
  playButtonClickCallbackVar <- newIORef (pure ())

  -- Set up codeBuffer callback for "changed" signals.
  -- The callback takes care of syntax highlighting.

  Gtk.onTextBufferChanged codeBuffer $ do
    -- The newly inserted code might be syntactically invalid. Preemptively
    -- discard the old syntax tree (if any) and disable the "play" button.
    atomicWriteIORef syntaxTreeRef Nothing
    Gtk.widgetSetSensitive playButton False

    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    text <- Gtk.textBufferGetText codeBuffer startIter endIter True

    parserThreadId <- readIORef parserThreadIdRef
    killThread parserThreadId

    typerThreadId <- readIORef typerThreadIdRef
    killThread typerThreadId

    let parser = liftA2 (,) Parsers.devin getState

    parserThread <- forkIO $ runParserT parser [] "" (0, text) >>= \case
      -- Parser failure:
      Left parseError -> do
        let offset = toOffset (errorPos parseError) text
        let messages = errorMessages parseError

        Gtk.postGUIASync $ do
          -- Remove previous "error" highlighting, if any
          (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
          Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

          -- Highlight as "error", starting from the parseError offset
          startIter <- Gtk.textBufferGetIterAtOffset codeBuffer offset
          Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

          -- Update the error log; clear the syntax tree preview
          (line, column) <- getLineColumn startIter
          let datum = (showLineColumn (line, column), showMessages messages)
          Gtk.seqStoreClear logStore
          Gtk.seqStoreAppend logStore datum
          Gtk.forestStoreClear syntaxTreeStore

      -- Parser success:
      Right (devin, comments) -> do
        let Devin {definitions} = devin
        atomicWriteIORef syntaxTreeRef (Just devin)

        Gtk.postGUIASync $ do
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
          highlightDevin tags codeBuffer devin
          for_ comments (highlightInterval (commentTag tags) codeBuffer)

          -- Highlight matching braces
          insertMark <- Gtk.textBufferGetInsert codeBuffer
          insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
          highlightDevinBraces tags codeBuffer insertIter devin

          -- Update the syntax tree preview
          let syntaxTree = map definitionTree definitions
          rootPath <- Gtk.treePathNew
          Gtk.forestStoreClear syntaxTreeStore
          Gtk.forestStoreInsertForest syntaxTreeStore rootPath 0 syntaxTree
          Gtk.treeViewExpandAll syntaxTreeView

        typerThreadId <- readIORef typerThreadIdRef
        killThread typerThreadId

        let typer = checkDevin devin

        typerThreadId <- forkIO $ pure (runTyper typer predefinedEnv) >>= \case
          -- Typer success:
          ((), env, []) -> do
            let (hasMain, _, _) = runTyper checkHasMain env

            Gtk.postGUIASync $ do
              Gtk.widgetSetSensitive playButton hasMain
              Gtk.seqStoreClear logStore

            where
              checkHasMain = lookupFunSignature "main" >>= \case
                Just (([], _), _) -> pure True
                _ -> pure False

          -- Typer failure:
          ((), _, errors) -> Gtk.postGUIASync $ do
            Gtk.seqStoreClear logStore

            for_ errors $ \error -> do
              -- Highlight the part of code which caused the failure
              startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
              endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
              Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

              -- Append the error description to the log
              (line, column) <- getLineColumn startIter
              let datum = (showLineColumn (line, column), Text.pack (display error))
              Gtk.seqStoreAppend logStore datum

        atomicWriteIORef typerThreadIdRef typerThreadId

    atomicWriteIORef parserThreadIdRef parserThread

  -- Set up codeBuffer callback for "notify::cursor-position" signals.
  -- The callback takes care of highlighting matching braces.

  G.onObjectNotify codeBuffer (Just "cursor-position") $ \_ -> do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    Gtk.textBufferRemoveTag codeBuffer (bracketTag tags) startIter endIter

    whenJustM (readIORef syntaxTreeRef) $ \devin -> do
      insertMark <- Gtk.textBufferGetInsert codeBuffer
      insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
      highlightDevinBraces tags codeBuffer insertIter devin
      pure ()

  -- The play button exhibits different behavior depending on context:
  -- 1. Initially, its function is to start the evaluation process;
  -- 2. Pressing it again will advance evaluation to the next debug statement.
  -- initialPlayButtonCallback holds the action corresponding to (1).

  let initialPlayButtonCallback = whenJustM (readIORef syntaxTreeRef) $ \devin -> do
        Gtk.widgetSetSensitive playButton False
        Gtk.widgetSetSensitive stopButton True
        Gtk.textViewSetEditable codeTextView False
        Gtk.containerRemove scrolledWindow2 syntaxTreeView
        Gtk.containerAdd scrolledWindow2 stateView
        Gtk.widgetShowAll scrolledWindow2

        state <- makePredefinedState
        evaluatorThreadId <- forkIO (go (evalDevin devin) state)
        atomicWriteIORef evaluatorThreadIdVar evaluatorThreadId

      go evaluator state = do
        (result, state') <- runEvaluatorStep evaluator state

        Gtk.postGUIASync $ do
          forest <- stateForest state'
          rootPath <- Gtk.treePathNew
          Gtk.forestStoreClear stateStore
          Gtk.forestStoreInsertForest stateStore rootPath 0 forest
          Gtk.treeViewExpandAll stateView

        case result of
          Done _ -> Gtk.postGUIASync cleanup

          Debug statement evaluator' -> do
            atomicWriteIORef playButtonClickCallbackVar $ do
              Gtk.postGUIASync (Gtk.widgetSetSensitive playButton False)

              (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
              Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

              evaluatorThreadId <- forkIO (go evaluator' state')
              atomicWriteIORef evaluatorThreadIdVar evaluatorThreadId

            Gtk.postGUIASync $ do
              Gtk.widgetSetSensitive playButton True
              highlightInterval (highlightTag tags) codeBuffer statement

          Error error -> Gtk.postGUIASync $ do
            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
            endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
            Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

            -- Display the error message.
            -- gi-gtk doesn't provide bindings for gtk_message_dialog_new() and
            -- related functions. To get around this limitation, we use
            -- GtkBuilder to load the MessageDialog from an XML string. This is
            -- ugly, but it works.

            (line, column) <- getLineColumn startIter
            let s = showsLineColumn (line, column) (showChar ' ' (display error))
            let s' = replace ">" "&gt;" (replace "<" "&lt;" (replace "&" "&amp;" s))
            let s'' = "<tt>" ++ s' ++ "</tt>"
            let s''' = replace ">" "&gt;" (replace "<" "&lt;" (replace "&" "&amp;" s''))

            let string = Text.pack $
                  "<interface>\n\
                  \  <object class=\"GtkMessageDialog\" id=\"dialog\">\n\
                  \    <property name=\"title\">Error</property>\n\
                  \    <property name=\"message-type\">error</property>\n\
                  \    <property name=\"buttons\">close</property>\n\
                  \    <property name=\"text\">" ++ s''' ++ "</property>\n\
                  \    <property name=\"use-markup\">True</property>\n\
                  \  </object>\n\
                  \</interface>"

            builder <- Gtk.builderNewFromString string -1
            let buildFn = Gtk.getObject Gtk.MessageDialog "dialog"
            dialog <- Gtk.buildWithBuilder buildFn builder
            Gtk.windowSetTransientFor dialog (Just window)
            Gtk.dialogRun dialog
            Gtk.widgetDestroy dialog
            Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

            cleanup

      cleanup = do
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

        Gtk.textViewSetEditable codeTextView True
        Gtk.widgetSetSensitive playButton True
        Gtk.widgetSetSensitive stopButton False
        Gtk.containerRemove scrolledWindow2 stateView
        Gtk.containerAdd scrolledWindow2 syntaxTreeView
        Gtk.widgetShowAll scrolledWindow2

        writeIORef playButtonClickCallbackVar initialPlayButtonCallback

  -- Set up playButton and stopButton callbacks for "clicked" signals:

  atomicWriteIORef playButtonClickCallbackVar initialPlayButtonCallback

  Gtk.onButtonClicked stopButton $ do
    evaluatorThreadId <- readIORef evaluatorThreadIdVar
    killThread evaluatorThreadId
    cleanup

  Gtk.onButtonClicked playButton $ do
    playButtonClickCallback <- readIORef playButtonClickCallbackVar
    playButtonClickCallback

  -- Display the UI:

  Gtk.widgetShowAll window


setUpTwoColumns ::
  (Gtk.IsTreeModel (model (Text, Text)), Gtk.IsTypedTreeModel model, Gtk.IsTreeView a, MonadIO m) =>
  model (Text, Text) -> a -> m ()
setUpTwoColumns model treeView = for_ [fst, snd] $ \f -> do
  cellRenderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily cellRenderer "monospace"

  let g = Gtk.setCellRendererTextText cellRenderer . f
  treeViewColumn <- Gtk.treeViewColumnNew
  Gtk.cellLayoutPackStart treeViewColumn cellRenderer True
  Gtk.cellLayoutSetDataFunction treeViewColumn cellRenderer model g

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

      if result >= 0 then
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
