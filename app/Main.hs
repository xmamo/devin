{-# LANGUAGE ApplicativeDo #-}
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
import Data.IORef
import Data.Maybe
import Data.String
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
import qualified Data.GI.Gtk.Threading as Gdk
import qualified Data.GI.Gtk as Gtk
import qualified Data.GI.Gtk.BuildFn as Gtk
import qualified GI.GtkSource as GtkSource

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
  Gdk.setCurrentThreadAsGUIThread

  let stopIconName = "media-playback-stop-symbolic"
  let playIconName = "media-playback-start-symbolic"
  let noTextTagTable = Nothing :: Maybe Gtk.TextTagTable
  let noAdjustment = Nothing :: Maybe Gtk.Adjustment

  syntaxTreeStore <- Gtk.forestStoreNew []
  stateStore <- Gtk.forestStoreNew []
  logStore <- Gtk.seqStoreNew []

  -- Build the UI:

  stopButton <- Gtk.buttonNewFromIconName (Just stopIconName) 1
  Gtk.widgetSetSensitive stopButton False

  playButton <- Gtk.buttonNewFromIconName (Just playIconName) 1
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

  -- Set up the the tag table. This is needed for syntax highlighting.

  scheme <- GtkSource.bufferGetStyleScheme codeBuffer
  tags <- generateTags scheme
  tagTable <- Gtk.textBufferGetTagTable codeBuffer
  applyTags tags tagTable

  -- Create and initialize:
  --
  --  * The mutable reference syntaxTreeRef, which stores the currently parsed
  --    syntax tree (if any);
  --
  --  * The condition variable parseAndTypeCheckCond, to signal when parsing,
  --    type checking, and syntax highlighting need to be performed again.

  syntaxTreeRef <- newIORef Nothing
  parseAndTypeCheckCond <- newEmptyMVar

  -- Set up codeBuffer callbacks for "changed" and "notify::cursor-position"
  -- signals. These take care of signaling parseAndTypeCheckCond and
  -- highlighting matching braces, respectively.

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

  -- Create and initialize:
  --
  --  * The mutable reference evaluatorAndStateRef, which stores the current
  --    evaluator and state to be used in the debugger (if any).
  --
  --  * The condition variable debuggerCond, to signal when to run the debugger.

  evaluatorAndStateRef <- newIORef Nothing
  debuggerCond <- newEmptyMVar

  -- Set up playButton and stopButton callbacks for "clicked" signals.
  --
  -- The play button exhibits different behavior depending on context:
  -- initially, its function is to start the debugging process; pressing it
  -- again will advance debugging to the next breakpoint.

  let debuggerSetup evaluator state = do
        Gtk.forestStoreClear stateStore

        Gtk.widgetSetSensitive playButton False
        Gtk.widgetSetSensitive stopButton True
        Gtk.textViewSetEditable codeTextView False
        Gtk.containerRemove rightScrolledWindow syntaxTreeView
        Gtk.containerAdd rightScrolledWindow stateView
        Gtk.widgetShowAll rightScrolledWindow

        writeIORef evaluatorAndStateRef (Just (evaluator, state))

  let debuggerCleanup = do
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

        Gtk.widgetSetSensitive playButton True
        Gtk.widgetSetSensitive stopButton False
        Gtk.textViewSetEditable codeTextView True
        Gtk.containerRemove rightScrolledWindow stateView
        Gtk.containerAdd rightScrolledWindow syntaxTreeView
        Gtk.widgetShowAll rightScrolledWindow

        writeIORef evaluatorAndStateRef Nothing

  Gtk.onButtonClicked playButton $ do
    whenM (isNothing <$> readIORef evaluatorAndStateRef) $ do
      Just syntaxTree <- readIORef syntaxTreeRef
      let evaluator = evalDevin syntaxTree
      state <- makePredefinedState
      debuggerSetup evaluator state

    putMVar debuggerCond ()

  Gtk.onButtonClicked stopButton debuggerCleanup

  -- Start the parse and type check worker thread:

  forkIO $ forever $ do
    takeMVar parseAndTypeCheckCond

    text <- Gdk.postGUISync $ do
      (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
      Gtk.textBufferGetText codeBuffer startIter endIter True

    -- Run the parser
    let parser = liftA2 (,) Parsers.devin getState
    parserResult <- runParserT parser [] "" (0, text)

    case parserResult of
      -- Parser failure:
      Left parseError -> do
        let offset = toOffset (errorPos parseError) text
        let messages = errorMessages parseError

        Gdk.postGUIASync $ do
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
          Gtk.seqStoreClear logStore
          Gtk.seqStoreAppend logStore datum
          Gtk.forestStoreClear syntaxTreeStore

      -- Parser success:
      Right (syntaxTree, comments) -> do
        Gdk.postGUIASync $ do
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
          Gtk.forestStoreClear syntaxTreeStore
          Gtk.forestStoreInsertForest syntaxTreeStore rootPath -1 forest
          Gtk.treeViewExpandAll syntaxTreeView

        -- Run the type checker
        let typer = checkDevin syntaxTree
        let typerResult = runTyper typer predefinedEnv

        case typerResult of
          -- Type checker success:
          ((), env, []) -> do
            let (hasMain, _, _) = runTyper checkHasMain env

            Gdk.postGUIASync $ do
              Gtk.widgetSetSensitive playButton hasMain
              Gtk.seqStoreClear logStore

            where
              checkHasMain = lookupFunSignature "main" >>= \case
                Just (([], _), _) -> pure True
                _ -> pure False

          -- Type checker failure:
          ((), _, errors) -> Gdk.postGUIASync $ do
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

  -- Start the debugger worker thread:

  let updateStateStore forest = do
        rootPath <- Gtk.treePathNew
        Gtk.forestStoreClear stateStore
        Gtk.forestStoreInsertForest stateStore rootPath -1 forest
        Gtk.treeViewExpandAll stateView

  forkIO $ forever $ do
    takeMVar debuggerCond
    evaluatorAndState <- Gdk.postGUISync (readIORef evaluatorAndStateRef)

    whenJust evaluatorAndState $ \(evaluator, state) -> do
      (result, state') <- runEvaluatorStep evaluator state

      case result of
        Done _ ->
          Gdk.postGUIASync debuggerCleanup

        Yield statement evaluator' | BreakpointStatement {} <- statement -> do
          forest <- stateForest state'

          Gdk.postGUIASync $ do
            writeIORef evaluatorAndStateRef (Just (evaluator', state'))

            (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
            Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

            highlightInterval (highlightTag tags) codeBuffer statement

            updateStateStore forest

            Gtk.widgetSetSensitive playButton True

        Yield _ evaluator' -> Gdk.postGUIASync $
          whenM (isJust <$> readIORef evaluatorAndStateRef) $ do
            writeIORef evaluatorAndStateRef (Just (evaluator', state'))
            putMVar debuggerCond ()

        Error error -> do
          forest <- stateForest state'

          Gdk.postGUIASync $ do
            startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
            endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
            Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

            updateStateStore forest

            -- Display the error message.
            --
            -- gi-gtk doesn't provide bindings for gtk_message_dialog_new(). To
            -- get around this limitation, we use Gtk.Builder to load a
            -- Gtk.MessageDialog from an XML string. This is ugly, but it works.

            (line, column) <- getLineColumn startIter
            let t = showsLineColumn (line, column) (' ' : display error)
            let t' = escapeXml ("<tt>" ++ escapeXml t ++ "</tt>")

            let string =
                  "<interface>\n\
                  \  <object class=\"GtkMessageDialog\" id=\"dialog\">\n\
                  \    <property name=\"title\">Error</property>\n\
                  \    <property name=\"message-type\">error</property>\n\
                  \    <property name=\"buttons\">close</property>\n\
                  \    <property name=\"use-markup\">true</property>\n\
                  \    <property name=\"text\">" ++ t' ++ "</property>\n\
                  \  </object>\n\
                  \</interface>\0"

            let buildFn = Gtk.getObject Gtk.MessageDialog "dialog"
            builder <- Gtk.builderNewFromString (Text.pack string) -1
            dialog <- Gtk.buildWithBuilder buildFn builder
            Gtk.windowSetTransientFor dialog (Just window)
            Gtk.dialogRun dialog

            -- Cleanup:

            Gtk.widgetDestroy dialog
            Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter
            debuggerCleanup

  -- Display the UI:

  Gtk.widgetShowAll window


setUpTwoColumns ::
  (Gtk.IsTypedTreeModel model, Gtk.IsTreeModel (model (Text, Text)), Gtk.IsTreeView a, MonadIO m) =>
  model (Text, Text) -> a -> m ()
setUpTwoColumns model view = for_ [fst, snd] $ \f -> do
  cellRenderer <- Gtk.cellRendererTextNew
  Gtk.setCellRendererTextFamily cellRenderer "monospace"

  viewColumn <- Gtk.treeViewColumnNew
  Gtk.cellLayoutPackStart viewColumn cellRenderer True

  Gtk.cellLayoutSetDataFunction viewColumn cellRenderer model $ \row ->
    Gtk.setCellRendererTextText cellRenderer (f row)

  Gtk.treeViewAppendColumn view viewColumn


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
