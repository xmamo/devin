{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Prelude hiding (getLine)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.IORef
import Data.Maybe
import Data.String
import System.Exit

import Data.Tree

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Text.Parsec.Error

import Control.Monad.Extra

import Data.GI.Base
import Data.GI.Base.GObject
import qualified GI.GObject as G
import qualified GI.GLib as G
import qualified GI.Gio as G
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as GtkSource
import Data.GI.Gtk.ModelView.CellLayout
import Data.GI.Gtk.ModelView.ForestStore
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.Threading

import Devin.Display
import Devin.Evaluator
import Devin.Evaluators
import Devin.Interval
import Devin.Parsec hiding (Error, try)
import Devin.Parsers
import Devin.Syntax
import Devin.Typer
import Devin.Typers

import Devin.Debug.Evaluator
import Devin.Debug.Syntax
import Devin.Highlight
import Devin.Highlight.Brackets
import Devin.Highlight.Syntax
import Devin.Levenshtein


main :: IO ()
main = Gtk.applicationNew Nothing [G.ApplicationFlagsDefaultFlags] >>= \case
  Nothing -> exitFailure

  Just application -> do
    G.onApplicationActivate application onActivate
    status <- G.applicationRun application Nothing
    when (status /= 0) (exitWith (ExitFailure (fromIntegral status)))


onActivate :: (Gtk.IsApplication a, ?self :: a) => G.ApplicationActivateCallback
onActivate = do
  userConfigDirName <- G.getUserConfigDir
  configDirName <- G.buildFilenamev [userConfigDirName, "devin"]
  codeFileName <- G.buildFilenamev [configDirName, "main.devin"]

  -- Add a fallback .monospace class for systems that lack it, such as KDE:

  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData cssProvider ".monospace { font-family: monospace; }"

  displayManager <- Gdk.displayManagerGet
  displays <- Gdk.displayManagerListDisplays displayManager

  for_ displays $ \display -> do
    screen <- Gdk.displayGetDefaultScreen display
    let priority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_FALLBACK
    Gtk.styleContextAddProviderForScreen screen cssProvider priority

  -- Build the UI:

  let iconName = Just "media-playback-stop-symbolic"
  let iconSize = fromIntegral (fromEnum Gtk.IconSizeButton)
  stopButton <- Gtk.buttonNewFromIconName iconName iconSize
  Gtk.widgetSetSensitive stopButton False

  let iconName = Just "media-playback-start-symbolic"
  let iconSize = fromIntegral (fromEnum Gtk.IconSizeButton)
  playButton <- Gtk.buttonNewFromIconName iconName iconSize
  Gtk.widgetSetSensitive playButton False

  codeBuffer <- GtkSource.bufferNew (Nothing @Gtk.TextTagTable)
  GtkSource.bufferSetHighlightSyntax codeBuffer False
  GtkSource.bufferSetHighlightMatchingBrackets codeBuffer False

  codeView <- GtkSource.viewNewWithBuffer codeBuffer
  GtkSource.viewSetShowLineNumbers codeView True
  GtkSource.viewSetAutoIndent codeView True
  GtkSource.viewSetTabWidth codeView 4
  Gtk.textViewSetMonospace codeView True

  syntaxTreeModel <- forestStoreNew []
  syntaxTreeView <- Gtk.treeViewNewWithModel syntaxTreeModel
  Gtk.treeViewSetHeadersVisible syntaxTreeView False
  Gtk.treeViewSetEnableSearch syntaxTreeView False
  Gtk.treeViewSetGridLines syntaxTreeView Gtk.TreeViewGridLinesVertical
  addMonospaceClass syntaxTreeView

  stateModel <- forestStoreNew []
  stateView <- Gtk.treeViewNewWithModel stateModel
  Gtk.treeViewSetHeadersVisible stateView False
  Gtk.treeViewSetEnableSearch stateView False
  Gtk.treeViewSetGridLines stateView Gtk.TreeViewGridLinesVertical
  addMonospaceClass stateView

  logModel <- seqStoreNew []
  logView <- Gtk.treeViewNewWithModel logModel
  Gtk.treeViewSetHeadersVisible logView False
  Gtk.treeViewSetEnableSearch logView False
  addMonospaceClass logView

  blankView <- Gtk.treeViewNew
  Gtk.treeViewSetHeadersVisible blankView False
  Gtk.treeViewSetEnableSearch blankView False
  Gtk.treeViewSetGridLines blankView Gtk.TreeViewGridLinesVertical
  addMonospaceClass blankView

  let adjustment = Nothing @Gtk.Adjustment
  codeScrolledWindow <- Gtk.scrolledWindowNew adjustment adjustment
  Gtk.containerAdd codeScrolledWindow codeView

  let adjustment = Nothing @Gtk.Adjustment
  rightScrolledWindow <- Gtk.scrolledWindowNew adjustment adjustment
  Gtk.containerAdd rightScrolledWindow blankView

  let adjustment = Nothing @Gtk.Adjustment
  logScrolledWindow <- Gtk.scrolledWindowNew adjustment adjustment
  Gtk.containerAdd logScrolledWindow logView

  horizontalPaned <- Gtk.panedNew Gtk.OrientationHorizontal
  Gtk.panedPack1 horizontalPaned codeScrolledWindow True False
  Gtk.panedPack2 horizontalPaned rightScrolledWindow True False

  verticalPaned <- Gtk.panedNew Gtk.OrientationVertical
  Gtk.panedPack1 verticalPaned horizontalPaned True False
  Gtk.panedPack2 verticalPaned logScrolledWindow False False

  headerBar <- Gtk.headerBarNew
  Gtk.headerBarSetTitle headerBar (Just "Devin")
  Gtk.headerBarSetShowCloseButton headerBar True
  Gtk.containerAdd headerBar stopButton
  Gtk.containerAdd headerBar playButton

  window <- Gtk.applicationWindowNew ?self
  Gtk.windowSetDefaultSize window 1024 576
  Gtk.windowSetTitlebar window (Just headerBar)
  Gtk.containerAdd window verticalPaned

  -- Set up the columns for syntaxTreeView, stateView, logView:

  appendColumnWithDataFunction syntaxTreeView syntaxTreeModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (fst row)

  appendColumnWithDataFunction syntaxTreeView syntaxTreeModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (snd row)

  appendColumnWithDataFunction stateView stateModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (fst row)

  appendColumnWithDataFunction stateView stateModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (snd row)

  appendColumnWithDataFunction logView logModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (fst row)

  appendColumnWithDataFunction logView logModel $ \renderer row ->
    Gtk.setCellRendererTextText renderer (snd row)

  -- Set up the the tag table. This is needed for syntax highlighting.

  styleScheme <- GtkSource.bufferGetStyleScheme codeBuffer
  tags <- getHighlightingTags styleScheme
  tagTable <- Gtk.textBufferGetTagTable codeBuffer
  addHighlightingTags tagTable tags

  -- Connect codeBuffer callbacks for "changed" and "notify::cursor-position"
  -- signals. The former callback signals that parsing, type checking, and
  -- syntax highlighting need to be performed again; the latter takes care of
  -- highlighting matching braces.

  syntaxTreeRef <- newIORef Nothing
  parseAndTypeCheckCond <- newEmptyMVar

  Gtk.onTextBufferChanged codeBuffer $ void $ do
    Gtk.widgetSetSensitive playButton False
    tryPutMVar parseAndTypeCheckCond ()

  G.onObjectNotify codeBuffer (Just "cursor-position") $ const $ do
    (startIter, endIter) <- Gtk.textBufferGetBounds ?self
    Gtk.textBufferRemoveTag ?self (bracketTag tags) startIter endIter

    whenJustM (readIORef syntaxTreeRef) $ \syntaxTree -> void $ do
      insertMark <- Gtk.textBufferGetInsert ?self
      insertIter <- Gtk.textBufferGetIterAtMark ?self insertMark
      highlightDevinBrackets ?self tags insertIter syntaxTree

  -- Connect playButton and stopButton callbacks for "clicked" signals.
  --
  -- The play button exhibits different behavior depending on context:
  -- initially, its function is to start the debugging process; pressing it
  -- again will advance the debugger to the next breakpoint.
  --
  -- While debugging, the window is switched to a different state where the
  -- syntax tree preview is replaced by the evaluator state.

  evaluatorAndStateRef <- newIORef Nothing
  debuggerCond <- newEmptyMVar

  Gtk.onButtonClicked playButton $ void $ do
    Gtk.widgetSetSensitive playButton False
    Gtk.widgetSetSensitive stopButton True

    readIORef evaluatorAndStateRef >>= \case
      Nothing -> do
        Gtk.textViewSetEditable codeView False
        setChild rightScrolledWindow stateView

        -- Save Devin code to $XDG_CONFIG_HOME/devin/main.devin
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        code <- Gtk.textIterGetText startIter endIter
        G.mkdirWithParents configDirName 0o777
        try @IOException (Text.writeFile codeFileName code)

        -- Set up the evaluator and its state
        Just syntaxTree <- readIORef syntaxTreeRef
        let evaluator = evalDevin syntaxTree
        state <- makePredefinedState
        writeIORef evaluatorAndStateRef (Just (evaluator, state))

      Just _ -> do
        -- Clear previous highlighting
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

    -- Signal to start or resume debugging
    tryPutMVar debuggerCond ()

  Gtk.onButtonClicked stopButton $ void $ do
    Gtk.widgetSetSensitive playButton False
    Gtk.widgetSetSensitive stopButton False
    writeIORef evaluatorAndStateRef Nothing
    tryPutMVar debuggerCond ()

  -- Connect window callback for "delete-event" signals, which saves the current
  -- Devin code to $XDG_CONFIG_HOME/devin/main.devin:

  Gtk.onWidgetDeleteEvent window $ const $ do
    (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
    code <- Gtk.textIterGetText startIter endIter
    G.mkdirWithParents configDirName 0o777
    try @IOException (Text.writeFile codeFileName code)
    pure Gdk.EVENT_PROPAGATE

  -- Load Devin code from $XDG_CONFIG_HOME/devin/main.devin:

  try @IOException $ do
    code <- Text.readFile codeFileName
    Gtk.textBufferSetText codeBuffer code (fromIntegral (Text.length code))

    startIter <- Gtk.textBufferGetStartIter codeBuffer
    Gtk.textBufferPlaceCursor codeBuffer startIter

  -- Start the worker thread responsible for parsing, type checking, and
  -- performing syntax highlighting:

  var1 <- newEmptyMVar
  var2 <- newEmptyMVar
  var3 <- newEmptyMVar

  forkOS $ forever $ do
    readMVar parseAndTypeCheckCond

    postGUIASync $ do
      (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
      code <- Gtk.textIterGetText startIter endIter
      takeMVar parseAndTypeCheckCond
      putMVar var1 code

    code <- takeMVar var1

    -- Run the parser
    parserResult <- parseT devin "" (0, code)

    case parserResult of
      -- Parser failure:
      Left parseError -> do
        offset <- toOffsetT (errorPos parseError) code
        let messages = errorMessages parseError

        postGUIASync $ do
          list <- seqStoreToList logModel
          startIter <- Gtk.textBufferGetIterAtOffset codeBuffer offset
          (line, column) <- getLineColumn startIter
          putMVar var2 (list, (line, column))

        (list, (line, column)) <- takeMVar var2
        let list' = [(displayLineColumn (line, column), showMessages messages)]
        let edits = diff list list'

        postGUIASync $ do
          writeIORef syntaxTreeRef Nothing

          -- Clear previous highlighting
          (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
          clearSyntaxHighlighting codeBuffer tags startIter endIter
          clearBracketsHighlighting codeBuffer tags startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

          -- Highlight as error, starting from the parseError offset
          startIter <- Gtk.textBufferGetIterAtOffset codeBuffer offset
          Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

          -- Hide the syntax tree preview
          setChild rightScrolledWindow blankView

          -- Update the error log
          patchSeqStore logModel edits
          Gtk.treeViewColumnsAutosize logView

      -- Parser success:
      Right (syntaxTree, comments) -> do
        postGUIASync $ do
          forest <- forestStoreGetForest syntaxTreeModel
          putMVar var3 forest

        forest <- takeMVar var3
        let forest' = map definitionTree (definitions syntaxTree)
        let edits = forestDiff forest forest'

        postGUIASync $ do
          writeIORef syntaxTreeRef (Just syntaxTree)

          -- Clear previous highlighting
          (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
          clearSyntaxHighlighting codeBuffer tags startIter endIter
          clearBracketsHighlighting codeBuffer tags startIter endIter
          Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter

          -- Highlight syntax
          highlightDevin codeBuffer tags syntaxTree
          for_ comments (highlightInterval (commentTag tags) codeBuffer)

          -- Highlight matching braces
          insertMark <- Gtk.textBufferGetInsert codeBuffer
          insertIter <- Gtk.textBufferGetIterAtMark codeBuffer insertMark
          highlightDevinBrackets codeBuffer tags insertIter syntaxTree

          -- Show and update the syntax tree preview
          setChild rightScrolledWindow syntaxTreeView
          let expandPredicate (label, _) = not (Text.isSuffixOf "Expression" label)
          patchForestStore syntaxTreeModel edits syntaxTreeView expandPredicate
          Gtk.treeViewColumnsAutosize syntaxTreeView

          -- Clear the error log
          seqStoreClear logModel
          Gtk.treeViewColumnsAutosize logView

        -- Run the type checker
        let typerResult = runTyper (checkDevin syntaxTree) predefinedEnv

        case typerResult of
          -- Type checker success:
          ((), env, []) -> do
            let (hasMain, _, _) = runTyper checkHasMain env
            postGUIASync (Gtk.widgetSetSensitive playButton hasMain)

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
            let datum = (displayLineColumn (line, column), Text.pack (display error))
            seqStoreAppend logModel datum
            Gtk.treeViewColumnsAutosize logView

  -- Start worker thread responsible for Devin code evaluation:

  var1 <- newEmptyMVar
  var2 <- newEmptyMVar

  forkOS $ forever $ do
    readMVar debuggerCond

    postGUIASync $ do
      evaluatorAndState <- readIORef evaluatorAndStateRef
      takeMVar debuggerCond
      putMVar var1 evaluatorAndState

    evaluatorAndState <- takeMVar var1

    case evaluatorAndState of
      Nothing -> postGUIASync $ do
        -- Clear previous highlighting
        (startIter, endIter) <- Gtk.textBufferGetBounds codeBuffer
        Gtk.textBufferRemoveTag codeBuffer (highlightTag tags) startIter endIter

        -- Clear the evaluator state preview
        forestStoreClear stateModel

        -- Revert back to the initial window state
        Gtk.widgetSetSensitive playButton True
        Gtk.widgetSetSensitive stopButton False
        Gtk.textViewSetEditable codeView True
        setChild rightScrolledWindow syntaxTreeView

      Just (evaluator, state) -> do
        -- Evaluate a single statement
        (result, state') <- runEvaluatorStep evaluator state

        case result of
          -- Evaluator done:
          Done _ -> postGUIASync $ void $ do
            writeIORef evaluatorAndStateRef Nothing
            tryPutMVar debuggerCond ()

          -- Evaluator yield (breakpoint statement):
          Yield statement evaluator' | BreakpointStatement{} <- statement -> do
            postGUIASync $ do
              forest <- forestStoreGetForest stateModel
              putMVar var2 forest

            forest <- takeMVar var2
            forest' <- stateForest state'
            let edits = forestDiff forest forest'

            postGUIASync $ do
              -- Highlight the breakpoint statement
              highlightInterval (highlightTag tags) codeBuffer statement

              -- Update the evaluator state preview
              patchForestStore stateModel edits stateView (const True)
              Gtk.treeViewColumnsAutosize stateView

              -- Enable the play button to resume execution
              writeIORef evaluatorAndStateRef (Just (evaluator', state'))
              Gtk.widgetSetSensitive playButton True

          -- Evaluator yield (any other statement):
          Yield _ evaluator' -> postGUIASync $
            whenM (isJust <$> readIORef evaluatorAndStateRef) $ void $ do
              writeIORef evaluatorAndStateRef (Just (evaluator', state'))
              tryPutMVar debuggerCond ()

          -- Evaluator error:
          Error error -> do
            forest' <- stateForest state'

            postGUIASync $ void $ do
              -- Disable the stop button
              Gtk.widgetSetSensitive stopButton False

              -- Highlight the segment of code which caused the error
              startIter <- Gtk.textBufferGetIterAtOffset codeBuffer (start error)
              endIter <- Gtk.textBufferGetIterAtOffset codeBuffer (end error)
              Gtk.textBufferApplyTag codeBuffer (errorTag tags) startIter endIter

              -- Update the evaluator state preview
              forest <- forestStoreGetForest stateModel
              let edits = forestDiff forest forest'
              patchForestStore stateModel edits stateView (const True)
              Gtk.treeViewColumnsAutosize stateView

              -- Display the error message.
              --
              -- gi-gtk doesn't provide bindings for gtk_message_dialog_new().
              -- To get around this, we use new' from haskell-gi-base.

              (line, column) <- getLineColumn startIter
              let string = displaysLineColumn (line, column) (' ' : display error)

              headerBar <- Gtk.headerBarNew
              Gtk.headerBarSetTitle headerBar (Just "Error")
              Gtk.headerBarSetShowCloseButton headerBar True

              messageDialog <- new' Gtk.MessageDialog
                [
                  Gtk.constructMessageDialogMessageType Gtk.MessageTypeError,
                  Gtk.constructMessageDialogButtons Gtk.ButtonsTypeClose,
                  Gtk.constructMessageDialogText (Text.pack string)
                ]

              messageArea <- Gtk.messageDialogGetMessageArea messageDialog
              addMonospaceClass messageArea

              Gtk.windowSetTitlebar messageDialog (Just headerBar)
              Gtk.windowSetTransientFor messageDialog (Just window)
              Gtk.widgetShowAll messageDialog
              Gtk.dialogRun messageDialog

              -- Prepare for cleanup. Since debuggerCond is signalled, the
              -- debugger worker thread should proceed by reverting back to the
              -- initial window state.

              Gtk.widgetDestroy messageDialog
              Gtk.textBufferRemoveTag codeBuffer (errorTag tags) startIter endIter
              writeIORef evaluatorAndStateRef Nothing
              tryPutMVar debuggerCond ()

  -- Display the UI:

  Gtk.widgetShowAll syntaxTreeView
  Gtk.widgetShowAll stateView
  Gtk.widgetShowAll window


addMonospaceClass :: (Gtk.IsWidget a, MonadIO m) => a -> m ()
addMonospaceClass widget = do
  context <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddClass context Gtk.STYLE_CLASS_MONOSPACE


setChild :: (Gtk.IsBin a, Gtk.IsWidget b, MonadIO m) => a -> b -> m ()
setChild bin widget = do
  let bin' = bin `asA` Gtk.Bin
  whenJustM (Gtk.binGetChild bin') (Gtk.containerRemove bin')
  Gtk.containerAdd bin' widget


appendColumnWithDataFunction ::
  (Gtk.IsTreeView a, IsTypedTreeModel model, Gtk.IsTreeModel (model row), MonadIO m) =>
  a -> model row -> (Gtk.CellRendererText -> row -> IO ()) -> m Int32
appendColumnWithDataFunction view model f = do
  renderer <- Gtk.cellRendererTextNew
  column <- Gtk.treeViewColumnNew
  cellLayoutSetDataFunction column renderer model (f renderer)
  Gtk.cellLayoutPackStart column renderer True
  Gtk.treeViewAppendColumn view column


patchSeqStore :: MonadIO m => SeqStore a -> [Edit a] -> m ()
patchSeqStore model = foldM_ f 0
  where
    f i (Copy _ _) =
      pure (i + 1)

    f i (Insert x) = do
      seqStoreInsert model i x
      pure (i + 1)

    f i (Delete _) = do
      seqStoreRemove model i
      pure i

    f i (Replace _ x) = do
      seqStoreInsert model i x
      seqStoreRemove model (i + 1)
      pure (i + 1)


patchForestStore ::
  Gtk.IsTreeView b =>
  ForestStore a -> [TreeEdit a] -> b -> (a -> Bool) -> IO ()
patchForestStore model edits view expandPredicate = do
  path <- Gtk.treePathNew
  go path 0 edits

  where
    go _ _ [] =
      pure ()

    go path i (TreeCopy _ _ : edits) =
      go path (i + 1) edits

    go path i (TreeInsert tree : edits) = do
      forestStoreInsertTree model path (fromIntegral i) tree
      Gtk.treePathAppendIndex path i
      expand path
      Gtk.treePathUp path
      go path (i + 1) edits

    go path i (TreeDelete _ : edits) = do
      Gtk.treePathAppendIndex path i
      forestStoreRemove model path
      Gtk.treePathUp path
      go path i edits

    go path i (TreeReplace _ tree : edits) = do
      forestStoreInsertTree model path (fromIntegral i) tree
      Gtk.treePathAppendIndex path i
      expand path
      Gtk.treePathNext path
      forestStoreRemove model path
      Gtk.treePathUp path
      go path (i + 1) edits

    go path i (TreeUpdate _ edits' : edits) = do
      Gtk.treePathAppendIndex path i
      go path 0 edits'
      Gtk.treePathUp path
      go path (i + 1) edits

    expand path = do
      tree <- forestStoreGetTree model path

      when (expandPredicate (rootLabel tree)) $ void $ do
        Gtk.treeViewExpandRow view path False
        Gtk.treePathDown path

        for_ (subForest tree) $ const $ do
          expand path
          Gtk.treePathNext path

        Gtk.treePathUp path


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


getLineColumn :: (Num a, MonadIO m) => Gtk.TextIter -> m (a, a)
getLineColumn iter = do
  line <- getLine iter
  column <- getColumn iter
  pure (line, column)


displayLineColumn :: (Show a, IsString b) => (a, a) -> b
displayLineColumn (line, column) = fromString (displaysLineColumn (line, column) "")


displaysLineColumn :: Show a => (a, a) -> ShowS
displaysLineColumn (line, column) =
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
