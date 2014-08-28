{-# LANGUAGE TemplateHaskell #-}
module Hbro.Hooks
    ( ResourceAction(..)
    , LinkClickedHook(..)
    , LoadRequestedHook(..)
    , NewWindowHook(..)
    , ResourceOpenedHook(..)
    , TitleChangedHook(..)
    , Hooks
    , keyHooksL
    , promptHooksL
    , webViewHooksL
    , initialize
    , routines
) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Gui.PromptBar.Hooks
import qualified Hbro.Gui.PromptBar.Hooks   as Prompt
import           Hbro.Gui.PromptBar.Signals hiding (Signals)
import qualified Hbro.IPC.Hooks             as IPC
import qualified Hbro.Keys                  as Keys
import qualified Hbro.Keys.Signals          as Keys
import           Hbro.Logger                hiding (initialize)
import           Hbro.Prelude
import           Hbro.Signals               hiding (initialize)
import           Hbro.WebView.Hooks         hiding (Hooks)
import qualified Hbro.WebView.Hooks         as WebView (Hooks, initHooks)
import           Hbro.WebView.Signals       hiding (Signals)

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter
import           Control.Lens.TH
import           Control.Monad.Reader
-- }}}


-- {{{ Hooks
data Hooks m = Hooks
    { _webViewHooks :: WebView.Hooks m
    , _keyHooks     :: Keys.Hooks m
    , _ipcHooks     :: TVar (IPC.Hooks m)
    , _promptHooks  :: PromptHooks m
    }

makeLensesWith ?? ''Hooks $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

initialize :: (Functor n, BaseIO m, Default (Keys.Status n), Default (LinkClickedHook n),
              Default (LoadRequestedHook n), Default (NewWindowHook n),
              Default (ResourceOpenedHook n), Default (TitleChangedHook n),
              Default (IPC.Hooks n))
          => m (Hooks n)
initialize = io (Hooks <$> WebView.initHooks
                      <*> Keys.initializeHooks
                      <*> newTVarIO def
                      <*> Prompt.initHooks)

routines :: (ControlIO m) => a -> Signals -> Hooks (ExceptT Text (ReaderT a m)) -> [m ()]
routines globalContext signals hooks =
-- WebView
    [ dequeueRoutine globalContext      (signals^._webViewSignals.downloadL)       $ hooks^.webViewHooksL.onDownloadL
    , dequeueRoutine globalContext      (signals^._webViewSignals.linkHoveredL)    $ hooks^.webViewHooksL.onLinkHoveredL
    , dequeueRoutine globalContext      (signals^._webViewSignals.linkClickedL)    $ hooks^.webViewHooksL.onLinkClickedL
    , dequeueRoutine globalContext      (signals^._webViewSignals.loadFinishedL)   $ hooks^.webViewHooksL.onLoadFinishedL
    , dequeueRoutine globalContext      (signals^._webViewSignals.loadRequestedL)  $ hooks^.webViewHooksL.onLoadRequestedL
    , dequeueRoutine globalContext      (signals^._webViewSignals.loadStartedL)    $ hooks^.webViewHooksL.onLoadStartedL
    , dequeueRoutine globalContext      (signals^._webViewSignals.newWindowL)      $ hooks^.webViewHooksL.onNewWindowL
    , dequeueRoutine globalContext      (signals^._webViewSignals.resourceOpenedL) $ hooks^.webViewHooksL.onResourceOpenedL
    , dequeueRoutine globalContext      (signals^._webViewSignals.titleChangedL)   $ hooks^.webViewHooksL.onTitleChangedL
-- Keys
    , Keys.dequeue globalContext  (signals^._webViewSignals.keyPressedL) $ hooks^.keyHooksL
-- IPC
    , IPC.dequeue globalContext   (signals^._ipcSignals) $ hooks^.ipcHooksL
-- Prompt
    , dequeueRoutine globalContext      (signals^._promptSignals.cancelledL) $ hooks^.promptHooksL.onCancelledL
    , dequeueRoutine globalContext      (signals^._promptSignals.changedL)   $ hooks^.promptHooksL.onChangedL
    , dequeueRoutine globalContext      (signals^._promptSignals.validatedL) $ hooks^.promptHooksL.onValidatedL
    ]
  -- where dequeueRoutine globalContext = dequeueRoutine globalContext


dequeueRoutine :: (ControlIO m, Describable s)
               => a -> TMVar s -> TMVar (s -> ExceptT Text (ReaderT a m) ()) -> m ()
dequeueRoutine globalContext signal hook = forever $ do
    arguments <- atomically $ takeTMVar signal
    debugM "hbro.hooks" $ "Signal acknowledged [" ++ describe arguments ++ "]."

    (`runReaderT` globalContext) . logErrors' $ do
        f <- (atomically $ tryReadTMVar hook) <!> ("Undefined hook: " ++ describe arguments)
        handleIO (throwError . tshow) $ f arguments
-- }}}
