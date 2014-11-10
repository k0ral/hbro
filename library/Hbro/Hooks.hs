{-# LANGUAGE TemplateHaskell #-}
module Hbro.Hooks
    ( ResourceAction(..)
    , Hooks
    , keyHooksL
    , promptHooksL
    , webViewHooksL
    , initialize
    , routines
) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
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

import           Control.Concurrent.Async.Lifted
import           Control.Lens.Getter
import           Control.Lens.TH
-- }}}


-- {{{ Hooks
declareLenses [d|
  data Hooks m = Hooks
    { webViewHooksL :: WebView.Hooks m
    , keyHooksL     :: Keys.Hooks m
    , ipcHooksL     :: TVar (IPC.Hooks m)
    , promptHooksL  :: PromptHooks m
    }
  |]

initialize :: (Functor n, BaseIO m, Default (Keys.Status n), Default (Hook n LinkClicked),
              Default (Hook n LoadRequested), Default (Hook n NewWindow),
              Default (Hook n TitleChanged), Default (IPC.Hooks n))
           => m (Hooks n)
initialize = Hooks <$> WebView.initHooks
                   <*> Keys.initializeHooks
                   <*> (io $ newTVarIO def)
                   <*> Prompt.initHooks

routines :: (ControlIO m) => a -> Signals -> Hooks (ExceptT Text (ReaderT a m)) -> [m ()]
routines globalContext signals hooks =
-- WebView
    [ dequeueRoutine globalContext      (webView^.downloadL)       $ hooks^.webViewHooksL.onDownloadL
    , dequeueRoutine globalContext      (webView^.linkHoveredL)    $ hooks^.webViewHooksL.onLinkHoveredL
    , dequeueRoutine globalContext      (webView^.linkClickedL)    $ hooks^.webViewHooksL.onLinkClickedL
    , dequeueRoutine globalContext      (webView^.loadFinishedL)   $ hooks^.webViewHooksL.onLoadFinishedL
    , dequeueRoutine globalContext      (webView^.loadRequestedL)  $ hooks^.webViewHooksL.onLoadRequestedL
    , dequeueRoutine globalContext      (webView^.loadStartedL)    $ hooks^.webViewHooksL.onLoadStartedL
    , dequeueRoutine globalContext      (webView^.newWindowL)      $ hooks^.webViewHooksL.onNewWindowL
    -- , dequeueRoutine globalContext      (webView^.resourceOpenedL) $ hooks^.webViewHooksL.onResourceOpenedL
    , dequeueRoutine globalContext      (webView^.titleChangedL)   $ hooks^.webViewHooksL.onTitleChangedL
-- Keys
    , Keys.dequeue globalContext  (webView^.keyPressedL) $ hooks^.keyHooksL
-- IPC
    , IPC.dequeue globalContext   (signals^.ipcSignalsL) $ hooks^.ipcHooksL
-- Prompt
    , dequeueRoutine globalContext      (prompt^.cancelledL) $ hooks^.promptHooksL.onCancelledL
    , dequeueRoutine globalContext      (prompt^.changedL)   $ hooks^.promptHooksL.onChangedL
    , dequeueRoutine globalContext      (prompt^.validatedL) $ hooks^.promptHooksL.onValidatedL
    ]
  where webView = signals^.webViewSignalsL
        prompt  = signals^.promptSignalsL

dequeueRoutine :: (ControlIO m, Event e)
               => a -> Signal e -> TMVar (Hook (ExceptT Text (ReaderT a m)) e) -> m ()
dequeueRoutine globalContext signal hook = forever $ do
    input <- waitFor signal
    debugM "hbro.hooks" $ "Signal acknowledged [" ++ describe signal ++ "]."

    (`runReaderT` globalContext) . logErrors' $ do
        (Hook f) <- (atomically $ tryReadTMVar hook) <!> ("Undefined hook: " ++ describe signal)
        async . handleIO (throwError . tshow) $ f input
