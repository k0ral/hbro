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
import Hbro.Error
import qualified Hbro.Keys as Keys
import qualified Hbro.Keys.Signals as Keys
import qualified Hbro.IPC.Hooks as IPC
import Hbro.Signals hiding(initialize)
import Hbro.Gui.PromptBar.Signals hiding(Signals)
import Hbro.Gui.PromptBar.Hooks
import qualified Hbro.Gui.PromptBar.Hooks as Prompt
import Hbro.Util
import Hbro.WebView.Hooks hiding(Hooks)
import qualified Hbro.WebView.Hooks as WebView (Hooks, initHooks)
import Hbro.WebView.Signals hiding(Signals)

import Control.Concurrent.STM
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad.Reader

-- import Network.URI (URI)
-- }}}


-- {{{ Hooks
data Hooks m = Hooks
    { _webViewHooks     :: WebView.Hooks m
    , _keyHooks         :: Keys.Hooks m
    , _ipcHooks         :: TVar (IPC.Hooks m)
    , _promptHooks      :: PromptHooks m
    }

makeLensesWith ?? ''Hooks $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

data UndefinedHook = UndefinedHook String deriving(Typeable)
instance Exception UndefinedHook
instance Show UndefinedHook where show (UndefinedHook name) = "Undefined hook: " ++ name


initialize :: (Functor n, MonadBase IO m, Default (Keys.Status n), Default (LinkClickedHook n),
              Default (LoadRequestedHook n), Default (NewWindowHook n),
              Default (ResourceOpenedHook n), Default (TitleChangedHook n),
              Default (IPC.Hooks n))
          => m (Hooks n)
initialize = io (Hooks <$> WebView.initHooks
                      <*> Keys.initializeHooks
                      <*> newTVarIO def
                      <*> Prompt.initHooks)

routines :: (MonadBase IO m, MonadCatch m, MonadThrow m) => a -> Signals -> Hooks (ReaderT a m) -> [m ()]
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


dequeueRoutine :: (MonadBase IO m, MonadThrow m, MonadCatch m, Show s)
               => a -> TMVar s -> TMVar (s -> ReaderT a m ()) -> m ()
dequeueRoutine globalContext signal hook = forever $ do
    arguments <- io . atomically $ takeTMVar signal
    io . debugM "hbro.hooks" $ "Signal acknowledged [" ++ show arguments ++ "]."

    logErrors $ do
        f <- (io . atomically $ tryReadTMVar hook) `failWithM` UndefinedHook (show arguments)
        (`runReaderT` globalContext) $ f arguments
-- }}}
