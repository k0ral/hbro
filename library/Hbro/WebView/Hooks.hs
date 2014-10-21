{-# LANGUAGE TemplateHaskell #-}
module Hbro.WebView.Hooks (
-- * Types
      DownloadHook(..)
    , LinkClickedHook(..)
    , LoadRequestedHook(..)
    , NewWindowHook(..)
    , ResourceOpenedHook(..)
    , TitleChangedHook(..)
    , Hooks
    , onDownloadL
    , onLinkClickedL
    , onLinkHoveredL
    , onLoadRequestedL
    , onLoadStartedL
    , onLoadFinishedL
    , onNewWindowL
    , onResourceOpenedL
    , onTitleChangedL
    , HasHooks(..)
-- * Functions
    , set
    , initHooks
) where

-- {{{ Imports
import           Hbro.Prelude
import           Hbro.WebView.Signals

import           Control.Lens.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
-- }}}


newtype DownloadHook m       = DownloadHook (Download -> m ())
newtype LinkClickedHook m    = LinkClickedHook (LinkClicked -> m ())
newtype LoadRequestedHook m  = LoadRequestedHook (LoadRequested -> m ())
newtype NewWindowHook m      = NewWindowHook (NewWindow -> m ())
newtype ResourceOpenedHook m = ResourceOpenedHook (ResourceOpened -> m ResourceAction)
newtype TitleChangedHook m   = TitleChangedHook (TitleChanged -> m ())

declareLenses [d|
  data Hooks m = Hooks
    { onDownloadL       :: TMVar (Download -> m ())
    , onLinkClickedL    :: TMVar (LinkClicked -> m ())
    , onLinkHoveredL    :: TMVar (LinkHovered -> m ())
    , onLoadRequestedL  :: TMVar (LoadRequested -> m ())
    , onLoadStartedL    :: TMVar (LoadStarted -> m ())
    , onLoadFinishedL   :: TMVar (LoadFinished -> m ())
    , onNewWindowL      :: TMVar (NewWindow -> m ())
    , onResourceOpenedL :: TMVar (ResourceOpened -> m ())
    , onTitleChangedL   :: TMVar (TitleChanged -> m ())
    }
  |]

class HasHooks n t | t -> n where _hooks :: Lens' t (Hooks n)

set :: (BaseIO m, MonadReader r m, HasHooks n r) => Lens' (Hooks n) (TMVar a) -> a -> m ()
set l v = atomically . (`writeTMVar` v) =<< askL (_hooks.l)


initHooks :: (Functor n, BaseIO m, Default (LinkClickedHook n), Default (LoadRequestedHook n), Default (NewWindowHook n), Default (ResourceOpenedHook n), Default (TitleChangedHook n))
          => m (Hooks n)
initHooks = io (Hooks <$> newEmptyTMVarIO
                      <*> newTMVarIO lc
                      <*> newEmptyTMVarIO
                      <*> newTMVarIO lr
                      <*> newEmptyTMVarIO
                      <*> newEmptyTMVarIO
                      <*> newTMVarIO nw
                      <*> newTMVarIO (void . ro)
                      <*> newTMVarIO tc)
  where
      LinkClickedHook    lc = def
      LoadRequestedHook  lr = def
      NewWindowHook      nw = def
      ResourceOpenedHook ro = def
      TitleChangedHook   tc = def
