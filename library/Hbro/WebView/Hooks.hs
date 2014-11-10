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
    -- , onResourceOpenedL
    , onTitleChangedL
    , HasHooks(..)
-- * Functions
    , set
    , initHooks
) where

-- {{{ Imports
import           Hbro.Event
import           Hbro.Prelude
import           Hbro.WebView.Signals

import           Control.Lens.Lens
import           Control.Lens.TH
-- }}}


newtype DownloadHook m       = DownloadHook (Download -> m ())
newtype LinkClickedHook m    = LinkClickedHook (LinkClicked -> m ())
newtype LoadRequestedHook m  = LoadRequestedHook (LoadRequested -> m ())
newtype NewWindowHook m      = NewWindowHook (NewWindow -> m ())
newtype ResourceOpenedHook m = ResourceOpenedHook (ResourceOpened -> m ResourceAction)
newtype TitleChangedHook m   = TitleChangedHook (TitleChanged -> m ())

declareLenses [d|
  data Hooks m = Hooks
    { onDownloadL       :: TMVar (Hook m Download)
    , onLinkClickedL    :: TMVar (Hook m LinkClicked)
    , onLinkHoveredL    :: TMVar (Hook m LinkHovered)
    , onLoadRequestedL  :: TMVar (Hook m LoadRequested)
    , onLoadStartedL    :: TMVar (Hook m LoadStarted)
    , onLoadFinishedL   :: TMVar (Hook m LoadFinished)
    , onNewWindowL      :: TMVar (Hook m NewWindow)
    -- , onResourceOpenedL :: TMVar (Hook m ResourceOpened)
    , onTitleChangedL   :: TMVar (Hook m TitleChanged)
    }
  |]

class HasHooks n t | t -> n where _hooks :: Lens' t (Hooks n)

set :: (BaseIO m, MonadReader r m, HasHooks n r) => Lens' (Hooks n) (TMVar a) -> a -> m ()
set l v = atomically . (`writeTMVar` v) =<< askL (_hooks.l)


initHooks :: (Functor n, BaseIO m, Default (Hook n LinkClicked), Default (Hook n LoadRequested), Default (Hook n NewWindow), -- Default (Hook n ResourceOpened),
              Default (Hook n TitleChanged))
          => m (Hooks n)
initHooks = io (Hooks <$> newEmptyTMVarIO
                      <*> newTMVarIO def
                      <*> newEmptyTMVarIO
                      <*> newTMVarIO def
                      <*> newEmptyTMVarIO
                      <*> newEmptyTMVarIO
                      <*> newTMVarIO def
                      -- <*> newTMVarIO def
                      <*> newTMVarIO def)
