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
import Hbro.Error
import Hbro.Util
import Hbro.WebView.Signals

import Control.Concurrent.STM
import Control.Lens.Lens
import Control.Lens.Setter hiding(set)
import Control.Lens.TH
import Control.Monad.Reader
-- }}}


newtype DownloadHook m       = DownloadHook (Download -> m ())
newtype LinkClickedHook m    = LinkClickedHook (LinkClicked -> m ())
newtype LoadRequestedHook m  = LoadRequestedHook (LoadRequested -> m ())
newtype NewWindowHook m      = NewWindowHook (NewWindow -> m ())
newtype ResourceOpenedHook m = ResourceOpenedHook (ResourceOpened -> m ResourceAction)
newtype TitleChangedHook m   = TitleChangedHook (TitleChanged -> m ())


data Hooks m = Hooks
    { _onDownload       :: TMVar (Download -> m ())
    , _onLinkClicked    :: TMVar (LinkClicked -> m ())
    , _onLinkHovered    :: TMVar (LinkHovered -> m ())
    , _onLoadRequested  :: TMVar (LoadRequested -> m ())
    , _onLoadStarted    :: TMVar (LoadStarted -> m ())
    , _onLoadFinished   :: TMVar (LoadFinished -> m ())
    , _onNewWindow      :: TMVar (NewWindow -> m ())
    , _onResourceOpened :: TMVar (ResourceOpened -> m ())
    , _onTitleChanged   :: TMVar (TitleChanged -> m ())
    }

makeLensesWith ?? ''Hooks $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
    -- & lensClass .~ (\name -> Just ("Has" ++ name, map toLower name))

class HasHooks n t | t -> n where _hooks :: Lens' t (Hooks n)

set :: (MonadBase IO m, MonadReader r m, HasHooks n r) => Lens' (Hooks n) (TMVar a) -> a -> m ()
set l v = io . atomically . (`writeTMVar` v) =<< askl (_hooks.l)


initHooks :: (Functor n, MonadBase IO m, Default (LinkClickedHook n), Default (LoadRequestedHook n), Default (NewWindowHook n), Default (ResourceOpenedHook n), Default (TitleChangedHook n))
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
