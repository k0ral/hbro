{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, TemplateHaskell #-}
-- | Rewrite many 'Graphics.UI.Gtk.WebKit.WebView' functions in a monadic way.
-- Designed to be imported as @qualified@.
module Hbro.Webkit.WebView where

-- {{{ Imports
import Hbro.Error
import Hbro.Network
--import Hbro.Types
import Hbro.Util

import Control.Monad.Base
import Control.Monad.Error  hiding(forM_, mapM_)
-- import Control.Monad.Reader hiding(forM_, mapM_)

-- import Data.Foldable (forM_, mapM_)
-- import Data.Functor

import Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.General.General as GTK
import qualified Graphics.UI.Gtk.WebKit.Download as W
import Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest)
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as W
import Graphics.UI.Gtk.WebKit.WebView (WebView)
import qualified Graphics.UI.Gtk.WebKit.WebView as W

import Network.URI as N hiding(parseURI, parseURIReference)

import Prelude hiding(mapM_)

import System.Glib.Attributes
import System.Glib.Signals
-- }}}


init :: (MonadBase IO m) => WebView -> m ()
init webView = io $ do
    set webView [ widgetCanDefault := True ]
    void . on webView W.closeWebView $ GTK.mainQuit >> return False

-- {{{ Monad-agnostic version of various WebKit functions
getUri :: (MonadBase IO m, MonadError HError m) => WebView -> m URI
getUri = maybe (throwError InvalidPageURI) parseURI <=< io . W.webViewGetUri

getTitle :: (MonadBase IO m, MonadError HError m) => WebView -> m String
getTitle = maybe (throwError InvalidPageTitle) return <=< io . W.webViewGetTitle

getIconUri :: (MonadBase IO m, MonadError HError m) => WebView -> m URI
getIconUri = maybe (throwError InvalidIconURI) parseURI <=< io . W.webViewGetUri

networkRequestGetUri :: NetworkRequest -> forall m. (MonadBase IO m, MonadError HError m) => m URI
networkRequestGetUri r = parseURIReference =<< maybe (throwError $ EmptyRequestURI r) return =<< io (W.networkRequestGetUri r)

downloadGetUri :: (MonadBase IO m, MonadError HError m) => W.Download -> m URI
downloadGetUri d = parseURI =<< maybe (throwError $ EmptyDownloadURI d) return =<< io (W.downloadGetUri d)

downloadGetSuggestedFilename :: (MonadBase IO m, MonadError HError m) => W.Download -> m String
downloadGetSuggestedFilename d = maybe (throwError $ EmptySuggestedFileName d) return =<< io (W.downloadGetSuggestedFilename d)
-- }}}
