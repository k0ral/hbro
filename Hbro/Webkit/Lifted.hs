-- | Some lifted functions from 'Graphics.UI.Gtk.WebKit'.
-- Designed to be imported as @qualified@.
module Hbro.Webkit.Lifted (
    UnavailableFileName(..),
-- * 'WebView'
    loadHtmlString,
    loadString,
-- * 'NetworkRequest'
    networkRequestGetUri,
-- * 'Download'
    downloadGetUri,
-- * 'NetworkRequest'
    downloadGetSuggestedFilename,
) where

-- {{{ Imports
import Hbro.Error
import Hbro.Util

import qualified Data.Text.Lazy as Lazy

-- import Graphics.UI.Gtk.General.General as GTK
import qualified Graphics.UI.Gtk.WebKit.Download as W
import Graphics.UI.Gtk.WebKit.NetworkRequest (NetworkRequest)
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as W
import Graphics.UI.Gtk.WebKit.WebView (WebView)
import qualified Graphics.UI.Gtk.WebKit.WebView as W

import Network.URI.Monadic

import Prelude hiding(mapM_)
-- }}}


data UnavailableFileName = UnavailableFileName deriving(Typeable)
instance Exception UnavailableFileName
instance Show UnavailableFileName where show _ = "No file name available."


loadHtmlString :: (MonadBase IO m) => Lazy.Text -> URI -> WebView -> m ()
loadHtmlString html uri webView = gAsync $ W.webViewLoadHtmlString webView (Lazy.unpack html) (show uri)

loadString :: (MonadBase IO m) => Lazy.Text -> URI -> WebView -> m ()
loadString html uri webView = gAsync $ W.webViewLoadString webView (Lazy.unpack html) Nothing Nothing (show uri)

networkRequestGetUri :: (MonadBase IO m, MonadThrow m) => NetworkRequest -> m URI
networkRequestGetUri r = parseURIReference =<< io (W.networkRequestGetUri r) `failWithM` UnavailableURI

downloadGetUri :: (MonadBase IO m, MonadThrow m) => W.Download -> m URI
downloadGetUri d = parseURI =<< io (W.downloadGetUri d) `failWithM` UnavailableURI

downloadGetSuggestedFilename :: (MonadBase IO m, MonadThrow m) => W.Download -> m String
downloadGetSuggestedFilename d = io (W.downloadGetSuggestedFilename d) `failWithM` UnavailableFileName
