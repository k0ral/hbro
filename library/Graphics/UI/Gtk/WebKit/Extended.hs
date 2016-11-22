{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some lifted functions from 'Graphics.UI.Gtk.WebKit'.
-- Designed to be imported as @qualified@.
module Graphics.UI.Gtk.WebKit.Extended
  ( -- * 'NetworkRequest'
    networkRequestGetUri
    -- * 'Download'
  , downloadGetUri
    -- * 'NetworkRequest'
  , downloadGetSuggestedFilename
    -- * 'WebDataSource'
  , dataSourceGetData
    -- * 'WebView'
  , webViewUri
  , webViewGetUri
  , webViewGetIconUri
  , webViewTryGetFaviconPixbuf
  , webViewGetTitle
  , loadHtmlString
  , loadString
    -- * Exceptions
  , WebKitException(..)
    -- * Re-export
  , module X
  ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Graphics.UI.Gtk.Gdk.Pixbuf
import           Graphics.UI.Gtk.General.General.Extended
import qualified Graphics.UI.Gtk.WebKit.Download          as W
import           Graphics.UI.Gtk.WebKit.NetworkRequest    (NetworkRequest)
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest    as W
import           Graphics.UI.Gtk.WebKit.WebDataSource     as W
import           Graphics.UI.Gtk.WebKit.WebView           as X hiding (webViewGetIconUri,
                                                                webViewGetTitle,
                                                                webViewGetUri,
                                                                webViewTryGetFaviconPixbuf,
                                                                webViewUri)
import qualified Graphics.UI.Gtk.WebKit.WebView           as W

import           Network.URI.Extended

import           System.Glib.Attributes
-- }}}

networkRequestGetUri :: (MonadIO m, MonadThrow m) => NetworkRequest -> m URI
networkRequestGetUri r = parseURIReference =<< io (W.networkRequestGetUri r) <!> UnavailableUri

downloadGetUri :: (MonadIO m, MonadThrow m) => W.Download -> m URI
downloadGetUri d = parseURI =<< io (W.downloadGetUri d) <!> UnavailableUri

downloadGetSuggestedFilename :: (MonadIO m, MonadThrow m) => W.Download -> m Text
downloadGetSuggestedFilename d = io (W.downloadGetSuggestedFilename d) <!> UnavailableFileName

dataSourceGetData :: (MonadIO m, MonadThrow m) => WebDataSource -> m ByteString
dataSourceGetData ds = io (webDataSourceGetData ds) <!> UnavailableData


-- | 'W.webViewUri' with 'Text'
webViewUri :: ReadAttr WebView (Maybe Text)
webViewUri = W.webViewUri

webViewGetUri :: (MonadIO m, MonadThrow m) => WebView -> m URI
webViewGetUri = gSync . W.webViewGetUri >=> maybe (throwM UnavailableUri) return >=> parseURI

webViewGetIconUri :: (MonadIO m, MonadThrow m) => WebView -> m URI
webViewGetIconUri = gSync . W.webViewGetIconUri >=> maybe (throwM UnavailableUri) return >=> parseURI

webViewTryGetFaviconPixbuf :: (MonadIO m, MonadThrow m) => WebView -> Int -> Int -> m Pixbuf
webViewTryGetFaviconPixbuf webView width height = gSync (W.webViewTryGetFaviconPixbuf webView width height) >>= maybe (throwM UnavailableFavicon) return

webViewGetTitle :: (MonadIO m, MonadThrow m) => WebView -> m Text
webViewGetTitle = gSync . W.webViewGetTitle >=> maybe (throwM UnavailableTitle) return

loadHtmlString :: (MonadIO m) => Text -> URI -> WebView -> m ()
loadHtmlString html uri webView = gAsync $ W.webViewLoadHtmlString webView html (show uri)

loadString :: (MonadIO m) => Text -> URI -> WebView -> m ()
loadString html uri webView = gAsync $ W.webViewLoadString webView html Nothing (show uri)


data WebKitException = UnavailableData
                     | UnavailableFavicon
                     | UnavailableFileName
                     | UnavailableTitle
                     | UnavailableUri
                     deriving(Eq, Show)

instance Exception WebKitException where
  displayException UnavailableData     = "No data available in the web frame."
  displayException UnavailableFavicon  = "No available favicon."
  displayException UnavailableFileName = "No file name available."
  displayException UnavailableTitle    = "No available title."
  displayException UnavailableUri      = "No URI available."
