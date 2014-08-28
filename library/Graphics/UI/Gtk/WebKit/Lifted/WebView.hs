-- | Some lifted functions from 'Graphics.UI.Gtk.WebKit.WebView'.
-- Designed to be imported as @qualified@.
module Graphics.UI.Gtk.WebKit.Lifted.WebView
    ( webViewGetUri
    , webViewGetIconUri
    , webViewTryGetFaviconPixbuf
    , webViewGetTitle
    , loadHtmlString
    , loadString
    , module Export
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Graphics.UI.Gtk.Gdk.Pixbuf
import           Graphics.UI.Gtk.WebKit.WebView as Export hiding
                                                           (webViewGetIconUri,
                                                           webViewGetTitle,
                                                           webViewGetUri, webViewTryGetFaviconPixbuf)
import qualified Graphics.UI.Gtk.WebKit.WebView as W

import           Network.URI.Monadic
-- }}}

-- | Error message
unavailableFavicon, unavailableURI, titleUnavailable :: Text
unavailableFavicon = "No available favicon."
unavailableURI = "No available URI."
titleUnavailable = "No available title."


webViewGetUri :: (BaseIO m, MonadError Text m) => WebView -> m URI
webViewGetUri = gSync . W.webViewGetUri >=> maybe (throwError unavailableURI) return >=> parseURI

webViewGetIconUri :: (BaseIO m, MonadError Text m) => WebView -> m URI
webViewGetIconUri = gSync . W.webViewGetIconUri >=> maybe (throwError unavailableURI) return >=> parseURI

webViewTryGetFaviconPixbuf :: (BaseIO m, MonadError Text m) => WebView -> Int -> Int -> m Pixbuf
webViewTryGetFaviconPixbuf webView width height = gSync (W.webViewTryGetFaviconPixbuf webView width height) >>= maybe (throwError unavailableFavicon) return

webViewGetTitle :: (BaseIO m, MonadError Text m) => WebView -> m Text
webViewGetTitle = gSync . W.webViewGetTitle >=> maybe (throwError titleUnavailable) return

loadHtmlString :: (BaseIO m) => Text -> URI -> WebView -> m ()
loadHtmlString html uri webView = gAsync $ W.webViewLoadHtmlString webView html (tshow uri)

loadString :: (BaseIO m) => Text -> URI -> WebView -> m ()
loadString html uri webView = gAsync $ W.webViewLoadString webView html Nothing (tshow uri)
