module Hbro.Extra.Misc where

-- {{{ Imports
import Hbro.Core hiding(goBack, goForward)
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebView
-- }}}


-- | Same as goBack function from Hbro.Core,
-- but with feedback in case of failure.
goForward :: Browser -> IO ()
goForward browser = do
    result        <- webViewCanGoForward webView
    feedbackLabel <- builderGetObject builder castToLabel "feedback"

    case result of
        True -> webViewGoForward webView
        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go forward !</span>" 5000 >> return ()

  where
    webView = mWebView $ mGUI browser
    builder = mBuilder $ mGUI browser

-- | Same as goBack function from Hbro.Core,
-- but with feedback in case of failure.
goBack :: Browser -> IO ()
goBack browser = do
    result        <- webViewCanGoBack webView
    feedbackLabel <- builderGetObject builder castToLabel "feedback"

    case result of
        True -> webViewGoBack webView
        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go back !</span>" 5000 >> return ()

  where
    webView = mWebView $ mGUI browser
    builder = mBuilder $ mGUI browser

-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: Browser -> IO ()
toggleSourceMode browser = do
    currentMode <- webViewGetViewSourceMode (mWebView $ mGUI browser)
    webViewSetViewSourceMode (mWebView $ mGUI browser) (not currentMode)
    reload True browser
