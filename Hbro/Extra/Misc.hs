module Hbro.Extra.Misc where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Graphics.UI.Gtk.WebKit.WebView
-- }}}


toggleSourceMode :: Browser -> IO ()
toggleSourceMode browser = do
    currentMode <- webViewGetViewSourceMode (mWebView $ mGUI browser)
    webViewSetViewSourceMode (mWebView $ mGUI browser) (not currentMode)
    reload True browser
