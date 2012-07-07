module Hbro.Webkit.WebSettings where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import System.Glib.Attributes
--- }}}


modify :: Attr WebSettings a -> (a -> a) -> K a
modify element modifier = with (mWebView . mGUI) $ \webView -> do
  settings <- webViewGetWebSettings webView
  oldValue <- get settings element
  set settings [element := modifier oldValue]
  webViewSetWebSettings webView settings
  return oldValue

toggle :: Attr WebSettings Bool -> K Bool
toggle = (`modify` not)

