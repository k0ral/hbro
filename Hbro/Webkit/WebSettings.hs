{-# LANGUAGE FlexibleContexts #-}
-- | Designed to be imported as @qualified@.
module Hbro.Webkit.WebSettings where

-- {{{ Imports
--import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import System.Glib.Attributes
--- }}}


modify :: (MonadIO m, MonadReader r m, HasWebView r) => Attr WebSettings a -> (a -> a) -> m a
modify element modifier = do
  webView <- asks _webview
  settings <- io $ webViewGetWebSettings webView
  oldValue <- io $ get settings element
  io $ set settings [element := modifier oldValue]
  io $ webViewSetWebSettings webView settings
  return oldValue

toggle :: (MonadIO m, MonadReader r m, HasWebView r) => Attr WebSettings Bool -> m Bool
toggle = (`modify` not)
