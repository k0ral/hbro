-- | Designed to be imported as @qualified@.
module Hbro.Webkit.WebSettings where

-- {{{ Imports
import Hbro.Gui
import Hbro.Util

import Control.Monad
import Control.Monad.Base

import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import System.Glib.Attributes as G
-- }}}


set :: (MonadBase IO m, GUIReader n m) => Attr WebSettings a -> a -> m ()
set element newValue = modify_ element $ const newValue

modify :: (MonadBase IO m, GUIReader n m) => Attr WebSettings a -> (a -> a) -> m a
modify element modifier = do
    w <- readGUI webView
    settings <- io $ webViewGetWebSettings w
    oldValue <- io $ get settings element
    io $ G.set settings [element := modifier oldValue]
    io $ webViewSetWebSettings w settings
    return oldValue

modify_ :: (MonadBase IO m, GUIReader n m) => Attr WebSettings a -> (a -> a) -> m ()
modify_ e m = void $ modify e m

toggle :: (MonadBase IO m, GUIReader n m) => Attr WebSettings Bool -> m Bool
toggle = (`modify` not)

toggle_ :: (MonadBase IO m, GUIReader n m) => Attr WebSettings Bool -> m ()
toggle_ = (`modify_` not)
