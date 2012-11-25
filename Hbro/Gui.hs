{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Hbro.Gui where

-- {{{ Imports
--import Hbro.Core
import Hbro.Util
import Hbro.Prompt()
import Hbro.Types
import Hbro.Webkit.WebView()

import Control.Conditional
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.IO.Class
import Control.Monad.Reader

--import Data.Foldable
import Data.Functor

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import qualified Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewLoadUri)
import Graphics.UI.Gtk.Windows.Window

import Prelude hiding(mapM_)

import System.Glib.Attributes
import System.Glib.Signals
import System.Glib.Types
-- }}}

-- Util
-- | Return the casted GObject corresponding to the given name (set in the builder's XML file)
getObject :: (MonadIO m, MonadReader r m, HasGUI r, GObjectClass a) => (GObject -> a) -> String -> m a
getObject cast name = do
    builder <- asks _builder
    io $ builderGetObject builder cast name

build' :: (MonadIO m, MonadReader r m, HasConfig r) => m GUI
build' = do
    xmlPath <- asks _UIFile
    io $ void GTK.initGUI
-- Load XML
    xmlPath' <- io xmlPath
    --logNormal $ "Loading GUI from " ++ xmlPath' ++ "... "
    builder <- io builderNew
    io $ builderAddFromFile builder xmlPath'
-- Build components
    (webView, sWindow) <- build builder
    (window, wBox)     <- build builder
    promptBar          <- build builder
    statusBar          <- build builder
    notificationBar    <- build builder
    inspectorWindow    <- initWebInspector webView wBox
-- Show window
    io $ widgetShowAll window
    io $ widgetHide (_box promptBar)

    --logNormal "Done."
    return $ GUI {
        __mainWindow      = window,
        __inspectorWindow = inspectorWindow,
        __scrollWindow    = sWindow,
        __webView         = webView,
        __promptBar       = promptBar,
        __statusBar       = statusBar,
        __notificationBar = notificationBar,
        __builder         = builder
    }

setupScrollWindow :: (MonadIO m, MonadReader r m, HasGUI r) => m ()
setupScrollWindow = do
    window <- asks _scrollWindow
    io $ scrolledWindowSetPolicy window PolicyNever PolicyNever

instance Buildable (Window, VBox) where
    build builder = io $ do
        window <- builderGetObject builder castToWindow "mainWindow"
        box    <- builderGetObject builder castToVBox "windowBox"
        return (window, box)

setupWindow :: (MonadIO m, MonadReader r m, HasGUI r) => m ()
setupWindow = do
    window <- asks _mainWindow
    io . windowSetDefault window . Just =<< asks _webView
    io $ windowSetDefaultSize window 800 600
    io $ widgetModifyBg window StateNormal (Color 0 0 10000)
    io . void $ onDestroy window GTK.mainQuit

instance Buildable StatusBar where
    build builder = io $ StatusBar <$> builderGetObject builder castToHBox "statusBox"

instance Buildable NotificationBar where
    build builder = io $ NotificationBar <$> builderGetObject builder castToLabel "notificationLabel"

-- {{{ Web inspector
initWebInspector :: (MonadIO m) => WebView -> VBox -> m (Window)
initWebInspector webView windowBox = do
    inspector       <- io $ webViewGetInspector webView
    inspectorWindow <- io windowNew
    io $ set inspectorWindow [ windowTitle := "hbro | Web inspector" ]

    _ <- io $ on inspector inspectWebView $ \_ -> do
        view <- webViewNew
        containerAdd inspectorWindow view
        return view

    _ <- io $ on inspector showWindow $ do
        widgetShowAll inspectorWindow
        return True

-- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

-- Attach inspector to browser's main window
    _ <- io $ on inspector attachWindow $ do
        webview <- webInspectorGetWebView inspector
        case webview of
            Just view -> do
                widgetHide inspectorWindow
                containerRemove inspectorWindow view
                widgetSetSizeRequest view (-1) 250
                boxPackEnd windowBox view PackNatural 0
                widgetShow view
                return True
            _ -> return False

-- Detach inspector in a distinct window
    _ <- io $ on inspector detachWindow $ do
        webview <- webInspectorGetWebView inspector
        _ <- case webview of
            Just view -> do
                containerRemove windowBox view
                containerAdd inspectorWindow view
                widgetShowAll inspectorWindow
                return True
            _ -> return False

        widgetShowAll inspectorWindow
        return True

    return inspectorWindow
-- }}}

-- {{{ Util
-- | Toggle a widget's visibility (provided for convenience).
toggleVisibility :: (MonadIO m, WidgetClass a) => a -> m ()
toggleVisibility widget = io $ do
    visibility <- get widget widgetVisible
    visibility ? widgetHide widget ?? widgetShow widget
-- }}}
