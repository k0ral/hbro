{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TemplateHaskell #-}
module Hbro.Gui (
    Buildable(..),
    StatusBar(..),
    GUI(),
    GUIReader(..),
    mainWindow,
    inspectorWindow,
    scrollWindow,
    webView,
    promptBar,
    statusBar,
    notificationBar,
    builder,
    getObject,
    toggleVisibility,
    buildFrom,
    init)
where

-- {{{ Imports
import Hbro.Notification
import Hbro.Prompt (PromptBar(..), PromptReader(..))
import qualified Hbro.Prompt as Prompt
import Hbro.Util
import qualified Hbro.Webkit.WebView as WebView

import Control.Applicative
import Control.Conditional hiding(when)
import Control.Lens hiding((??), view)
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Base
import Control.Monad.Error  hiding(forM_, mapM_)
import Control.Monad.Trans.Control

-- import Data.Foldable
-- import Data.Functor
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewLoadUri)
import Graphics.UI.Gtk.Windows.Window

import Prelude hiding(init, mapM_)

import System.Glib.Attributes hiding(get, set)
import qualified System.Glib.Attributes as G (get, set)
import System.Glib.Signals
import System.Glib.Types
-- }}}

-- {{{ Types
newtype StatusBar = StatusBar HBox

instance GObjectClass StatusBar where
    toGObject (StatusBar h) = toGObject h
    unsafeCastGObject g     = StatusBar $ unsafeCastGObject g
instance ObjectClass StatusBar
instance WidgetClass StatusBar


data GUI m = GUI {
    _mainWindow         :: Window,
    _inspectorWindow    :: Window,
    _scrollWindow       :: ScrolledWindow,  -- ^ 'ScrolledWindow' containing the webview
    _webView            :: WebView,
    _promptBar          :: PromptBar m,
    _statusBar          :: StatusBar,
    _notificationBar    :: NotificationBar,
    _builder            :: Builder          -- ^ Builder object created from XML file
}

makeLenses ''GUI

-- | 'MonadReader' for 'GUI'
class (Monad m) => GUIReader n m | m -> n where
    readGUI :: Simple Lens (GUI n) a -> m a

-- | UI elements that can be built from a @GtkBuilder@ object (that is: an XML file)
class Buildable a where
    build :: (MonadBase IO m) => Builder -> m a

instance (Monad m) => Buildable (PromptBar m) where
    build b = io $ do
        l  <- builderGetObject b castToLabel "promptDescription"
        e  <- builderGetObject b castToEntry "promptEntry"
        b' <- builderGetObject b castToHBox  "promptBox"
        oC <- newIORef . const $ return ()
        oV <- newIORef . const $ return ()

        return $ PromptBar b' l e oC oV

instance Buildable (WebView, ScrolledWindow) where
    build b = io $ do
        window  <- builderGetObject b castToScrolledWindow "webViewParent"
        wv      <- webViewNew
        containerAdd window wv

        return (wv, window)

instance Buildable (Window, VBox) where
    build b = io $ do
        w  <- builderGetObject b castToWindow "mainWindow"
        b' <- builderGetObject b castToVBox "windowBox"
        return (w, b')

instance Buildable StatusBar where
    build b = io $ StatusBar <$> builderGetObject b castToHBox "statusBox"

instance Buildable NotificationBar where
    build b = io $ NotificationBar <$> builderGetObject b castToLabel "notificationLabel" <*> newIORef Nothing

instance (Monad m) => Buildable (GUI m) where
    build b = io $ do
        (webView', sWindow') <- build b
        (window', wBox')     <- build b
        promptBar'           <- build b
        statusBar'           <- build b
        notificationBar'     <- build b
        inspectorWindow'     <- initWebInspector webView' wBox'

        return $ GUI window' inspectorWindow' sWindow' webView' promptBar' statusBar' notificationBar' b
-- }}}


-- {{{ Util
-- | Return the casted GObject corresponding to the given name (set in the builder's XML file)
getObject :: (MonadBase IO m, GUIReader n m, GObjectClass a) => (GObject -> a) -> String -> m a
getObject cast name = do
    b <- readGUI builder
    io $ builderGetObject b cast name

-- | Toggle a widget's visibility (provided for convenience).
toggleVisibility :: (MonadBase IO m, WidgetClass a) => a -> m ()
toggleVisibility widget = io $ do
    visibility <- G.get widget widgetVisible
    visibility ? widgetHide widget ?? widgetShow widget
-- }}}


-- {{{ Initialization
buildFrom :: (Monad n, MonadBase IO m) => FilePath -> m (GUI n)
buildFrom uiFile = do
    b <- io builderNew
    io $ builderAddFromFile b uiFile
    build b


init :: (MonadBase IO m, MonadBaseControl IO m, GUIReader m m, NotificationReader m, PromptReader m m, Error e, Show e, MonadError e m) => m ()
init = do
    w  <- readGUI webView
    mw <- readGUI mainWindow
    initWindow       mw
    initScrollWindow =<< readGUI scrollWindow
    Prompt.init      =<< readGUI promptBar
    WebView.init     w

    io $ windowSetDefault mw (Just w)

-- Validate/cancel prompt
    e <- readGUI (promptBar.(Prompt.entry))
    io . void $ on e keyPressEvent (f w)

-- Show window
    io . widgetShowAll =<< readGUI mainWindow
    Prompt.hide

    return ()
  where
    f w = do
        key <- eventKeyName
        when (key == "Return" || key == "Escape") $ io $ do
            --runInIO clean
            widgetGrabFocus w
            return ()
        return False


initScrollWindow :: (MonadBase IO m) => ScrolledWindow -> m ()
initScrollWindow window = io $ scrolledWindowSetPolicy window PolicyNever PolicyNever


initWindow :: (MonadBase IO m) => Window -> m ()
initWindow window = io $ do
    windowSetDefaultSize window 800 600
    widgetModifyBg window StateNormal (Color 0 0 10000)
    void $ onDestroy window GTK.mainQuit


initWebInspector :: (MonadBase IO m) => WebView -> VBox -> m (Window)
initWebInspector webView' windowBox = do
    inspector <- io $ webViewGetInspector webView'
    window'   <- io windowNew
    io $ G.set window' [ windowTitle := "hbro | Web inspector" ]

    io . void . on inspector inspectWebView $ \_ -> do
        view <- webViewNew
        containerAdd window' view
        return view

    io . void . on inspector showWindow $ do
        widgetShowAll window'
        return True

-- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

-- Attach inspector to browser's main window
    _ <- io $ on inspector attachWindow $ do
        webview <- webInspectorGetWebView inspector
        case webview of
            Just view -> do
                widgetHide window'
                containerRemove window' view
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
                containerAdd window' view
                widgetShowAll window'
                return True
            _ -> return False

        widgetShowAll window'
        return True

    return window'
-- }}}
