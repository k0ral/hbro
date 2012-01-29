{-# LANGUAGE DoRec #-}
module Hbro.Gui (
    getObject,
    initGUI,
    showWebInspector,
    toggleVisibility
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Util
import qualified Hbro.Prompt as Prompt
import Hbro.Types

import Control.Monad hiding(forM_, mapM_)

--import Data.Foldable

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import qualified Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewLoadUri)
import Graphics.UI.Gtk.Windows.Window

import Prelude hiding(mapM_)

import System.Console.CmdArgs (whenNormal)
import System.Glib.Attributes
import System.Glib.Signals
import System.Glib.Types
-- }}}

-- Util
getObject :: GObjectClass a => (GObject -> a) -> String -> K a        
getObject cast name = with (mBuilder . mGUI) $ \builder -> builderGetObject builder cast name

initGUI :: (RefDirs -> FilePath) -> [AttrOp WebSettings] -> IO GUI
initGUI xmlPath settings = do
    void GTK.initGUI
-- Load XML
    xmlPath' <- resolve xmlPath
    whenNormal . putStr . ("Loading GUI from " ++) . (++ "... ") $ xmlPath'
    builder <- builderNew
    builderAddFromFile builder xmlPath'
-- Initialize components
    (webView, sWindow) <- initWebView      builder settings
    (window, wBox)     <- initWindow       builder webView
    promptBar          <- Prompt.init      builder
    statusBar          <- initStatusBar    builder
    inspectorWindow    <- initWebInspector webView wBox
-- Show window
    widgetShowAll window
    widgetHide (mBox promptBar)
    
    whenNormal $ putStrLn "Done."
    return $ GUI { 
        mWindow          = window, 
        mInspectorWindow = inspectorWindow, 
        mScrollWindow    = sWindow, 
        mWebView         = webView, 
        mPromptBar       = promptBar, 
        mStatusBar       = statusBar, 
        mBuilder         = builder
    }

initWebView :: Builder -> [AttrOp WebSettings] -> IO (WebView, ScrolledWindow)
initWebView builder settings = do
-- Initialize ScrolledWindows
    window <- builderGetObject builder castToScrolledWindow "webViewParent"
    scrolledWindowSetPolicy window PolicyNever PolicyNever
-- Initialize WebSettings
    webSettings <- webSettingsNew
    set webSettings settings
-- Initialize WebView
    webView     <- webViewNew
    set webView [ widgetCanDefault := True ]
    webViewSetWebSettings webView webSettings
    containerAdd window webView    
-- 
    _ <- on webView closeWebView $ GTK.mainQuit >> return False
    
    return (webView, window)

initWindow :: Builder -> WebView -> IO (Window, VBox)
initWindow builder webView = do
    window <- builderGetObject builder castToWindow "mainWindow"
    windowSetDefault window $ Just webView
    windowSetDefaultSize window 800 600
    widgetModifyBg window StateNormal (Color 0 0 10000)
    _ <- onDestroy window GTK.mainQuit
    
    box <- builderGetObject builder castToVBox "windowBox"
    
    return (window, box)
    
initStatusBar :: Builder -> IO HBox
initStatusBar builder = builderGetObject builder castToHBox "statusBox"
    

-- {{{ Web inspector
initWebInspector :: WebView -> VBox -> IO (Window)
initWebInspector webView windowBox = do 
    inspector       <- webViewGetInspector webView
    inspectorWindow <- windowNew
    set inspectorWindow [ windowTitle := "hbro | Web inspector" ]

    _ <- on inspector inspectWebView $ \_ -> do
        view <- webViewNew
        containerAdd inspectorWindow view
        return view
    
    _ <- on inspector showWindow $ do
        widgetShowAll inspectorWindow
        return True

-- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

-- Attach inspector to browser's main window
    _ <- on inspector attachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        case getWebView of
            Just view -> do 
                widgetHide inspectorWindow
                containerRemove inspectorWindow view
                widgetSetSizeRequest view (-1) 250
                boxPackEnd windowBox view PackNatural 0
                widgetShow view
                return True
            _ -> return False

-- Detach inspector in a distinct window
    _ <- on inspector detachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        _ <- case getWebView of
            Just view -> do
                containerRemove windowBox view
                containerAdd inspectorWindow view
                widgetShowAll inspectorWindow
                return True
            _ -> return False
        
        widgetShowAll inspectorWindow
        return True

    return inspectorWindow

-- | Show web inspector for current webpage.
showWebInspector :: K ()
showWebInspector = do
    inspector <- with (mWebView . mGUI) webViewGetInspector
    io $ webInspectorInspectCoordinates inspector 0 0
-- }}}



-- {{{ Util
-- | Toggle a widget's visibility (provided for convenience).
toggleVisibility :: WidgetClass a => a -> IO ()
toggleVisibility widget = do
    visibility <- get widget widgetVisible
    case visibility of
        False -> widgetShow widget
        _     -> widgetHide widget
-- }}}
