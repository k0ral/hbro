{-# LANGUAGE DoRec #-}
module Hbro.Gui (
    initGUI,
    showWebInspector,
    prompt,
    promptIncremental,
    toggleVisibility
) where

-- {{{ Imports
import Hbro.Types

import Control.Monad
import Control.Monad.Trans

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import qualified Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import System.Console.CmdArgs (whenNormal, whenLoud)
import System.Glib.Attributes
import System.Glib.Signals
-- }}}


initGUI :: FilePath -> [AttrOp WebSettings] -> IO GUI
initGUI xmlPath settings = do
    whenNormal $ putStr ("Loading GUI from " ++ xmlPath ++ "... ")
    void GTK.initGUI

-- Load XML
    builder <- builderNew
    builderAddFromFile builder xmlPath
 
-- Init components
    (webView, sWindow) <- initWebView        builder settings
    (window, wBox)     <- initWindow         builder webView
    promptBar          <- initPromptBar      builder
    statusBar          <- initStatusBar      builder
    inspectorWindow    <- initWebInspector   webView wBox
    
-- Show window
    widgetShowAll window
    widgetHide (mBox promptBar)
    
    whenNormal $ putStrLn "Done."
    return $ GUI window inspectorWindow sWindow webView promptBar statusBar builder

initWebView :: Builder -> [AttrOp WebSettings] -> IO (WebView, ScrolledWindow)
initWebView builder settings = do
    webView     <- webViewNew
    webSettings <- webSettingsNew
           
    set webSettings settings
    
    set webView [ widgetCanDefault := True ]
    _ <- on webView closeWebView $ GTK.mainQuit >> return False
    webViewSetWebSettings webView webSettings
        
-- On new window request
    _ <- on webView createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> do
                whenLoud $ putStrLn ("Requesting new window: " ++ uri ++ "...")
                webViewLoadUri webView uri
            Nothing  -> return ()
        return webView
    
    window <- builderGetObject builder castToScrolledWindow "webViewParent"
    containerAdd window webView 
    scrolledWindowSetPolicy window PolicyNever PolicyNever
    
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

initPromptBar :: Builder -> IO PromptBar
initPromptBar builder = do
    label  <- builderGetObject builder castToLabel "promptDescription"
    labelSetAttributes label [
      AttrStyle  {paStart = 0, paEnd = -1, paStyle = StyleItalic},
      AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}
      ]
    
    entry <- builderGetObject builder castToEntry "promptEntry"
    box   <- builderGetObject builder castToHBox  "promptBox"
    
    return $ PromptBar box label entry
    
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
showWebInspector :: WebView -> IO ()
showWebInspector webView = do
    inspector <- webViewGetInspector webView
    webInspectorInspectCoordinates inspector 0 0
-- }}}


-- {{{ Prompt
openPrompt :: PromptBar -> String -> String -> IO ()
openPrompt _promptBar@PromptBar {mBox = promptBox, mDescription = description, mEntry = entry} newDescription defaultText = do
    labelSetText description newDescription
    entrySetText entry defaultText
    
    widgetShow promptBox
    widgetGrabFocus entry
    editableSetPosition entry (-1)
    
-- | Open prompt bar with given description and default value,
-- and register a callback to trigger at validation.
prompt :: String -> String -> (String -> IO ()) -> GUI -> IO ()
prompt l d = prompt' l d False

-- | Same as 'prompt', but callback is triggered for each change in prompt's entry.
promptIncremental :: String -> String -> (String -> IO ()) -> GUI -> IO ()
promptIncremental l d = prompt' l d True

prompt' :: String -> String -> Bool -> (String -> IO ()) -> GUI -> IO ()
prompt' description defaultText incremental callback _gui@GUI {mPromptBar = promptBar, mWebView = webView} = do
    openPrompt promptBar description defaultText

-- Register callback
    case incremental of
        True -> do 
            id1 <- on entry editableChanged $ entryGetText entry >>= callback
            rec id2 <- on entry keyPressEvent $ do
                key <- eventKeyName
                
                case key of
                    "Return" -> liftIO $ do
                        widgetHide promptBox
                        signalDisconnect id1
                        signalDisconnect id2
                        widgetGrabFocus webView
                    "Escape" -> liftIO $ do
                        widgetHide promptBox
                        signalDisconnect id1
                        signalDisconnect id2
                        widgetGrabFocus webView
                    _ -> return ()
                return False
            return ()

        _ -> do
            rec id <- on entry keyPressEvent $ do
                key  <- eventKeyName

                case key of
                    "Return" -> liftIO $ do
                        widgetHide promptBox
                        entryGetText entry >>= callback
                        signalDisconnect id
                        widgetGrabFocus webView
                    "Escape" -> liftIO $ do
                        widgetHide promptBox
                        signalDisconnect id
                        widgetGrabFocus webView
                    _        -> return ()
                return False

            return ()
  where
    promptBox = mBox promptBar
    entry     = mEntry promptBar
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
