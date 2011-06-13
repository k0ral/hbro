{-# LANGUAGE OverloadedStrings #-} 
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Util

import qualified Config.Dyre as Dyre
import Control.Concurrent
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.IMContext
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings

import Network.URL
import Prelude

import System.Environment
import System.Glib.Attributes
import System.Glib.Signals
import System.Posix.Process
-- }}}

-- {{{ Type definitions
data Browser = Browser {
    mArgs           :: [String],
    mGUI            :: GUI
}

type KeyBindingsList = [(([Modifier], String), (GUI -> IO ()))]

data Configuration = Configuration {
    mHomePage       :: String,          -- ^ Startup page 
    mSocketDir      :: String,          -- ^ Path to socket directory (/tmp for example)
    mKeyBindings    :: KeyBindingsList, -- ^ List of keybindings
    mWebSettings    :: IO WebSettings,  -- ^ Web settings
    mAtStartUp      :: GUI -> IO (),    -- ^ Custom startup instructions
    mError          :: Maybe String     -- ^ Error
}
-- }}}

-- {{{ Entry point
-- | Entry point of the application.
-- Check if help display is requested.
realMain :: Configuration -> IO ()
realMain configuration = do
    args <- getArgs

    case args of
        ["--help"]  -> putStrLn "Usage: browser [url]"
        _           -> initBrowser configuration
-- }}}

-- {{{ Main function
-- | Application's main function.
-- Create browser and load homepage.
initBrowser :: Configuration -> IO ()
initBrowser configuration = do
    case (mError configuration) of
        Just error -> putStrLn error
        _          -> return ()

    -- Initialize browser
    args <- initGUI
    gui  <- loadGUI ""
    let browser = Browser args gui

    -- Initialize IPC socket
    pid <- getProcessID
    _ <- forkIO $ createReplySocket ("ipc://" ++ (mSocketDir configuration) ++ "/hbro." ++ (show pid)) gui

    -- Load configuration
    settings <- mWebSettings configuration
    webViewSetWebSettings (mWebView gui) settings

    -- Launch custom startup
    (mAtStartUp configuration) gui

    -- Load url
    let url = case args of
                [arg] -> arg
                _     -> mHomePage configuration

    loadURL url gui

    -- Load key bindings
    let keyBindings = importKeyBindings (mKeyBindings configuration)

    
    -- On new window request
    --newWindowWebView <- webViewNew
    _ <- on (mWebView gui) createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> webViewLoadUri (mWebView gui) uri
            --Just uri -> runExternalCommand $ "hbro " ++ uri
            Nothing  -> return ()
        return (mWebView gui)
--         return newWindowWebView

--     _ <- on newWindowWebView loadCommitted $ \frame -> do
--         getUri <- (webViewGetUri newWindowWebView)
--         case getUri of 
--             Just uri -> runExternalCommand $ "hbro \"" ++ uri ++ "\""
--             _        -> return ()


    -- Web inspector
    inspector <- webViewGetInspector (mWebView gui)
    _ <- on inspector inspectWebView $ \_ -> do
        webView <- webViewNew
        containerAdd (mInspectorWindow gui) webView
        return webView
    
    _ <- on inspector showWindow $ do
        widgetShowAll (mInspectorWindow gui)
        return True

    -- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

    _ <- on inspector attachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        case getWebView of
            Just webView -> do widgetHide (mInspectorWindow gui)
                               containerRemove (mInspectorWindow gui) webView
                               widgetSetSizeRequest webView (-1) 250
                               boxPackEnd (mWindowBox gui) webView PackNatural 0
                               widgetShow webView
                               return True
            _            -> return False

    _ <- on inspector detachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        _ <- case getWebView of
            Just webView -> do containerRemove (mWindowBox gui) webView
                               containerAdd (mInspectorWindow gui) webView
                               widgetShowAll (mInspectorWindow gui)
                               return True
            _            -> return False
        
        widgetShowAll (mInspectorWindow gui)
        return True

    -- Key bindings
--     imContext <- get (mWebView gui) webViewImContext
--     _ <- on (mWebView gui) keyPressEvent $ do
--         value      <- eventKeyVal
--         modifiers  <- eventModifier

--         let keyString = keyToString value
--         
--         case keyString of
--             Just "<Escape>" -> do
--                 liftIO $ imContextFocusIn imContext
--                 return True
--             _               -> return False

    _ <- after (mWebView gui) keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        case keyString of 
            Just string -> do 
                case Map.lookup (Set.fromList modifiers, string) keyBindings of
                    Just callback   -> liftIO $ callback gui
                    _               -> liftIO $ putStrLn string 
            _ -> return ()

        return False

--     imContextFilterKeypress imContext $ do
--         value      <- eventKeyVal
--         modifiers  <- eventModifier

--         let keyString = keyToString value
--         putStrLn keyString

-- --         case keyString of 
-- --             Just string -> do 
-- --                 case Map.lookup (Set.fromList modifiers, string) keyBindings of
-- --                     Just callback   -> liftIO $ callback gui
-- --                     _               -> liftIO $ putStrLn string 
-- --             _ -> return ()

--         return False

    -- Connect and show.
    _ <- onDestroy (mWindow gui) mainQuit
    widgetShowAll (mWindow gui)
    widgetHide (mPromptLabel gui)
    widgetHide (mPrompt gui)

    mainGUI
-- }}}

-- | Show web inspector for current webpage.
showWebInspector :: GUI -> IO ()
showWebInspector gui = do
    inspector <- webViewGetInspector (mWebView gui)
    webInspectorInspectCoordinates inspector 0 0


-- | Load given URL in the browser.
loadURL :: String -> GUI -> IO ()
loadURL url gui =
    case importURL url of
        Just url' -> loadURL' url' gui
        _         -> return ()


-- | Backend function for loadURL.
loadURL' :: URL -> GUI -> IO ()
loadURL' url@URL {url_type = Absolute _} gui =
    webViewLoadUri (mWebView gui) (exportURL url)
loadURL' url@URL {url_type = HostRelative} gui = 
    webViewLoadUri (mWebView gui) ("file://" ++ exportURL url) >> putStrLn (show url)
loadURL' url@URL {url_type = _} gui = 
    webViewLoadUri (mWebView gui) ("http://" ++ exportURL url) >> print url

-- {{{ Dyre
showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }

browser :: Configuration -> IO ()
browser = Dyre.wrapMain Dyre.defaultParams {
    Dyre.projectName  = "hbro",
    Dyre.showError    = showError,
    Dyre.realMain     = realMain
}
-- }}}
