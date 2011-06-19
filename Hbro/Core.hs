{-# LANGUAGE OverloadedStrings #-} 
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import qualified Config.Dyre as Dyre
import Control.Concurrent
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Gtk.Abstract.Container
--import Graphics.UI.Gtk.Abstract.IMContext
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
--import Graphics.UI.Gtk.Windows.Window

import Network.URL
import Prelude

import System.Console.CmdArgs
import System.Glib.Signals
import System.Posix.Process
-- }}}

-- {{{ Commandline options
cliOptions :: CliOptions
cliOptions = CliOptions{
    mURI = def &= help "URI to open at start-up" &= explicit &= name "u" &= name "uri" &= typ "URI"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A suckless minimal KISSy browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Entry point
-- | Entry point of the application.
-- | Parse arguments and step down in favour of initBrowser.
realMain :: Configuration -> IO ()
realMain configuration = do
    options <- getOptions
    initBrowser configuration options
-- }}}

-- {{{ Main function
-- | Application's main function.
-- Create browser and load homepage.
initBrowser :: Configuration -> CliOptions -> IO ()
initBrowser configuration options = do
    -- Print configuration error, if any
    case (mError configuration) of
        Just e -> putStrLn e
        _      -> return ()

    -- Initialize browser
    _   <- initGUI
    gui <- loadGUI (mUIFile configuration)
    let browser = Browser options configuration gui
    let webView = mWebView gui

    -- Initialize IPC socket
    pid <- getProcessID
    _   <- forkIO $ createReplySocket ("ipc://" ++ (mSocketDir configuration) ++ "/hbro." ++ show pid) browser

    -- Load configuration
    settings <- mWebSettings configuration
    webViewSetWebSettings webView settings
    (mAtStartUp configuration) browser

    -- Load homepage
    goHome browser

    -- Load key bindings
    let keyBindings = importKeyBindings (mKeyBindings configuration)

    
    -- On new window request
    --newWindowWebView <- webViewNew
    _ <- on webView createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> webViewLoadUri webView uri
            --Just uri -> runExternalCommand $ "hbro " ++ uri
            Nothing  -> return ()
        return webView
--         return newWindowWebView

--     _ <- on newWindowWebView loadCommitted $ \frame -> do
--         getUri <- (webViewGetUri newWindowWebView)
--         case getUri of 
--             Just uri -> runExternalCommand $ "hbro \"" ++ uri ++ "\""
--             _        -> return ()


    -- Web inspector
    inspector <- webViewGetInspector webView
    _ <- on inspector inspectWebView $ \_ -> do
        inspectorWebView <- webViewNew
        containerAdd (mInspectorWindow gui) inspectorWebView
        return inspectorWebView
    
    _ <- on inspector showWindow $ do
        widgetShowAll (mInspectorWindow gui)
        return True

    -- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

--     _ <- on inspector attachWindow $ do
--         getWebView <- webInspectorGetWebView inspector
--         case getWebView of
--             Just webView -> do widgetHide (mInspectorWindow gui)
--                                containerRemove (mInspectorWindow gui) webView
--                                widgetSetSizeRequest webView (-1) 250
--                                boxPackEnd (mWindowBox gui) webView PackNatural 0
--                                widgetShow webView
--                                return True
--             _            -> return False

--     _ <- on inspector detachWindow $ do
--         getWebView <- webInspectorGetWebView inspector
--         _ <- case getWebView of
--             Just webView -> do containerRemove (mWindowBox gui) webView
--                                containerAdd (mInspectorWindow gui) webView
--                                widgetShowAll (mInspectorWindow gui)
--                                return True
--             _            -> return False
        
--         widgetShowAll (mInspectorWindow gui)
--         return True

    -- Key bindings
--     imContext <- get webView webViewImContext
--     _ <- on webView keyPressEvent $ do
--         value      <- eventKeyVal
--         modifiers  <- eventModifier

--         let keyString = keyToString value
--         
--         case keyString of
--             Just "<Escape>" -> do
--                 liftIO $ imContextFocusIn imContext
--                 return True
--             _               -> return False

    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        case keyString of 
            Just string -> do 
                case Map.lookup (Set.fromList modifiers, string) keyBindings of
                    Just callback -> liftIO $ callback browser
                    _             -> liftIO $ putStrLn string 
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
    showPrompt False browser 

    mainGUI
-- }}}

-- {{{ Util functions
goHome, goBack, goForward, stopLoading :: Browser -> IO ()

-- | Wrapper around webViewGoBack function
goBack      browser = webViewGoBack       (mWebView $ mGUI browser)
-- | Wrapper around webViewGoForward function
goForward   browser = webViewGoForward    (mWebView $ mGUI browser)
-- | Wrapper around webViewStopLoading function
stopLoading browser = webViewStopLoading  (mWebView $ mGUI browser)
-- | Load homepage (retrieved from commandline options or configuration file)
goHome      browser = case (mURI $ mOptions browser) of
    Just uri -> loadURL uri browser
    _        -> loadURL (mHomePage $ mConfiguration browser) browser

-- | Wrapper around webViewReload{BypassCache}
-- If boolean argument is False, it bypasses the cache
reload :: Bool -> Browser -> IO()
reload True browser = webViewReload             (mWebView $ mGUI browser)
reload _    browser = webViewReloadBypassCache  (mWebView $ mGUI browser)

zoomIn, zoomOut :: Browser -> IO()
-- | Wrapper around webViewZoomIn function
zoomIn  browser = webViewZoomIn (mWebView $ mGUI browser)
-- | Wrapper around webViewZoomOut function
zoomOut browser = webViewZoomOut (mWebView $ mGUI browser)

-- | Wrapper around webFramePrint function
printPage :: Browser -> IO()
printPage browser = do
    frame <- webViewGetMainFrame (mWebView $ mGUI browser)
    webFramePrint frame
-- }}}


-- | Show web inspector for current webpage.
showWebInspector :: Browser -> IO ()
showWebInspector browser = do
    inspector <- webViewGetInspector (mWebView $ mGUI browser)
    webInspectorInspectCoordinates inspector 0 0


-- | Load given URL in the browser.
loadURL :: String -> Browser -> IO ()
loadURL url browser =
    case importURL url of
        Just url' -> loadURL' url' browser
        _         -> return ()
-- }}}


-- | Backend function for loadURL.
loadURL' :: URL -> Browser -> IO ()
loadURL' url@URL {url_type = Absolute _} browser =
    webViewLoadUri (mWebView $ mGUI browser) (exportURL url)
loadURL' url@URL {url_type = HostRelative} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("file://" ++ exportURL url) >> putStrLn (show url)
loadURL' url@URL {url_type = _} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("http://" ++ exportURL url) >> print url

-- {{{ Dyre
showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }

hbro :: Configuration -> IO ()
hbro = Dyre.wrapMain Dyre.defaultParams {
    Dyre.projectName  = "hbro",
    Dyre.showError    = showError,
    Dyre.realMain     = realMain,
    Dyre.ghcOpts      = ["-threaded"]
}
-- }}}
