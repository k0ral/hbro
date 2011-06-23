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

--import Graphics.UI.Gtk.Abstract.IMContext
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
--import Graphics.UI.Gtk.Windows.Window

import Network.URL
import Prelude

import System.Console.CmdArgs
import System.Glib.Signals
import System.Posix.Process
import qualified System.ZMQ as ZMQ
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
-- Parse arguments and step down in favour of initBrowser.
-- Print configuration error, if any.
realMain :: Configuration -> IO ()
realMain configuration = do
    options <- getOptions

    case (mError configuration) of
        Just e -> putStrLn e
        _      -> return ()

    initBrowser configuration options
-- }}}

-- {{{ Main function
-- | Application's main function.
-- Create browser and load homepage.
initBrowser :: Configuration -> CliOptions -> IO ()
initBrowser configuration options = do
    -- Initialize browser
    _   <- initGUI
    gui <- loadGUI (mUIFile configuration)
    let browser = Browser options configuration gui
    let webView = mWebView gui

    -- Initialize IPC socket
    pid       <- getProcessID
    context   <- ZMQ.init 1
    repSocket <- ZMQ.socket context ZMQ.Rep 
    let socketURI = "ipc://" ++ (mSocketDir configuration) ++ "/hbro." ++ show pid

    ZMQ.bind repSocket socketURI
      
    _ <- quitAdd 0 $ do
        ZMQ.setOption repSocket (ZMQ.Linger 0)
        ZMQ.close repSocket
        ZMQ.term  context
        return False

    _ <- forkIO $ listenToSocket repSocket browser

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
        putStrLn "NEW WINDOW"
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

-- | Load given URL in the browser.
loadURL :: String -> Browser -> IO ()
loadURL url browser =
    case importURL url of
        Just url' -> loadURL' url' browser
        _         -> return ()

-- | Backend function for loadURL.
loadURL' :: URL -> Browser -> IO ()
loadURL' url@URL {url_type = Absolute _} browser =
    webViewLoadUri (mWebView $ mGUI browser) (exportURL url)
loadURL' url@URL {url_type = HostRelative} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("file://" ++ exportURL url) >> putStrLn (show url)
loadURL' url@URL {url_type = _} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("http://" ++ exportURL url) >> print url
-- }}}

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
