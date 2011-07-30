{-# LANGUAGE OverloadedStrings #-} 
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import Control.Concurrent
import Control.Monad.Trans(liftIO)

import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import Network.URL

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
-- | Entry point for the application.
-- Parse commandline arguments, print configuration error if any,
-- create browser and load homepage.
realMain :: Configuration -> IO ()
realMain config = do
    -- Parse commandline arguments
    options <- getOptions

    -- Print configuration error, if any
    case (mError config) of
        Just e -> putStrLn e
        _      -> return ()

    -- Initialize GUI
    _   <- initGUI
    gui <- loadGUI (mUIFile config)
    let browser = Browser options config gui
    let webView = mWebView gui
    let window  = mWindow gui

    _ <- onDestroy window mainQuit
    widgetShowAll window
    showPrompt False browser 

    -- Load additionnal settings from configuration
    settings <- mWebSettings config
    webViewSetWebSettings webView settings
    (mSetup config) browser

    -- Load homepage
    case (mURI options) of
        Just uri -> loadURI uri browser
        _        -> goHome browser

    -- Load key bindings
    let keyBindings = importKeyBindings (mKeys config)

    -- On new window request
    _ <- on webView createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        putStrLn "NEW WINDOW"
        case newUri of
            Just uri -> webViewLoadUri webView uri
            Nothing  -> return ()
        return webView

    -- Manage keys
    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        case keyString of 
            Just "<Escape>" -> liftIO $ showPrompt False browser
            Just string -> do 
                case Map.lookup (Set.fromList modifiers, string) keyBindings of
                    Just callback -> liftIO $ callback browser
                    _             -> liftIO $ putStrLn string 
            _ -> return ()

        return False


    -- Initialize IPC socket
    pid <- getProcessID
    let socketURI = "ipc://" ++ (mSocketDir config) ++ "/hbro." ++ show pid
    putStrLn $ "Listening socket " ++ socketURI

    ZMQ.withContext 1 $ \context -> do
        _ <- forkIO $ createRepSocket context socketURI browser
    
        mainGUI -- Main loop

        -- Make sure response socket is closed at exit
        ZMQ.withSocket context ZMQ.Req $ \reqSocket -> do
            ZMQ.connect reqSocket socketURI
            ZMQ.send reqSocket (pack "Quit") []
            _ <- ZMQ.receive reqSocket []
            return ()
-- }}}


-- {{{ Browsing functions
-- | Load homepage (set from configuration file).
goHome :: Browser -> IO ()
goHome browser = loadURI (mHomePage $ mConfiguration browser) browser

-- | Wrapper around webViewGoBack function, provided for convenience.
goBack :: Browser -> IO ()
goBack browser = webViewGoBack (mWebView $ mGUI browser)

-- | Wrapper around webViewGoForward function, provided for convenience.
goForward :: Browser -> IO ()
goForward browser = webViewGoForward (mWebView $ mGUI browser)

-- | Wrapper around webViewStopLoading function, provided for convenience.
stopLoading :: Browser -> IO ()
stopLoading browser = webViewStopLoading (mWebView $ mGUI browser)

-- | Wrapper around webViewReload{BypassCache}.
reload 
    :: Bool     -- ^ If False, cache is bypassed.
    -> Browser
    -> IO ()
reload True browser = webViewReload             (mWebView $ mGUI browser)
reload _    browser = webViewReloadBypassCache  (mWebView $ mGUI browser)

-- | Load given URL in the browser.
loadURI :: String -> Browser -> IO ()
loadURI url browser =
    case importURL url of
        Just url' -> loadURI' url' browser
        _         -> return ()

-- | Backend function for loadURI.
loadURI' :: URL -> Browser -> IO ()
loadURI' url@URL {url_type = Absolute _} browser =
    webViewLoadUri (mWebView $ mGUI browser) (exportURL url)
loadURI' url@URL {url_type = HostRelative} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("file://" ++ exportURL url)
loadURI' url@URL {url_type = _} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("http://" ++ exportURL url)
-- }}}


-- {{{ Zoom
-- | Wrapper around webViewZoomIn function, provided for convenience.
zoomIn :: Browser -> IO ()
zoomIn browser = webViewZoomIn (mWebView $ mGUI browser)

-- | Wrapper around webViewZoomOut function, provided for convenience.
zoomOut :: Browser -> IO ()
zoomOut browser = webViewZoomOut (mWebView $ mGUI browser)
-- }}}


-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: Browser -> IO()
printPage browser = do
    frame <- webViewGetMainFrame (mWebView $ mGUI browser)
    webFramePrint frame


-- {{{ Scrolling
-- | Scroll up to top of web page. Provided for convenience.
verticalHome :: Browser -> IO ()
verticalHome browser = do
    adjustment  <- scrolledWindowGetVAdjustment (mScrollWindow $ mGUI browser)
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower


-- | Scroll down to bottom of web page. Provided for convenience.
verticalEnd :: Browser -> IO ()
verticalEnd browser = do
    adjustment  <- scrolledWindowGetVAdjustment (mScrollWindow $ mGUI browser)
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper

-- |
horizontalHome :: Browser -> IO ()
horizontalHome browser = do
    adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow $ mGUI browser)
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- |
horizontalEnd :: Browser -> IO ()
horizontalEnd browser = do
    adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow $ mGUI browser)
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper 
-- }}}


-- | Spawn a new instance of the browser.
newWindow :: IO ()
newWindow = runExternalCommand "hbro"

-- | Execute a javascript file on current webpage.
executeJSFile :: String -> Browser -> IO ()
executeJSFile filePath browser = do
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript (mWebView $ mGUI browser) script'
