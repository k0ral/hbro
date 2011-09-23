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
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView

import Network.URL

import System.Console.CmdArgs
import System.Glib.Signals
import System.Process
import System.Posix.Process
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
-- | Available commandline options
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
    maybe (return ()) putStrLn $ mError config

-- Initialize GUI
    _   <- initGUI
    gui <- loadGUI $ mUIFile config
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

-- Load key bindings
    let keyBindings = importKeyBindings (mKeys config)

-- On new window request
    _ <- on webView createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> do
                whenLoud $ putStrLn ("Requesting new window: " ++ uri ++ "...")
                webViewLoadUri webView uri
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
                    Just callback -> do
                        liftIO $ callback browser
                        liftIO $ whenLoud (putStrLn $ "Key pressed: " ++ show modifiers ++ string ++ " (mapped)")
                    _ -> liftIO $ whenLoud (putStrLn $ "Key pressed: " ++ show modifiers ++ string ++ " (unmapped)")
            _ -> return ()

        return False

-- Load homepage
    case (mURI options) of
        Just uri -> do 
            webViewLoadUri webView uri
            whenLoud $ putStrLn ("Loading " ++ uri ++ "...")
        _        -> goHome browser


-- Initialize IPC socket
    pid <- getProcessID
    let socketURI = "ipc://" ++ (mSocketDir config) ++ "/hbro." ++ show pid

    --timeoutAdd (putStrLn "OK" >> return True) 2000

    ZMQ.withContext 1 $ \context -> do
        _ <- forkIO $ createRepSocket context socketURI browser
    
        mainGUI -- Main loop

    -- Make sure response socket is closed at exit
        whenLoud $ putStrLn "Closing socket..."
        ZMQ.withSocket context ZMQ.Req $ \reqSocket -> do
            ZMQ.connect reqSocket socketURI
            ZMQ.send reqSocket (pack "Quit") []
            _ <- ZMQ.receive reqSocket []
            return ()

        whenNormal $ putStrLn "Exiting..."
-- }}}


-- {{{ Browsing functions
-- | Load homepage (set from configuration file).
goHome :: Browser -> IO ()
goHome browser = do
    whenLoud $ putStrLn ("Loading homepage: " ++ uri)
    loadURI uri browser
  where
    uri = mHomePage $ mConfiguration browser

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
        Just url' -> do
            whenLoud $ putStrLn ("Loading URI: " ++ url)
            loadURI' url' browser
        _ -> return ()

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

-- | Scroll to the left edge of web page. Provided for convenience.
horizontalHome :: Browser -> IO ()
horizontalHome browser = do
    adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow $ mGUI browser)
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- | Scroll to the right edge of web page. Provided for convenience.
horizontalEnd :: Browser -> IO ()
horizontalEnd browser = do
    adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow $ mGUI browser)
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper 
-- }}}


-- | Spawn a new instance of the browser.
newInstance :: IO ()
newInstance = spawn (proc "hbro" [])

-- | Execute a javascript file on current webpage.
executeJSFile :: String -> Browser -> IO ()
executeJSFile filePath browser = do
    whenNormal $ putStrLn ("Executing Javascript file: " ++ filePath)
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript (mWebView $ mGUI browser) script'


-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
savePage :: String -> Browser -> IO ()
savePage _path browser = do
    frame        <- webViewGetMainFrame webView
    dataSource   <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()
    
  where
    webView  = mWebView $ mGUI browser
