{-# LANGUAGE OverloadedStrings #-} 
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import Control.Concurrent
import Control.Monad.Trans(liftIO)

import Data.ByteString.Char8 (pack, unpack)
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

    -- Load additionnal settings from configuration
    settings <- mWebSettings configuration
    webViewSetWebSettings webView settings
    (mSetup configuration) browser

    -- Load homepage
    goHome browser

    -- Load key bindings
    let keyBindings = importKeyBindings (mKeys configuration)

    -- On new window request
    _ <- on webView createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        putStrLn "NEW WINDOW"
        case newUri of
            Just uri -> webViewLoadUri webView uri
            Nothing  -> return ()
        return webView

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

    -- Connect and show.
    _ <- onDestroy (mWindow gui) mainQuit
    widgetShowAll (mWindow gui)
    showPrompt False browser 


    -- Initialize IPC socket
    pid <- getProcessID
    let socketURI = "ipc://" ++ (mSocketDir configuration) ++ "/hbro." ++ show pid
    putStrLn $ "Listening socket " ++ socketURI

    ZMQ.withContext 1 $ \context -> do
        _ <- forkIO $ createRepSocket context socketURI browser
    
        mainGUI

        ZMQ.withSocket context ZMQ.Req $ \reqSocket -> do
            ZMQ.connect reqSocket socketURI
            ZMQ.send reqSocket (pack "Quit") []
            reply <- ZMQ.receive reqSocket []
            return ()
-- }}}


-- {{{ Browse
-- | Load homepage (retrieved from commandline options or configuration file)
goHome :: Browser -> IO ()
goHome browser = case (mURI $ mOptions browser) of
    Just uri -> loadURI uri browser
    _        -> loadURI (mHomePage $ mConfiguration browser) browser

-- | Wrapper around webViewGoBack function
goBack :: Browser -> IO ()
goBack browser = webViewGoBack (mWebView $ mGUI browser)

-- | Wrapper around webViewGoForward function
goForward :: Browser -> IO ()
goForward browser = webViewGoForward (mWebView $ mGUI browser)

-- | Wrapper around webViewStopLoading function
stopLoading :: Browser -> IO ()
stopLoading browser = webViewStopLoading (mWebView $ mGUI browser)

-- | Wrapper around webViewReload{BypassCache}
-- If boolean argument is False, it bypasses the cache
reload :: Bool -> Browser -> IO()
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
    webViewLoadUri (mWebView $ mGUI browser) ("file://" ++ exportURL url) >> putStrLn (show url)
loadURI' url@URL {url_type = _} browser = 
    webViewLoadUri (mWebView $ mGUI browser) ("http://" ++ exportURL url) >> print url
-- }}}


-- {{{ Zoom
-- | Wrapper around webViewZoomIn function
zoomIn :: Browser -> IO ()
zoomIn  browser = webViewZoomIn (mWebView $ mGUI browser)

-- | Wrapper around webViewZoomOut function
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
newWindow :: Browser -> IO ()
newWindow browser = runExternalCommand "hbro"

-- | Execute a javascript file on current webpage.
executeJSFile :: String -> Browser -> IO ()
executeJSFile filePath browser = do
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript (mWebView $ mGUI browser) script'
