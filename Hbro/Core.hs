{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ExistentialQuantification #-}
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Control.Concurrent
--import Control.Monad.Trans(liftIO)
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General hiding(initGUI)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
--import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import Network.URL

import System.Console.CmdArgs
import System.Directory
--import System.Glib.Attributes
import System.Glib.Signals
import System.IO
--import System.Process
import System.Posix.Process
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
-- | Available commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mURI = def &= help "URI to open at start-up" &= explicit &= name "u" &= name "uri" &= typ "URI"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Configuration
dyreParameters :: D.Params Config
dyreParameters = D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}

showError :: Config -> String -> Config
showError config message = config { mError = Just message }

-- | Default configuration.
-- Does quite nothing.
defaultConfig :: FilePath -> FilePath -> Config 
defaultConfig home tmp = Config {
    mHomePage    = "https://www.google.com",
    mSocketDir   = tmp,
    mUIFile      = home ++ "/.config/hbro/ui.xml",
    mKeys        = const [],
    mWebSettings = [],
    mSetup       = const (return () :: IO ()),
    mCommands    = [],
    mError       = Nothing
}
-- }}}

-- {{{ Entry point
-- | Browser's main function.
-- To be called in function "main" with a proper configuration.
launchHbro :: Config -> IO ()
launchHbro = D.wrapMain dyreParameters

-- | Entry point for the application.
-- Print configuration error if any, parse commandline arguments,
-- initialize the GUI and forward the environment to realMain.
realMain :: Config -> IO ()
realMain config = do
-- Print configuration error, if any
    maybe (return ()) putStrLn $ mError config

-- Parse commandline arguments
    options <- getOptions

-- Print in-use paths
    getPaths dyreParameters >>= \(a,b,c,d,e) -> whenLoud $ do 
        putStrLn ("Current binary:  " ++ a)
        putStrLn ("Custom binary:   " ++ b)
        putStrLn ("Config file:     " ++ c)
        putStrLn ("Cache directory: " ++ d)
        putStrLn ("Lib directory:   " ++ e)
        putStrLn ""
        
-- Initialize GUI
    gui <- initGUI (mUIFile config) (mWebSettings config)

-- Initialize IPC socket
    ZMQ.withContext 1 $ realMain' config options gui


-- | Entry point for the application.
-- Create browser and load homepage.
realMain' :: Config -> CliOptions -> GUI -> ZMQ.Context -> IO ()
realMain' config options gui@GUI {mWebView = webView, mWindow = window} context = let
    environment = Environment options config gui context
    keys        = importKeyBindings $ (mKeys config) environment
    setup       = mSetup config
    socketDir   = mSocketDir config 
    commands    = mCommands config
  in do
-- Apply custom setup
    setup environment

-- Manage keys
    _ <- after window keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        case keyString of 
            Just "<Escape>" -> liftIO $ widgetHide ((mBox . mPromptBar) gui)
            Just string -> do 
                case Map.lookup (Set.fromList modifiers, string) keys of
                    Just callback -> do
                        liftIO $ callback
                        liftIO $ whenLoud (putStrLn $ "Key pressed: " ++ show modifiers ++ string ++ " (mapped)")
                    _ -> liftIO $ whenLoud (putStrLn $ "Key pressed: " ++ show modifiers ++ string ++ " (unmapped)")
            _ -> return ()

        return False

-- Load homepage
    case (mURI options) of
        Just uri -> do 
            fileURI <- doesFileExist uri
            case fileURI of
                True -> getCurrentDirectory >>= \dir -> webViewLoadUri webView $ "file://" ++ dir ++ "/" ++ uri
                _    -> webViewLoadUri webView uri
            
            whenLoud $ putStrLn ("Loading " ++ uri ++ "...")
        _ -> goHome webView config

-- Open socket
    pid              <- getProcessID
    let commandsList =  Map.fromList $ defaultCommandsList ++ commands
    let socketURI    =  "ipc://" ++ socketDir ++ "/hbro." ++ show pid
    void $ forkIO (openRepSocket context socketURI (listenToCommands environment commandsList))
    
-- Manage POSIX signals
    void $ installHandler sigINT (Catch interruptHandler) Nothing

    --timeoutAdd (putStrLn "OK" >> return True) 2000
    mainGUI -- Main loop

-- Make sure response socket is closed at exit
    whenLoud $ putStrLn "Closing socket..."
    closeSocket context socketURI
    whenNormal $ putStrLn "Exiting..."

-- | POSIX signal SIGINT handler.
interruptHandler :: IO ()
interruptHandler = do
    whenLoud $ putStrLn "Received SIGINT."
    mainQuit
-- }}}

-- {{{ Browsing functions
-- | Load homepage (set from configuration file).
goHome :: WebView -> Config -> IO ()
goHome webView config = do
    whenLoud $ putStrLn ("Loading homepage: " ++ uri)
    loadURI webView uri
  where
    uri = mHomePage config

-- | Wrapper around webViewLoadUri meant to transparently add the proper protocol prefix (http:// or file://).
-- Most of the time, you want to use this function instead of webViewLoadUri.
loadURI :: WebView -> String -> IO ()
loadURI webView uri = do
    whenLoud $ putStrLn ("Loading URI: " ++ uri)
    case importURL uri of
        Just uri'@URL {url_type = Absolute _}   -> webViewLoadUri webView (exportURL uri')
        Just uri'@URL {url_type = HostRelative} -> webViewLoadUri webView ("file://" ++ exportURL uri')
        Just uri'@URL {url_type = _}            -> webViewLoadUri webView ("http://" ++ exportURL uri')
        _ -> whenNormal $ putStrLn ("WARNING: not a valid URI: " ++ uri)
-- }}}

-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: WebView -> IO ()
printPage webView = do
    frame <- webViewGetMainFrame webView
    webFramePrint frame


-- {{{ Scrolling
-- | Scroll up to top of web page. Provided for convenience.
goTop :: ScrolledWindow -> IO ()
goTop window = do
    adjustment  <- scrolledWindowGetVAdjustment window
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower


-- | Scroll down to bottom of web page. Provided for convenience.
goBottom :: ScrolledWindow -> IO ()
goBottom window = do
    adjustment  <- scrolledWindowGetVAdjustment window
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper

-- | Scroll to the left edge of web page. Provided for convenience.
goLeft :: ScrolledWindow -> IO ()
goLeft window = do
    adjustment  <- scrolledWindowGetHAdjustment window
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- | Scroll to the right edge of web page. Provided for convenience.
goRight :: ScrolledWindow -> IO ()
goRight window = do
    adjustment  <- scrolledWindowGetHAdjustment window
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper 
-- }}}


-- | Execute a javascript file on current webpage.
executeJSFile :: String -> WebView -> IO ()
executeJSFile filePath webView = do
    whenNormal $ putStrLn ("Executing Javascript file: " ++ filePath)
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript webView script'


-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
savePage :: String -> WebView -> IO ()
savePage _path webView = do
    frame        <- webViewGetMainFrame webView
    dataSource   <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()
