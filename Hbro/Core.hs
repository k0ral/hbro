{-# LANGUAGE OverloadedStrings #-}
module Hbro.Core (
-- * Main
    defaultConfig,
    launchHbro,
-- * Browsing
    goHome,
    loadURI,
-- * Scrolling    
    goTop,
    goBottom,
    goLeft,
    goRight,
-- * Misc
    printPage,
    executeJSFile
) where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Types
--import Hbro.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Control.Concurrent
import Control.Monad.Reader

import qualified Data.Map as M

import Graphics.UI.Gtk.General.General hiding(initGUI)
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView

import Network.URL

import System.Console.CmdArgs
import System.Directory
import System.IO
import System.Posix.Process
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mURI          = def &= help "URI to open at start-up" &= explicit &= name "u" &= name "uri" &= typ "URI",
    mVanilla      = def &= help "Do not read custom configuration file." &= explicit &= name "1" &= name "vanilla",
    mDenyReconf   = def &= help "Deny recompilation even if the configuration file has changed." &= explicit &= name "deny-reconf",
    mForceReconf  = def &= help "Force recompilation even if the configuration file hasn't changed." &= explicit &= name "force-reconf",
    mDyreDebug    = def &= help "Force the application to use './cache/' as the cache directory, and ./ as the configuration directory. Useful to debug the program without installation." &= explicit &= name "dyre-debug",
    mMasterBinary = def &= explicit &= name "dyre-master-binary"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Configuration (Dyre)
dyreParameters :: D.Params (Config, CliOptions)
dyreParameters = D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}

showError :: (Config, a) -> String -> (Config, a)
showError (config, x) message = (config { mError = Just message }, x)

-- | Default configuration.
-- Homepage: Google, socket directory: /tmp,
-- UI file: ~/.config/hbro/, no key/command binding.
defaultConfig :: CommonDirectories -> Config
defaultConfig directories = Config {
    mCommonDirectories = directories,
    mHomePage          = "https://encrypted.google.com/",
    mSocketDir         = mTemporary directories,
    mUIFile            = (mConfiguration directories) ++ "/ui.xml",
    mKeys              = const [],
    mWebSettings       = [],
    mSetup             = const (return () :: IO ()),
    mCommands          = [],
    mError             = Nothing
}
-- }}}

-- {{{ Entry point
-- | Browser's main function.
-- To be called in main function with a proper configuration.
-- See Hbro.Main for an example.
launchHbro :: Config -> IO ()
launchHbro config = do
    options <- getOptions
    
    case mVanilla options of
        True -> D.wrapMain dyreParameters{ D.configCheck = False } (config, options)
        _    -> D.wrapMain dyreParameters (config, options)

realMain :: (Config, CliOptions) -> IO ()
realMain (config, options) = do
-- Print configuration error, if any
    maybe (return ()) putStrLn $ mError config

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

realMain' :: Config -> CliOptions -> GUI -> ZMQ.Context -> IO ()
realMain' config options gui@GUI {mWebView = webView, mWindow = window} context = let
    environment = Environment options config gui context
    setup       = mSetup config
    socketDir   = mSocketDir config 
    commands    = mCommands config
  in do
-- Apply custom setup
    setup environment

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
    let commandsList = M.fromList $ defaultCommandsList ++ commands
    let socketURI    = "ipc://" ++ socketDir ++ "/hbro." ++ show pid
    void $ forkIO (openRepSocket context socketURI (listenToCommands environment commandsList))
    
-- Manage POSIX signals
    void $ installHandler sigINT (Catch interruptHandler) Nothing

    --timeoutAdd (putStrLn "OK" >> return True) 2000
    mainGUI -- Main loop

-- Make sure response socket is closed at exit
    whenLoud $ putStrLn "Closing socket..."
    closeSocket context socketURI
    whenNormal $ putStrLn "Exiting..."

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
    
-- {{{ Misc
-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: WebView -> IO ()
printPage webView = do
    frame <- webViewGetMainFrame webView
    webFramePrint frame


-- | Execute a javascript file on current webpage.
executeJSFile :: String -> WebView -> IO ()
executeJSFile filePath webView = do
    whenNormal $ putStrLn ("Executing Javascript file: " ++ filePath)
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript webView script'
-- }}}


-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
_savePage :: String -> WebView -> IO ()
_savePage _path webView = do
    frame        <- webViewGetMainFrame webView
    dataSource   <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()
