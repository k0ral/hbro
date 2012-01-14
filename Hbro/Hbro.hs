{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Hbro (
-- * Main
    launchHbro
) where

-- {{{ Imports
import Hbro.Config
import Hbro.Core
import Hbro.Gui
--import Hbro.Keys
import Hbro.Socket
import Hbro.Types
import Hbro.Util

import qualified Config.Dyre as D
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Concurrent
import Control.Monad.Reader hiding(mapM_)

import Data.Foldable
import qualified Data.Map as M

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General hiding(initGUI)
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri, webViewLoadUri)
import Graphics.UI.Gtk.WebKit.Download

import Network.URI

import Prelude hiding(mapM_)

import System.Console.CmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Glib.Signals
import System.IO
import System.Posix.Process
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mURI           = def &= help "URI to open at start-up" &= explicit &= name "u" &= name "uri" &= typ "URI",
    mVanilla       = def &= help "Do not read custom configuration file." &= explicit &= name "1" &= name "vanilla",
    mRecompileOnly = def &= help "Do not launch browser after recompilation." &= explicit &= name "r" &= name "recompile-only",
    mDenyReconf    = def &= help "Deny recompilation even if the configuration file has changed." &= explicit &= name "deny-reconf",
    mForceReconf   = def &= help "Force recompilation even if the configuration file hasn't changed." &= explicit &= name "force-reconf",
    mDyreDebug     = def &= help "Force the application to use './cache/' as the cache directory, and ./ as the configuration directory. Useful to debug the program without installation." &= explicit &= name "dyre-debug",
    mMasterBinary  = def &= explicit &= name "dyre-master-binary"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Util
-- | 
printDyrePaths :: IO ()
printDyrePaths = getPaths dyreParameters >>= \(a,b,c,d,e) -> (putStrLn . unlines) [
    "Current binary:  " ++ a,
    "Custom binary:   " ++ b,
    "Config file:     " ++ c,
    "Cache directory: " ++ d,
    "Lib directory:   " ++ e, []]

-- | Launch a recompilation of the configuration file
recompile :: IO ()
recompile = do
    customCompile dyreParameters 
    getErrorString dyreParameters >>= mapM_ putStrLn
    

showError :: (Config, a) -> String -> (Config, a)
showError (config, x) message = (config { mError = Just message }, x)
-- }}}

-- {{{ Entry point
-- | Browser's main function.
-- To be called in main function with a proper configuration.
-- See Hbro.Main for an example.
launchHbro :: (CommonDirectories -> Config) -> IO ()
launchHbro configGenerator = do
    homeDir   <- getHomeDirectory
    tmpDir    <- getTemporaryDirectory
    configDir <- getUserConfigDir "hbro"
    dataDir   <- getUserDataDir   "hbro"
    
    let config = configGenerator (CommonDirectories homeDir tmpDir configDir dataDir)

    options <- getOptions
    when (mRecompileOnly options) $ recompile  >> exitSuccess
    case mVanilla options of
        True -> D.wrapMain dyreParameters{ D.configCheck = False } (config, options)
        _    -> D.wrapMain dyreParameters (config, options)
        
dyreParameters :: D.Params (Config, CliOptions)
dyreParameters = D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}

realMain :: (Config, CliOptions) -> IO ()
realMain (config, options) = do
-- Print configuration error, if any
    mapM_ putStrLn (mError config)

-- Print in-use paths
    whenLoud printDyrePaths
        
-- Initialize GUI
    gui <- initGUI (mUIFile config) (mWebSettings config)

-- Initialize IPC socket
    ZMQ.withContext 1 $ realMain' config options gui

realMain' :: Config -> CliOptions -> GUI -> ZMQ.Context -> IO ()
realMain' config options gui@GUI {mWebView = webView, mWindow = window} context = let
    environment      = Environment options config gui context
    setup            = mSetup config
    socketDir        = mSocketDir config 
    commands         = mCommands config
    keyEventHandler  = mKeyEventHandler config
    keyEventCallback = (mKeyEventCallback config) environment
  in do
-- Apply custom setup
    setup environment
    
-- Bind new window hook
    _ <- on webView createWebView $ \frame -> do
        webFrameGetUri frame >>= maybe (return webView) (\uri -> do
            whenLoud $ putStrLn ("Requesting new window: " ++ show uri ++ "...")
            (mNewWindowHook config) environment uri)

-- Bind download hook
    void $ on webView downloadRequested $ \download -> do
        uri      <- (>>= parseURI) `fmap` downloadGetUri download
        filename <- downloadGetSuggestedFilename download
        size     <- downloadGetTotalSize download
        
        case (uri, filename) of
            (Just uri', Just filename') -> do
                whenNormal $ putStrLn ("Requested download: " ++ show uri')
                (mDownloadHook config) environment uri' filename' size
            _                           -> return ()
        return False
        
-- Setup key handler
    rec i <- after webView keyPressEvent $ keyEventHandler keyEventCallback i webView

-- Load homepage
    startURI <- case (mURI options) of
        Just uri -> do 
            fileURI <- doesFileExist uri
            case fileURI of
                True -> getCurrentDirectory >>= \dir -> return $ Just ("file://" ++ dir </> uri)
                _    -> return $ Just uri
        _ -> return Nothing
    
    maybe (goHome webView config) (webViewLoadUri webView) (startURI >>= parseURIReference)

-- Open socket
    pid              <- getProcessID
    let commandsList = M.fromList $ defaultCommandsList ++ commands
    let socketURI    = "ipc://" ++ socketDir </> "hbro." ++ show pid
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
interruptHandler = whenLoud (putStrLn "Received SIGINT.") >> mainQuit
-- }}}
