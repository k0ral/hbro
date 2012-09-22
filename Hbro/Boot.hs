{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Hbro.Boot where

-- {{{ Imports
import Hbro.Core
import qualified Hbro.Gui as Gui
import qualified Hbro.Prompt as Prompt
import qualified Hbro.Socket as Socket
import Hbro.Types
import Hbro.Util
import Hbro.Webkit.WebView as WebView

import Control.Concurrent
import qualified Config.Dyre as D
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

-- import Data.Functor
import Data.IORef
-- import Data.Maybe

import Graphics.UI.Gtk.General.General hiding(initGUI)

import Network.URI as N

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}

-- {{{ Commandline options
baseOptions :: CliOptions
baseOptions = CliOptions {
    __startURI      = def &= explicit &= name "u" &= name "uri"          &= typ "URI" &= help "URI to open at start-up",
    __vanilla       = def &= explicit &= name "1" &= name "vanilla"      &= help "Do not read custom configuration file.",
    __recompile     = def &= explicit &= name "r" &= name "recompile"    &= help "Force recompilation and do not launch browser.",
    __denyReconf    = def &= explicit             &= name "deny-reconf"  &= help "Deny recompilation even if the configuration file has changed.",
    __forceReconf   = def &= explicit             &= name "force-reconf" &= help "Force recompilation even if the configuration file hasn't changed.",
    __dyreDebug     = def &= explicit             &= name "dyre-debug"   &= help "Force the application to use './cache/' as the cache directory, and ./ as the configuration directory. Useful to debug the program without installation.",
    __masterBinary  = def &= explicit             &= name "dyre-master-binary"}

-- | Available commandline options (cf hbro -h).
cliOptions :: Mode (CmdArgs CliOptions)
cliOptions = cmdArgsMode $ baseOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Dynamic reconfiguration
-- | Print various paths used for dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = do
    (a, b, c, d, e) <- getPaths dyreParameters
    putStrLn $ unlines [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e, []]

-- | Dynamic reconfiguration settings
dyreParameters :: D.Params (Either String (Config, Setup, CliOptions))
dyreParameters = D.defaultParams {
    D.projectName             = "hbro",
    D.showError               = \_ -> Left,
    D.realMain                = realMain,
    D.ghcOpts                 = ["-threaded"],
    D.statusOut               = hPutStrLn stderr,
    D.includeCurrentDirectory = False}

-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile  dyreParameters
    getErrorString dyreParameters
-- }}}

-- | Main function to call in the configuration file (cf 'Hbro.Main')
-- First parse commandline options, then perform dynamic reconfiguration process
hbro :: Config -> Setup -> IO ()
hbro config startUp = do
    options <- cmdArgsRun cliOptions

    when (_recompile options) $
        recompile >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)

    D.wrapMain dyreParameters{ D.configCheck = not $ _vanilla options } $ Right (config, startUp, options)


-- | Entry point called after dynamic recompilation.
realMain :: Either String (Config, Setup, CliOptions) -> IO ()
realMain (Left e) = putStrLn e
realMain (Right (config, Setup customSetup, options)) = do
    void $ installHandler sigINT (Catch interruptHandler) Nothing
    whenLoud printDyrePaths

    gui        <- runReaderT Gui.build' config
    hooks      <- Hooks <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing
    startURI   <- getStartURI options
    keys       <- newIORef ""
    zmqContext <- ZMQ.init 1

    result <- runErrorT . (`runReaderT` Context options config gui zmqContext hooks keys) $ do
        threadSync <- fork Socket.open

        Gui.setupWindow
        Gui.setupScrollWindow
        Prompt.setup
        WebView.setup
        customSetup

        maybe goHome loadURI startURI    -- Load home page
        io mainGUI                       -- Main loop

        Socket.close
        io $ takeMVar threadSync

    either print return result

    ZMQ.term zmqContext
    logNormal "Exiting..."


--
getStartURI :: CliOptions -> IO (Maybe URI)
getStartURI options = case (__startURI options) of
    Just uri -> do
        fileURI <- doesFileExist uri
        case fileURI of
            True -> getCurrentDirectory >>= return . N.parseURIReference . ("file://" ++) . (</> uri)
            _    -> return $ N.parseURIReference uri
    _ -> return Nothing


--
interruptHandler :: IO ()
interruptHandler = logVerbose "Received SIGINT." >> mainQuit
