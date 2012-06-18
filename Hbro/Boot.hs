module Hbro.Boot (
-- * Commandline options
    getOptions,
-- * Dynamic reconfiguration
    printDyrePaths,
    recompile,
-- * Boot
    hbro    
) where

import qualified Hbro.Hbro as Hbro
import Hbro.Types
import Hbro.Util

import qualified Config.Dyre as D
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Monad

import Graphics.UI.Gtk.General.General hiding(initGUI)

import System.Console.CmdArgs
import System.Exit
import System.IO
import System.Posix.Signals




-- {{{ Commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mURI           = def &= name "u" &= name "uri" &= typ "URI" &= help "URI to open at start-up" &= explicit,
    mVanilla       = def &= name "1" &= name "vanilla"&= help "Do not read custom configuration file." &= explicit,
    mRecompile     = def &= name "r" &= name "recompile" &= help "Force recompilation and do not launch browser." &= explicit,
    mDenyReconf    = def             &= name "deny-reconf" &= help "Deny recompilation even if the configuration file has changed." &= explicit,
    mForceReconf   = def             &= name "force-reconf" &= help "Force recompilation even if the configuration file hasn't changed." &= explicit,
    mDyreDebug     = def             &= name "dyre-debug" &= help "Force the application to use './cache/' as the cache directory, and ./ as the configuration directory. Useful to debug the program without installation." &= explicit,
    mMasterBinary  = def             &= name "dyre-master-binary" &= explicit
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A minimal KISS-compliant browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = getPaths dyreParameters >>= \(a,b,c,d,e) -> (putStrLn . unlines) [
    "Current binary:  " ++ a,
    "Custom binary:   " ++ b,
    "Config file:     " ++ c,
    "Cache directory: " ++ d,
    "Lib directory:   " ++ e, []]

-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile  dyreParameters 
    getErrorString dyreParameters 

showError :: (Config', a) -> String -> (Config', a)
showError (_, x) message = (Left message, x)

dyreParameters :: D.Params (Config', CliOptions)
dyreParameters = D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}
-- }}}

-- | Browser's main function.
-- To be called in main function with a proper configuration.
-- See Hbro.Main for an example.
hbro :: Config -> IO ()
hbro config = do
    options <- getOptions
    
    when (mRecompile options) $
        recompile
        >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)
    
    case mVanilla options of
        True -> D.wrapMain dyreParameters{ D.configCheck = False } (Right config, options)
        _    -> D.wrapMain dyreParameters                          (Right config, options)


realMain :: (Config', CliOptions) -> IO ()
realMain (Left e, _)             = putStrLn e
realMain config = do
    void $ installHandler sigINT (Catch interruptHandler) Nothing
    whenLoud printDyrePaths
    Hbro.main config


interruptHandler :: IO ()
interruptHandler = logVerbose "Received SIGINT." >> mainQuit

