{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Hbro.Boot where

-- {{{ Imports
import Hbro.Core
import Hbro.Default()
import qualified Hbro.Dyre as Dyre
import qualified Hbro.Gui as Gui
import qualified Hbro.Options as Options
import qualified Hbro.Prompt as Prompt
import qualified Hbro.Socket as Socket
import Hbro.Types
import Hbro.Util
import Hbro.Webkit.WebView as WebView

import Control.Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Data.Default
-- import Data.Functor
import Data.IORef
-- import Data.Maybe

import Graphics.UI.Gtk.General.General hiding(initGUI)

import System.Exit
import System.Posix.Signals
import qualified System.ZMQ as ZMQ
-- }}}


-- | Main function to call in the configuration file (cf 'Hbro.Main')
-- First parse commandline options, then perform dynamic reconfiguration process
hbro :: Config -> Setup -> IO ()
hbro config setup = do
    opts <- Options.get

    when (_help opts)      $ putStrLn Options.help >> exitSuccess
    when (_recompile opts) $ Dyre.recompile >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)

    Dyre.wrap realMain opts (config, setup, opts)


-- | Entry point called after dynamic recompilation.
realMain :: (Config, Setup, CliOptions) -> IO ()
realMain (config, Setup customSetup, options) = do
    void $ installHandler sigINT (Catch (runReaderT interruptHandler options)) Nothing

    gui        <- runReaderT Gui.build' config
    hooks      <- Hooks <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing
    startURI   <- Options.getStartURI options
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
    runReaderT (logNormal "Exiting...") options


--
interruptHandler :: (MonadIO m, MonadReader r m, HasOptions r) => m ()
interruptHandler = logNormal "Received SIGINT." >> io mainQuit
