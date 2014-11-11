module Hbro.Boot (hbro) where

-- {{{ Imports
import           Hbro.Config                     as Config
import           Hbro.Core                       as Core
import           Hbro.Dyre                       as Dyre
import           Hbro.Error
import           Hbro.Gui                        as Gui
import           Hbro.Hooks                      as Hooks
import           Hbro.IPC                        as IPC (routine)
import           Hbro.K                          as K
import           Hbro.Logger                     as Logger
import           Hbro.Options                    as Options
import           Hbro.Prelude
import           Hbro.Signals                    as Signals
import           Hbro.Webkit.WebSettings         as Settings

import           Control.Concurrent.Async.Lifted
import           Control.Lens                    hiding ((<|), (??), (|>))

import           Data.Version                    hiding (Version)

import           Filesystem

import           Graphics.UI.Gtk.General.General as Gtk

import           Network.URI.Monadic

import           Paths_hbro

import           System.Posix.Process
import           System.Posix.Signals
import qualified System.ZMQ4                     as ZMQ (version)
import           System.ZMQ4.Monadic             (runZMQ)
-- }}}

-- | Main function to call in the configuration file (cf file @Hbro/Main.hs@).
hbro :: K () -> IO ()
hbro setup = do
    options <- parseOptions
    case options of
        Left Rebuild -> Dyre.recompile >>= mapM_ (infoM "hbro.boot")
        Left Version -> do
            (a, b, c) <- io ZMQ.version
            putStrLn $ "hbro: v" ++ pack (showVersion version)
            putStrLn $ "0MQ library: v" ++ intercalate "." (map tshow [a, b, c])
        Right runOptions -> Dyre.wrap (runOptions^.dyreModeL)
                                      (withAsyncBound guiThread . mainThread)
                                      (setup, runOptions)

-- | Gtk main loop thread.
guiThread :: IO ()
guiThread = do
    async $ do
        installHandler sigINT  (Catch onInterrupt) Nothing
        installHandler sigTERM (Catch onInterrupt) Nothing
    initGUI >> mainGUI
    debugM "hbro.main" "GUI thread correctly exited."
  where onInterrupt  = logInterrupt >> gAsync mainQuit
        logInterrupt = infoM "hbro.main" "Received interrupt signal."


mainThread :: (ControlIO m) => (K (), CliOptions) -> Async (StM IO ()) -> m ()
mainThread (customSetup, options) uiThread = logErrors_ $ do
    Logger.initialize $ options^.logLevelL

    -- Signals
    signals <- Signals.initialize
    hooks   <- Hooks.initialize

    -- GUI
    uiFiles <- getUIFiles options
    gui     <- asum $ map Gui.initialize uiFiles
    attachGuiSignals gui signals

    -- K monad
    globalStatus <- K.init gui hooks signals

    -- IPC
    theSocketURI <- getSocketURI options

    io . (`runReaderT` globalStatus) . runExceptT $ do
        resetKeyBindings
        Settings.resetAll
        lift customSetup

        debugM "hbro.boot" . ("Start-up configuration: \n" ++) . describe =<< Config.get id

        logErrors_ $ maybe goHome (load <=< getStartURI) (options^.startURIL)

    io . withAsyncList (Hooks.routines globalStatus signals hooks) $ \_ ->
      withAsync (runZMQ $ IPC.routine theSocketURI (signals^.ipcSignalsL)) $ \_ ->
        wait uiThread

    debugM "hbro.main" "All threads correctly exited."


-- | Return the list of available UI files (from configuration and package)
getUIFiles :: (BaseIO m) => CliOptions -> m [FilePath]
getUIFiles options = do
    fileFromConfig  <- getAppConfigDirectory "hbro" >/> "ui.xml"
    fileFromPackage <- fpFromText . pack <$> io (getDataFileName "examples/ui.xml")
    return $ catMaybes [options^.uiFileL, Just fileFromConfig, Just fileFromPackage]

-- | Return socket URI used by this instance
getSocketURI :: (BaseIO m) => CliOptions -> m Text
getSocketURI options = maybe getDefaultSocketURI (return . normalize) $ options^.socketPathL
  where
    normalize = ("ipc://" ++) . fpToText
    getDefaultSocketURI = do
      dir <- fpToText <$> io (getAppCacheDirectory "hbro")
      pid <- io getProcessID
      return $ "ipc://" ++ dir ++ "/hbro." ++ tshow pid

-- | Parse URI passed in commandline, check whether it is a file path or an internet URI
-- and return the corresponding normalized URI (that is: prefixed with "file://" or "http://")
getStartURI :: (BaseIO m, MonadError Text m) => URI -> m URI
getStartURI uri = do
    fileURI    <- io . isFile . fpFromText $ tshow uri
    workingDir <- io getWorkingDirectory

    parseURIReference ("file://" ++ fpToText workingDir ++ "/" ++ tshow uri) <| fileURI |> return uri
    -- maybe abort return =<< logErrors (parseURIReference fileURI')
