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
import           Hbro.Logger
import           Hbro.Options                    as Options
import           Hbro.Prelude
import           Hbro.Signals                    as Signals
import           Hbro.Webkit.WebSettings         as Settings

import           Control.Concurrent.Async.Lifted
import           Control.Lens                    hiding ((??))
import           Control.Monad.Reader            hiding (guard, mapM_, msum,
                                                  when)

import           Filesystem

import           Graphics.UI.Gtk.General.General as Gtk

import           Paths_hbro

import           System.Posix.Signals
import           System.ZMQ4.Monadic             (runZMQ)
-- }}}

-- | Main function to call in the configuration file (cf file @Hbro/Main.hs@).
hbro :: K () -> IO ()
hbro setup = void . runMaybeT $ do
    options <- parseOptions

    Dyre.wrap (options^.dyreModeL)
              (\x -> withAsyncBound guiThread (mainThread x))
              (setup, options)

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
    socketURI <- getSocketURI options

    io . (`runReaderT` globalStatus) . runExceptT $ do
        resetKeyBindings
        Settings.resetAll
        lift customSetup

        debugM "hbro.boot" . ("Start-up configuration: \n" ++) . describe =<< Config.get id

        logErrors_ $ maybe goHome load (options^.startURIL)

    io . withAsyncList (Hooks.routines globalStatus signals hooks) $ \_ -> do
      withAsync (runZMQ $ IPC.routine socketURI (signals^._ipcSignals)) $ \_ ->
        wait uiThread

    debugM "hbro.main" "All threads correctly exited."

-- | Return the list of available UI files (from configuration and package)
getUIFiles :: (BaseIO m) => CliOptions -> m [FilePath]
getUIFiles options = do
    fileFromConfig  <- getAppConfigDirectory "hbro" >/> "ui.xml"
    fileFromPackage <- fpFromText . pack <$> (io $ getDataFileName "examples/ui.xml")
    return $ catMaybes [options^.uiFileL, Just fileFromConfig, Just fileFromPackage]
