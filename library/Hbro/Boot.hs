{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Hbro.Boot (Settings(..), hbro) where

-- {{{ Imports
import           Hbro.Config                     as Config
import           Hbro.Core                       as Core
import           Hbro.Defaults
import           Hbro.Dyre                       as Dyre
import           Hbro.Event
import           Hbro.Gui                        as Gui
import           Hbro.Gui.MainView
import           Hbro.IPC                        as IPC (CommandMap,
                                                         bindCommands)
import           Hbro.Keys                       as Keys
import           Hbro.Logger
import           Hbro.Options                    as Options
import           Hbro.Prelude

import           Control.Concurrent.Async.Lifted
import           Control.Lens                    hiding ((<|), (??), (|>))
import           Control.Monad.Trans.Resource

import           Data.UUID
import           Data.Version                    hiding (Version)

import           Graphics.UI.Gtk.General.General as Gtk

import           Network.URI.Extended

import qualified Paths_hbro                      as Package

import           System.Directory
import           System.Info
import           System.Posix.Signals
import           System.Random
import qualified System.ZMQ4                     as ZMQ (version)
-- }}}

-- | What users can configure.
data Settings = Settings
    { configuration :: Config
    , commandMap    :: forall m r . (God r m, MonadCatch m) => CommandMap m
    , keyMap        :: forall m r . (God r m, MonadCatch m) => KeyMap m
    , startUp       :: forall m r . (God r m, MonadCatch m) => m ()
    }

instance Default Settings where
  def = Settings
          { configuration = def
          , commandMap    = defaultCommandMap
          , keyMap        = defaultKeyMap
          , startUp       = debug "No start-up script defined"
          }


getDataFileName :: (MonadIO m, Functor m) => Text -> m FilePath
getDataFileName file = io (Package.getDataFileName $ unpack file)

-- | Main function to call in the configuration file. Cf @Hbro/Main.hs@ as an example.
hbro :: Settings -> IO ()
hbro settings = do
    options <- parseOptions

    case options of
        Left Rebuild -> Dyre.recompile >>= mapM_ putStrLn
        Left Version -> printVersions
        Right runOptions -> runResourceT . runThreadedLoggingT (runOptions^.logLevel_) $ Dyre.wrap (runOptions^.dyreMode_)
                              (withAsyncBound guiThread . mainThread)
                              (settings, runOptions)

-- | Gtk main loop thread.
guiThread :: (ControlIO m, MonadLogger m) => m ()
guiThread = do
    io . async $ do
        installHandler sigINT  (Catch onInterrupt) Nothing
        installHandler sigTERM (Catch onInterrupt) Nothing
    io $ initGUI >> mainGUI
    debug "GUI thread correctly exited."
  where onInterrupt  = logInterrupt >> postGUIAsync mainQuit
        logInterrupt = putStrLn "Received interrupt signal."


mainThread :: (ControlIO m, MonadCatch m, MonadThreadedLogger m, Alternative m, MonadResource m)
           => (Settings, CliOptions) -> Async () -> m ()
mainThread (settings, options) uiThread = logErrors_ $ do
    uiFiles      <- getUIFiles options
    (builder, mainView, promptBar, statusBar, notifBar) <- asum $ map Gui.initialize uiFiles

    socketURI    <- getSocketURI options
    keySignal    <- newSignal KeyMapPressed
    config       <- io . newTVarIO $ configuration settings

    flip runReaderT (config, ())
      . withReaderT (mainView, )
      . withReaderT (statusBar, )
      . withReaderT (promptBar, )
      . withReaderT (notifBar, )
      . withReaderT (builder, )
      . withReaderT (keySignal, )
      . withAsync (bindCommands socketURI (commandMap settings)) . const $ do
        bindKeys (mainView^.keyPressedHandler_) keySignal (keyMap settings)

        addHandler (mainView^.linkClickedHandler_) defaultLinkClickedHandler
        addHandler (mainView^.loadRequestedHandler_) defaultLoadRequestedHandler
        addHandler (mainView^.newWindowHandler_) defaultNewWindowHandler
        addHandler (mainView^.titleChangedHandler_) defaultTitleChangedHandler

        startUp settings

        debug . ("Start-up configuration: \n" ++) . describe =<< Config.get id

        maybe goHome (load <=< getStartURI) (options^.startURI_)
        io $ wait uiThread

    debug "All threads correctly exited."


-- | Return the list of available UI files (from configuration and package)
getUIFiles :: (MonadIO m, Functor m) => CliOptions -> m [FilePath]
getUIFiles options = do
    fileFromConfig  <- getAppUserDataDirectory "hbro" >/> "ui.xml"
    fileFromPackage <- getDataFileName "examples/ui.xml"
    return $ catMaybes [options^.uiFile_, Just fileFromConfig, Just fileFromPackage]

-- | Return socket URI used by this instance
getSocketURI :: (MonadIO m, Functor m) => CliOptions -> m Text
getSocketURI options = maybe getDefaultSocketURI (return . normalize) $ options^.socketPath_
  where
    normalize = ("ipc://" ++) . pack
    getDefaultSocketURI = do
      dir  <- pack <$> io getTemporaryDirectory
      uuid <- io (randomIO :: IO UUID)
      return $ "ipc://" ++ dir ++ "/hbro-" ++ tshow uuid

-- | Parse URI passed in commandline, check whether it is a file path or an internet URI
-- and return the corresponding normalized URI (that is: prefixed with "file://" or "http://")
getStartURI :: (MonadIO m, MonadThrow m) => URI -> m URI
getStartURI uri = do
    fileURI    <- io . doesFileExist $ show uri
    workingDir <- pack <$> io getCurrentDirectory

    if fileURI then parseURIReference ("file://" ++ workingDir ++ "/" ++ tshow uri) else return uri
    -- maybe abort return =<< logErrors (parseURIReference fileURI')

printVersions :: IO ()
printVersions = do
  (a, b, c) <- ZMQ.version
  putStrLn $ "hbro-" ++ pack (showVersion Package.version)
  putStrLn $ "compiled by " ++ pack compilerName ++ "-" ++ pack (showVersion compilerVersion)
  putStrLn $ "using zeromq-" ++ intercalate "." (map tshow [a, b, c])
