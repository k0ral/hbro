{-# LANGUAGE FlexibleContexts #-}
module Hbro.Util where

-- {{{ Imports
import Hbro.Types

import Control.Concurrent
import Control.Exception
import Control.Monad hiding(mapM_)
import Control.Monad.Error hiding(mapM_)
import Control.Monad.Reader hiding(mapM_)
--import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.Download as W
import Graphics.UI.Gtk.WebKit.NetworkRequest as W

import Network.URI (URI)
import qualified Network.URI as N

import Prelude hiding(log, mapM_)

import System.FilePath
import qualified System.Info as Sys
-- import System.IO
import System.IO.Error
import System.Posix.Process
import System.Posix.Types
import System.Process
-- }}}

-- | Alias for 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Like 'forkIO' using 'MVar' as thread control
fork :: (MonadIO m, MonadBaseControl IO m) => m () -> m (MVar ())
fork f = do
    mvar <- io newEmptyMVar
    void . liftBaseWith $ \runInIO -> forkIO $ finally (void $ runInIO f) (putMVar mvar ())
    return mvar

-- | Like '(</>)' with first argument in IO to build platform-dependent paths.
(>/>) :: (MonadIO m) => IO FilePath -> FilePath -> m FilePath
(>/>) a b = io $ (</> b) <$> a

whenNormal :: (MonadIO m, MonadReader s m, HasOptions s) => m () -> m ()
whenNormal f = do
    quiet <- asks _quiet
    case quiet of
        False -> f
        _ -> return ()

whenLoud :: (MonadIO m, MonadReader s m, HasOptions s) => m () -> m ()
whenLoud f = do
    verbose <- asks _verbose
    case verbose of
        True -> f
        _ -> return ()


logNormal, logVerbose :: (MonadIO m, MonadReader s m, HasOptions s) => String -> m ()
logNormal  = whenNormal . io . putStrLn
logVerbose = whenLoud   . io . putStrLn

-- {{{ Process management
-- | Run external command and won't kill when parent process exit.
spawn :: MonadIO m => String -> [String] -> m ()
spawn command options = io . void $ createProcess (proc command options) { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True }

-- | Return the list of process IDs corresponding to all running instances of the browser.
getAllProcessIDs :: MonadIO m => m [ProcessID]
getAllProcessIDs = do
    (_, pids, _)  <- io $ readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- io $ readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- io $ getProcessID

    return $ delete myPid . map (read :: String -> ProcessID) . nub . words $ pids ++ " " ++ pids'
-- }}}

-- | Set a temporary markup text to a label that disappears after some delay.
labelSetMarkupTemporary :: {-IORef HandlerId ->-} Label -> String -> Int -> IO ()
labelSetMarkupTemporary {-x-} label text delay = do
    --handler <- readIORef x
    --timeoutRemove handler

    labelSetMarkup label text
    timeoutAdd (clear >> return False) delay >> return () -- >>= writeIORef x
  where
    clear = labelSetMarkup label ""

errorHandler :: (MonadIO m, MonadReader r m, HasOptions r) => FilePath -> IOError -> m ()
errorHandler file e = do
  when (isAlreadyInUseError e) $ whenNormal . io . putStrLn $ "ERROR: file <" ++ file ++ "> is already opened and cannot be reopened."
  when (isDoesNotExistError e) $ whenNormal . io . putStrLn $ "ERROR: file <" ++ file ++ "> doesn't exist."
  when (isPermissionError   e) $ whenNormal . io . putStrLn $ "ERROR: user doesn't have permission to open file <" ++ file ++ ">."


parseURIReference :: (MonadError HError m) => String -> m URI
parseURIReference uri = maybe (throwError $ InvalidURI uri) return $ N.parseURIReference uri

parseURI :: (MonadError HError m) => String -> m URI
parseURI uri = maybe (throwError $ InvalidURI uri) return $ N.parseURI uri

networkRequestGetUri :: (MonadIO m, MonadError HError m) => NetworkRequest -> m URI
networkRequestGetUri r = parseURIReference =<< maybe (throwError $ EmptyRequestURI r) return =<< io (W.networkRequestGetUri r)

downloadGetUri :: (MonadIO m, MonadError HError m) => W.Download -> m URI
downloadGetUri d               = parseURI =<< maybe (throwError $ EmptyDownloadURI d) return =<< io (W.downloadGetUri d)

downloadGetSuggestedFilename :: (MonadIO m, MonadError HError m) => W.Download -> m String
downloadGetSuggestedFilename d = maybe (throwError $ EmptySuggestedFileName d) return =<< io (W.downloadGetSuggestedFilename d)


-- Boolean types conversion
isCaseSensitive :: CaseSensitivity -> Bool
isCaseSensitive CaseSensitive = True
isCaseSensitive _             = False

isForward :: Direction -> Bool
isForward Forward = True
isForward _       = False

isWrapped :: Wrap -> Bool
isWrapped Wrap = True
isWrapped _    = False

-- Common pango attributes
allItalic, allBold :: PangoAttribute
allItalic = AttrStyle  {paStart = 0, paEnd = -1, paStyle  = StyleItalic}
allBold   = AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}



notify :: (Functor m, MonadIO m, MonadReader r m, HasNotificationBar r, HasHooks r, MonadError HError m) => Int -> String -> m ()
notify duration text = do
    label   <- _label <$> asks _notificationbar
    handler <- asks _notificationTimer

    io $ labelSetAttributes label [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 32767 32767 32767}]
    io $ labelSetMarkup label text
    io $ mapM_ timeoutRemove =<< readIORef handler

    newID <- io $ timeoutAdd (labelSetMarkup label "" >> return False) duration
    io $ writeIORef handler $ Just newID

-- | Convert a Modifier to a String.
stringify :: Modifier -> String
stringify Control = "C-"
--stringify' Shift   = "S-"
stringify Alt     = "M-"
stringify _       = []
