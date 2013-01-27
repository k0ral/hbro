module Hbro.Util where

-- {{{ Imports
import Control.Concurrent
import Control.Exception
import Control.Monad hiding(mapM_)
import Control.Monad.Base
-- import Control.Monad.Error hiding(mapM_)
-- import Control.Monad.Reader hiding(mapM_)
import Control.Monad.Trans.Control

-- import Data.Foldable
import Data.Functor
import Data.List

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.General.General

-- import qualified Network.URI as N

import Prelude hiding(log, mapM_)

import System.FilePath
import qualified System.Info as Sys
-- import System.IO
import System.Posix.Process
import System.Posix.Types
import System.Process
-- }}}

-- {{{ Aliases/shortcuts
-- | Alias for 'liftIO'
io :: MonadBase IO m => IO a -> m a
io = liftBase

-- | Like 'forkIO' using 'MVar' as thread control
fork :: (MonadBaseControl IO m) => m () -> m (MVar ())
fork f = do
    mvar <- io newEmptyMVar
    liftBaseWith $ \runInIO -> void . forkIO $ finally (void $ runInIO f) (putMVar mvar ())
    return mvar

-- | Like '(</>)' with first argument in IO to build platform-dependent paths.
(>/>) :: (MonadBase IO m) => IO FilePath -> FilePath -> m FilePath
(>/>) a b = io $ (</> b) <$> a
-- }}}

-- {{{ Process management
-- | Run external command and won't kill when parent process exit.
spawn :: MonadBase IO m => String -> [String] -> m ()
spawn command options = io . void $ createProcess (proc command options) { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True }

-- | Return the list of process IDs corresponding to all running instances of the browser.
getAllProcessIDs :: MonadBase IO m => m [ProcessID]
getAllProcessIDs = do
    (_, pids, _)  <- io $ readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- io $ readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- io $ getProcessID

    return $ delete myPid . map (read :: String -> ProcessID) . nub . words $ pids ++ " " ++ pids'
-- }}}


{-errorHandler :: (MonadBase IO m, MonadReader r m, HasOptions r) => FilePath -> IOError -> m ()
errorHandler file e = do
  when (isAlreadyInUseError e) $ unlessQuiet . io . putStrLn $ "ERROR: file <" ++ file ++ "> is already opened and cannot be reopened."
  when (isDoesNotExistError e) $ unlessQuiet . io . putStrLn $ "ERROR: file <" ++ file ++ "> doesn't exist."
  when (isPermissionError   e) $ unlessQuiet . io . putStrLn $ "ERROR: user doesn't have permission to open file <" ++ file ++ ">."-}

-- Common pango attributes
allItalic, allBold :: PangoAttribute
allItalic = AttrStyle  {paStart = 0, paEnd = -1, paStyle  = StyleItalic}
allBold   = AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}

postGUISync' :: (MonadBaseControl IO m) => m a -> m a
postGUISync' f = control $ \runInIO -> postGUISync (runInIO f)
