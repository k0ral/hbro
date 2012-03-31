module Hbro.Util (
    io,
    resolve,
-- * Process management
    spawn,
    getAllProcessIDs,
-- * Misc
    send'',
    labelSetMarkupTemporary,
    dmenu,
    errorHandler
) where

-- {{{ Imports
import Hbro.Types

import Control.Exception
--import Control.Monad.Reader
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
--import Data.IORef
import Data.List

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.General

import System.Console.CmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
import qualified System.Info as Sys
import System.IO
import System.IO.Error hiding(try)
import System.Posix.Process
import System.Posix.Types
import System.Process
import System.ZMQ
-- }}}


io :: MonadIO m => IO a -> m a
io = liftIO

send'' :: Socket a -> ByteString -> IO ()
send'' x y = send x y []

resolve :: (RefDirs -> a) -> IO a
resolve f = do
    homeDir   <- getHomeDirectory
    tmpDir    <- getTemporaryDirectory
    configDir <- getUserConfigDir "hbro"
    dataDir   <- getUserDataDir   "hbro"
    
    return . f $ RefDirs homeDir tmpDir configDir dataDir

-- {{{ Process management
-- | Run external command and won't kill when parent process exit.
spawn :: String -> [String] -> IO ()
spawn command options = spawn' (proc command options)

spawn' :: CreateProcess -> IO ()
spawn' command = createProcess command { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True } >> return ()

-- | Return the list of process IDs corresponding to all running instances of the browser.
getAllProcessIDs :: IO [ProcessID]
getAllProcessIDs = do 
    (_, pids, _)  <- readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- getProcessID

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

-- | Open dmenu with given input and return selected entry.
dmenu :: [String]            -- ^ dmenu's commandline options
      -> String              -- ^ dmenu's input
      -> IO (Maybe String)   -- ^ Selected entry
dmenu options input = do
    (in_, out, err, pid) <- runInteractiveProcess "dmenu" options Nothing Nothing
    hPutStr in_ input
    hClose in_
    
    output <- try $ hGetLine out :: IO (Either IOError String)
    let output' = case output of
          Left _  -> Nothing
          Right x -> Just x
    
    hClose out >> hClose err >> (void $ waitForProcess pid)
    return output'

errorHandler :: FilePath -> IOError -> IO ()
errorHandler file e = do
  when (isAlreadyInUseError e) $ (whenNormal . putStrLn) ("ERROR: file <" ++ file ++ "> is already opened and cannot be reopened.")
  when (isDoesNotExistError e) $ (whenNormal . putStrLn) ("ERROR: file <" ++ file ++ "> doesn't exist.")
  when (isPermissionError   e) $ (whenNormal . putStrLn) ("ERROR: user doesn't have permission to open file <" ++ file ++ ">.")
