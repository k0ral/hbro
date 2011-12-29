module Hbro.Util (
-- * Process management
    runCommand',
    spawn,
    getAllProcessIDs,
-- * Misc
    labelSetMarkupTemporary,
    dmenu,
    errorHandler
) where

-- {{{ Imports
--import Hbro.Types

--import Control.Monad.Reader
import Control.Monad

import Data.List

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.General

import System.Console.CmdArgs
import qualified System.Info as Sys
import System.IO
import System.IO.Error
import System.Posix.Process
import System.Process
-- }}}


-- {{{ Process management
-- | Like run `runCommand`, but return IO ()
runCommand' :: String -> IO ()
runCommand' command = runCommand command >> return ()

-- | Run external command and won't kill when parent process exit.
-- nohup for ignore all hup signal. 
-- `> /dev/null 2>&1` redirect all stdout (1) and stderr (2) to `/dev/null`
spawn :: String -> [String] -> IO ()
spawn command options = spawn' (proc command options)

spawn' :: CreateProcess -> IO ()
--spawn' command = runCommand' $ "nohup " ++ command ++ " > /dev/null 2>&1"
spawn' command = createProcess command { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True } >> return ()

-- | Return the list of process IDs corresponding to all running instances of the browser.
getAllProcessIDs :: IO [FilePath]
getAllProcessIDs = do 
    (_, pids, _)  <- readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- getProcessID

    return $ delete (show myPid) . nub . words $ pids ++ " " ++ pids'
-- }}}

-- | Set a temporary markup text to a label that disappears after some delay.
labelSetMarkupTemporary :: Label -> String -> Int -> IO HandlerId
labelSetMarkupTemporary label text delay = do
    labelSetMarkup label text
    timeoutAdd clear delay
  where
    clear = labelSetMarkup label "" >> return False

-- | Open dmenu with given input and return selected entry.
dmenu :: [String]            -- ^ dmenu's commandline options
      -> String              -- ^ dmenu's input
      -> IO (Maybe String)   -- ^ Selected entry
dmenu options input = do
    (in_, out, err, pid) <- runInteractiveProcess "dmenu" options Nothing Nothing
    hPutStr in_ input
    hClose in_
    
    output <- try $ hGetLine out 
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
