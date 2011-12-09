module Hbro.Util (
-- * Process management
    runCommand',
    spawn,
    getAllProcessIDs,
-- * Misc
    labelSetMarkupTemporary,
    dmenu
) where

-- {{{ Imports
--import Hbro.Types

--import Control.Monad.Reader
import Control.Monad

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.General

import qualified System.Info as Sys
import System.IO
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


(>>?) :: (a -> IO ()) -> Maybe a -> IO ()
(>>?) = maybe (return ())

    
    
dmenu :: [String] -> T.Text -> IO String
dmenu options input = do
    (in_, out, err, pid) <- runInteractiveProcess "dmenu" options Nothing Nothing
    T.hPutStr in_ input
    hClose  in_
    
    output <- catch (hGetLine out) $ \_error -> return []
    
    hClose out >> hClose err >> (void $ waitForProcess pid)
    return output
