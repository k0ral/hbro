module Hbro.Util where

-- {{{ Imports
import Hbro.Types

--import Control.Monad.Reader
import Control.Monad

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

--import System.Console.CmdArgs (whenLoud, whenNormal)
import qualified System.Info as Sys
import System.IO
import System.Posix.Process
import System.Process
-- }}}


instance Ord Modifier where
    m <= m' =  fromEnum m <= fromEnum m'


-- {{{ Keys-related functions
-- | Converts a keyVal to a String.
-- For printable characters, the corresponding String is returned, except for the space character for which "<Space>" is returned.
-- For non-printable characters, the corresponding keyName between <> is returned.
-- For modifiers, Nothing is returned.
keyToString :: KeyVal -> Maybe String
keyToString keyVal = case keyToChar keyVal of
    Just ' '    -> Just "<Space>"
    Just char   -> Just [char]
    _           -> case keyName keyVal of
        "Caps_Lock"         -> Nothing
        "Shift_L"           -> Nothing
        "Shift_R"           -> Nothing
        "Control_L"         -> Nothing
        "Control_R"         -> Nothing
        "Alt_L"             -> Nothing
        "Alt_R"             -> Nothing
        "Super_L"           -> Nothing
        "Super_R"           -> Nothing
        "Menu"              -> Nothing
        "ISO_Level3_Shift"  -> Nothing
        "dead_circumflex"   -> Just "^"
        "dead_diaeresis"    -> Just "¨"
        x                   -> Just ('<':x ++ ">")

-- | Convert key bindings list to a map.
-- Calls importKeyBindings'.
importKeyBindings :: KeysList -> Map.Map (Set.Set Modifier, String) (IO ())
importKeyBindings list = Map.fromList $ importKeyBindings' list

-- | Convert modifiers list to modifiers sets.
-- The order of modifiers in key bindings don't matter.
-- Called by importKeyBindings.
importKeyBindings' :: KeysList -> [((Set.Set Modifier, String), IO ())]
importKeyBindings' (((a, b), c):t) = ((Set.fromList a, b), c):(importKeyBindings' t)
importKeyBindings' _ = []
-- }}}


-- {{{ Run commands
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
-- }}}

-- | Set a temporary markup text to a label that disappears after some delay.
labelSetMarkupTemporary :: Label -> String -> Int -> IO HandlerId
labelSetMarkupTemporary label text delay = do
    labelSetMarkup label text
    timeoutAdd clear delay
  where
    clear = labelSetMarkup label "" >> return False


getAllProcessIDs :: IO [FilePath]
getAllProcessIDs = do 
    (_, pids, _)  <- readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- getProcessID

    return $ delete (show myPid) . nub . words $ pids ++ " " ++ pids'
    
    


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
