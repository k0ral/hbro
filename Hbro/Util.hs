module Hbro.Util (
-- * Process management
    spawn,
    getAllProcessIDs,
-- * WebKit functions redefinition
    webFrameGetUri,
    webViewGetUri,
    webViewLoadUri,
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
import qualified Graphics.UI.Gtk.WebKit.WebFrame as WebKit
import qualified Graphics.UI.Gtk.WebKit.WebView as WebKit

import Network.URI

import System.Console.CmdArgs
import qualified System.Info as Sys
import System.IO
import System.IO.Error
import System.Posix.Process
import System.Process
-- }}}


-- {{{ Process management
-- | Run external command and won't kill when parent process exit.
spawn :: String -> [String] -> IO ()
spawn command options = spawn' (proc command options)

spawn' :: CreateProcess -> IO ()
spawn' command = createProcess command { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True } >> return ()

-- | Return the list of process IDs corresponding to all running instances of the browser.
getAllProcessIDs :: IO [FilePath]
getAllProcessIDs = do 
    (_, pids, _)  <- readProcessWithExitCode "pidof" ["hbro"] []
    (_, pids', _) <- readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
    myPid         <- getProcessID

    return $ delete (show myPid) . nub . words $ pids ++ " " ++ pids'
-- }}}

-- {{{ Webkit functions redefinition
-- | Replacement for Graphics.UI.Gtk.WebKit.WebFrame(webFrameGetUri), using the Network.URI type.
webFrameGetUri :: WebKit.WebFrame -> IO (Maybe URI)
webFrameGetUri frame = (>>= parseURI) `fmap` WebKit.webFrameGetUri frame

-- | Replacement for Graphics.UI.Gtk.WebKit.WebView(webViewGetUri), using the Network.URI type.
webViewGetUri :: WebKit.WebView -> IO (Maybe URI)
webViewGetUri webView = (>>= parseURI) `fmap` WebKit.webViewGetUri webView

-- | Replacement for Graphics.UI.Gtk.WebKit.WebView(webViewLoadUri), using the Network.URI type.
webViewLoadUri :: WebKit.WebView -> URI -> IO ()
webViewLoadUri webView uri = do
    whenLoud $ putStrLn ("Loading URI: " ++ show uri)
    case uriScheme uri of
        [] -> WebKit.webViewLoadUri webView ("http://" ++ show uri)
        _  -> WebKit.webViewLoadUri webView (show uri)
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
