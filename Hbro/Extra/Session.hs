module Hbro.Extra.Session where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView

import System.Directory
import System.Environment
import System.Glib.Signals
import System.IO
import System.Posix.Process
import System.Process 
-- }}}

setupSession :: Browser -> IO ()
setupSession browser = do
    pid             <- getProcessID
    configHome      <- getEnv "XDG_CONFIG_HOME"
    previousSession <- getDirectoryContents (configHome ++ "/hbro/") >>= return . (filter sessionFilesFilter)

    let sessionFile = configHome ++ "/hbro/session." ++ show pid

    _ <- on webView loadFinished $ \_ -> do        
        uri <- webViewGetUri webView
        case uri of
            Just u -> writeFile sessionFile u
            _      -> return ()

    _ <- quitAdd 0 $ do
        removeFile sessionFile
        return False
    
    return ()

  where
    webView = mWebView $ mGUI browser

sessionFilesFilter :: String -> Bool
sessionFilesFilter ('s':'e':'s':'s':'i':'o':'n':'.':_) = True
sessionFilesFilter _ = False


loadFromSession :: Browser -> IO ()
loadFromSession browser = do
    configHome      <- getEnv "XDG_CONFIG_HOME"
    previousSession <- getDirectoryContents (configHome ++ "/hbro/") >>= return . (filter sessionFilesFilter)
    sessionURIs     <- mapM getSessionURI previousSession 


    (Just input, Just output, _, _) <- createProcess (proc "dmenu" ["-l", "10"]) {
        std_in = CreatePipe,
        std_out = CreatePipe }
    _ <- T.hPutStr input (T.unlines sessionURIs)
    
    uri <- catch (hGetLine output) (\_error -> return "ERROR" )
    
    case uri of
        "ERROR" -> return ()
        ""      -> return ()
        u       -> do
            _ <- spawn (proc "hbro" ["-u", uri])
            return ()
    
getSessionURI :: String -> IO T.Text
getSessionURI fileName = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- T.readFile $ configHome ++ "/hbro/" ++ fileName

    return $ (head . T.lines) file

