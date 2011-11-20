module Hbro.Extra.Session where

-- {{{ Imports
--import Hbro.Types
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

setupSession :: WebView -> IO ()
setupSession webView = do
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

sessionFilesFilter :: String -> Bool
sessionFilesFilter ('s':'e':'s':'s':'i':'o':'n':'.':_) = True
sessionFilesFilter _ = False


loadFromSession :: [String] -> IO ()
loadFromSession dmenuOptions = do
    configHome      <- getEnv "XDG_CONFIG_HOME"
    previousSession <- getDirectoryContents (configHome ++ "/hbro/sessions/") >>= return . (filter sessionFilesFilter)
    sessionURIs     <- mapM getSessionURI previousSession 

    selection <- dmenu dmenuOptions (T.unlines sessionURIs)
    
    case selection of
        ""      -> return ()
        u       -> do
            _ <- spawn "hbro" ["-u", u]
            return ()
    
getSessionURI :: String -> IO T.Text
getSessionURI fileName = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- T.readFile $ configHome ++ "/hbro/sessions/" ++ fileName

    return $ (head . T.lines) file

