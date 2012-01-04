module Hbro.Extra.Session where

-- {{{ Imports
--import Hbro.Types
import Hbro.Util

import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri)

import Prelude hiding(mapM_)

import System.Directory
import System.Environment
import System.Glib.Signals
import System.IO
import System.Posix.Process
import System.Process 
-- }}}

setupSession :: WebView -> String -> IO ()
setupSession webView sessionDirectory = do
    pid             <- getProcessID
    previousSession <- getDirectoryContents sessionDirectory

    let sessionFile = sessionDirectory ++ show pid

    _ <- on webView loadFinished $ \_ -> do
        webViewGetUri webView >>= mapM_ ((writeFile sessionFile) . show)
        
    _ <- quitAdd 0 $ do
        removeFile sessionFile
        return False
    
    return ()


--loadFromSession :: [String] -> String -> IO ()
--loadFromSession dmenuOptions sessionDirectory = do
--    previousSession <- getDirectoryContents sessionDirectory
--    sessionURIs     <- mapM getSessionURI previousSession 
-- 
--    selection <- dmenu dmenuOptions (T.unlines sessionURIs)
--    
--    case selection of
--        ""      -> return ()
--        u       -> do
--            _ <- spawn "hbro" ["-u", u]
--            return ()
--    
--getSessionURI :: String -> IO T.Text
--getSessionURI fileName = do
--    configHome  <- getEnv "XDG_CONFIG_HOME"
--    file        <- T.readFile $ configHome ++ "/hbro/sessions/" ++ fileName
-- 
--    return $ (head . T.lines) file

