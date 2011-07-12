module Hbro.Extra.Bookmarks where

-- {{{ Imports
import Hbro.Core
import Hbro.Gui
import Hbro.Types
import Hbro.Util

import Data.ByteString.Char8 (pack, unpack)
import Data.List

import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.WebKit.WebView

import System.Environment
import System.Exit
import qualified System.Info as Sys
import System.Posix.Process
import System.Process 
import System.ZMQ 
-- }}}


-- 
addToBookmarks :: Browser -> IO ()
addToBookmarks browser = do
    uri <- webViewGetUri (mWebView $ mGUI browser)

    case uri of
        Just u -> prompt "Bookmark with tag:" "" False browser (\b -> do 
            tags <- entryGetText (mPromptEntry $ mGUI b)
            bookmark u (words tags)) 
        _ -> return ()

-- 
addAllInstancesToBookmarks :: Browser -> IO ()
addAllInstancesToBookmarks browser = 
    prompt "Bookmark all instances with tag:" "" False browser (\b -> do 
        tags          <- entryGetText (mPromptEntry $ mGUI b)
        (_, pids, _)  <- readProcessWithExitCode "pidof" ["hbro"] []
        (_, pids', _) <- readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
        myPid         <- getProcessID

        let pidsList    = delete (show myPid) . nub . words $ pids ++ " " ++ pids'
        let bookmarkPID = (\pid -> withContext 1 $ \context -> do
            withSocket context Req $ \reqSocket ->
              let 
                socketURI = "ipc://" ++ (mSocketDir $ mConfiguration browser) ++ "/hbro." ++ pid
              in do
                connect reqSocket socketURI

                send reqSocket (pack "getUri") []
                uri <- receive reqSocket []

                bookmark (unpack uri) (words tags)
            )

        _ <- mapM bookmarkPID pidsList
        return ())

-- |
loadFromBookmarks :: Browser -> IO ()
loadFromBookmarks browser = do 
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- readFile $ configHome ++ "/hbro/bookmarks"

    let file' = unlines . sort . nub $ map reformat (lines file)

    (code, result, e) <- readProcessWithExitCode "dmenu" ["-l", "10"] file'
    case (code, result) of
        (ExitSuccess, r) -> 
          let
            uri:_ = reverse . words $ r
          in
            loadURL uri browser
        _ -> putStrLn e

  where
    reformat line =
      let
        uri:tags = words line 
        tags'    = sort $ map (\tag -> "[" ++ tag ++ "]") tags
      in 
        unwords $ tags' ++ [uri]

-- | 
loadTagFromBookmarks :: Browser -> IO ()        
loadTagFromBookmarks browser = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- readFile $ configHome ++ "/hbro/bookmarks"

    let list = unlines . sort . nub . words . unwords $ map getTags (lines file)

    (code, result, e) <- readProcessWithExitCode "dmenu" [] list
    case (code, result) of
        (ExitSuccess, tag) -> 
          let
            file' = filter (tagFilter tag) (lines file)
            uris  = map getUri file'
          in do
            _ <- mapM (\uri -> runExternalCommand ("hbro -u \"" ++ uri ++ "\"")) uris
            return ()
        
        _ -> putStrLn e

  where
    tagFilter tag line = let uri:tags = words line in case (intersect [tag] tags) of
        [t] -> True
        _   -> False


-- |
deleteTagFromBookmarks :: Browser -> IO ()
deleteTagFromBookmarks browser = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- readFile $ configHome ++ "/hbro/bookmarks"

    let tagsList = unlines . sort . nub . words . unwords $ map getTags (lines file)

    (code, result, e) <- readProcessWithExitCode "dmenu" [] tagsList
    case (code, result) of
        (ExitSuccess, tag) ->
          let
            file' = unlines $ filter (tagFilter tag) (lines file)
          in do
            writeFile (configHome ++ "/hbro/bookmarks.old") file
            writeFile (configHome ++ "/hbro/bookmarks")     file'
            return ()
            

        _ -> putStrLn e
  where 
    tagFilter tag line = let uri:tags = words line in case (intersect [tag] tags) of
        [t] -> False
        _   -> True


-- The elementary bookmark action
bookmark :: String -> [String] -> IO ()
bookmark uri tags = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    appendFile (configHome ++ "/hbro/bookmarks") $ uri ++ " " ++ (unwords tags) ++ "\n"

-- |
getTags :: String -> String
getTags line = let _:tags = words line in unwords tags

-- |
getUri :: String -> String
getUri line = let uri:_ = words line in uri
-- }}}
