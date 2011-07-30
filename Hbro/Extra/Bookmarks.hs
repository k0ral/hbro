module Hbro.Extra.Bookmarks (
    addWithTags,
    addAllWithTags,
    load,
    loadWithTag,
    deleteWithTag,
    add,
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Gui
import Hbro.Types
import Hbro.Util

import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.WebKit.WebView

import System.Environment
import qualified System.Info as Sys
import System.IO
import System.Posix.Process
import System.Process 
import System.ZMQ 
-- }}}


-- | Add current URI to bookmarks.
-- Prompt for a tags list to apply.
addWithTags :: Browser -> IO ()
addWithTags browser = do
    uri <- webViewGetUri (mWebView $ mGUI browser)

    case uri of
        Just u -> prompt "Bookmark with tags:" "" False browser (\b -> do 
            tags <- entryGetText (mPromptEntry $ mGUI b)
            add u (words tags)) 
        _ -> return ()

-- | Add current URIs from all opened windows to bookmarks.
addAllWithTags :: Browser -> IO ()
addAllWithTags browser = 
    prompt "Bookmark all instances with tag:" "" False browser (\b -> do 
        tags          <- entryGetText (mPromptEntry $ mGUI b)
        uri           <- webViewGetUri (mWebView $ mGUI browser)
        case uri of
            Just u -> add u $ words tags
            _      -> return ()

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

                send reqSocket (B.pack "getUri") []
                uri <- receive reqSocket []

                add (B.unpack uri) (words tags)
            )

        _ <- mapM bookmarkPID pidsList
        return ())

-- |
load :: Browser -> IO ()
load browser = do 
    -- Load bookmarks file
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- T.readFile $ configHome ++ "/hbro/bookmarks"

    -- Reformat lines
    let file' = T.unlines . sort . nub $ map reformat (T.lines file)

    -- Let user select a URI
    (Just input, Just output, _, _) <- createProcess (proc "dmenu" ["-l", "10"]) {
        std_in = CreatePipe,
        std_out = CreatePipe }
    _ <- T.hPutStr input file'

    entry <- catch (hGetLine output) (\e -> return "ERROR" )
    case reverse . words $ entry of
        ["ERROR"]   -> return ()
        uri:_       -> loadURI uri browser
        _           -> return ()

  
reformat :: T.Text -> T.Text
reformat line =
    T.unwords $ tags' ++ [uri]
  where
    uri:tags = T.words line 
    tags'    = sort $ map (\tag -> T.snoc (T.cons '[' tag) ']') tags

-- | 
loadWithTag :: Browser -> IO ()        
loadWithTag browser = do
    -- Read bookmarks file
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- T.readFile $ configHome ++ "/hbro/bookmarks"

    -- Filter tags list
    let list = T.unlines . sort . nub . T.words . T.unwords $ map getTags (T.lines file)

    -- Let user select a tag
    (Just input, Just output, _, _) <- createProcess (proc "dmenu" ["-l", "10"]) {
        std_in = CreatePipe,
        std_out = CreatePipe }
    _ <- T.hPutStr input list

    tag <- catch (hGetLine output) (\e -> return "ERROR" )
    case tag of
        "ERROR" -> return ()
        ""      -> return ()
        t       -> do
            _ <- mapM (\uri -> runExternalCommand ("hbro -u \"" ++ (T.unpack uri) ++ "\"")) uris
            return ()
          where
            file' = filter (tagFilter $ T.pack t) (T.lines file)
            uris  = map getUri file'

-- 
tagFilter :: T.Text -> T.Text -> Bool
tagFilter tag line = let uri:tags = T.words line in case (intersect [tag] tags) of
    [t] -> True
    _   -> False


-- |
deleteWithTag :: Browser -> IO ()
deleteWithTag browser = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- T.readFile $ configHome ++ "/hbro/bookmarks"

    let tagsList = T.unlines . sort . nub . T.words . T.unwords $ map getTags (T.lines file)

    (Just input, Just output, _, _) <- createProcess (proc "dmenu" []) {
        std_in = CreatePipe,
        std_out = CreatePipe }
    _ <- T.hPutStr input tagsList

    tag <- catch (hGetLine output) (\e -> return "ERROR" )
    case tag of
        "ERROR" -> return ()
        ""      -> return ()
        t       -> do
            T.writeFile (configHome ++ "/hbro/bookmarks.old") file
            T.writeFile (configHome ++ "/hbro/bookmarks")     file'
            return ()
          where
            file' = T.unlines $ filter (not . (tagFilter $ T.pack tag)) (T.lines file)


-- | The elementary bookmark action
add :: String -> [String] -> IO ()
add uri tags = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    appendFile (configHome ++ "/hbro/bookmarks") $ uri ++ " " ++ (unwords tags) ++ "\n"

-- |
getTags :: T.Text -> T.Text
getTags line = let _:tags = T.words line in T.unwords tags

-- |
getUri :: T.Text -> T.Text
getUri line = let uri:_ = T.words line in uri
