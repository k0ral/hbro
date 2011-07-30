module Hbro.Extra.BookmarksQueue where

-- {{{ Imports
import Hbro.Types

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Graphics.UI.Gtk.WebKit.WebView

import System.Environment
-- }}}


-- | Add current URI to the end of the queue.
append :: Browser -> IO ()
append browser = do
    uri         <- webViewGetUri (mWebView $ mGUI browser)
    configHome  <- getEnv "XDG_CONFIG_HOME"

    case uri of
        Just u -> appendFile (configHome ++ "/hbro/queue") (u ++ "\n")
        _ -> return ()

-- | Add current URI to the beginning of the queue.
push :: Browser -> IO ()
push browser = do
    uri         <- webViewGetUri (mWebView $ mGUI browser)
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- catch (T.readFile $ configHome ++ "/hbro/queue") (\_error -> return T.empty)

    case uri of 
        Just u ->
            if (file == T.empty)
                then return ()
                else do
                    let fileLines = T.lines file
                    let file' = T.unlines . nub $ (T.pack u):fileLines
                
                    T.writeFile (configHome ++ "/hbro/queue") file'

                    return ()
        _ -> return ()

-- | Return the first URI from the queue, while removing it.
popFront :: IO String
popFront = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- catch (T.readFile $ configHome ++ "/hbro/queue") (\_error -> return T.empty)

    if file == T.empty
        then return ""
        else do
            let fileLines = T.lines file
            let file' = T.unlines . tail . nub $ fileLines
        
            T.writeFile (configHome ++ "/hbro/queue.old") file
            T.writeFile (configHome ++ "/hbro/queue") file'

            return $ T.unpack (head fileLines)

-- | Return the last URI from the queue, while removing it.
popBack :: IO String
popBack = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- catch (T.readFile $ configHome ++ "/hbro/queue") (\_error -> return T.empty)

    if file == T.empty
        then return ""
        else do
            let fileLines = T.lines file
            let file' = T.unlines . reverse . tail . reverse. nub $ fileLines
        
            T.writeFile (configHome ++ "/hbro/queue.old") file
            T.writeFile (configHome ++ "/hbro/queue") file'

            return $ T.unpack (head $ reverse fileLines)

-- | Return a random URI from the queue, while removing it.
-- popRandom :: Browser -> IO String
-- popRandom browser = do
--     configHome  <- getEnv "XDG_CONFIG_HOME"
--     file        <- catch (T.readFile $ configHome ++ "/hbro/queue") (\e -> return T.empty)

--     if file == T.empty
--         then return ""
--         else do
--             let fileLines = T.lines file
--             let file' = T.unlines . reverse . tail . reverse. nub $ fileLines
--         
--             T.writeFile (configHome ++ "/hbro/queue.old") file
--             T.writeFile (configHome ++ "/hbro/queue") file'

--             return $ T.unpack (head $ reverse fileLines)
    
