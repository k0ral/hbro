module Hbro.Extra.BookmarksQueue where

-- {{{ Imports
--import Hbro.Types

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment
-- }}}


-- | Add current URI to the end of the queue.
append :: String -> IO ()
append uri = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    appendFile (configHome ++ "/hbro/queue") (uri ++ "\n")

-- | Add current URI to the beginning of the queue.
push :: String -> IO ()
push uri = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- catch (T.readFile $ configHome ++ "/hbro/queue") (\_error -> return T.empty)

    if (file == T.empty)
      then return ()
      else do
        let fileLines = T.lines file
        let file' = T.unlines . nub $ (T.pack uri):fileLines
        
        T.writeFile (configHome ++ "/hbro/queue") file'
        
        return ()

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
    
