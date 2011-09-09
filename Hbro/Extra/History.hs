module Hbro.Extra.History where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Data.List
import Data.Time

import System.Environment
import System.Exit
import System.Locale
import System.Process 
-- }}} 


-- |
addToHistory :: String -> String -> IO ()
addToHistory uri title = do
    now        <- getCurrentTime
    configHome <- getEnv "XDG_CONFIG_HOME"
    let time    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    appendFile (configHome ++ "/hbro/history") $ time ++ " " ++ uri ++ " " ++ title ++ "\n"

-- |
loadFromHistory :: Browser -> IO ()
loadFromHistory browser = do
    configHome  <- getEnv "XDG_CONFIG_HOME"
    file        <- readFile $ configHome ++ "/hbro/history"

    let file' = unlines . nub $ map reformat (sort . lines $ file)

    (code, result, e) <- readProcessWithExitCode "dmenu" ["-l", "10"] file'
    return ()
    case (code, result) of
        (ExitSuccess, r) -> 
          let
            uri:_ = words $ r
          in
            loadURI uri browser
        _ -> putStrLn e

  where
    reformat line =
      let
        _date:_time:uri:title = words line 
      in 
        unwords $ [uri] ++ title
    
