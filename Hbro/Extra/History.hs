module Hbro.Extra.History where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
import Hbro.Util

--import Control.Monad.Reader

import Data.List
import Data.Time

--import System.IO.Error
import System.Locale
-- }}} 


-- | Add a single history entry to the history file
add :: FilePath   -- ^ Path to history file
    -> String     -- ^ URI for the new history entry
    -> String     -- ^ Title for the new history entry
    -> IO ()
add historyFile uri title = do
    now        <- getCurrentTime
    let time    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    let newLine = time ++ " " ++ uri ++ " " ++ title ++ "\n"
    _ <- catch (appendFile historyFile newLine) (\_ -> putStrLn $ "[WARNING] You need to create history file" ++ historyFile)
    return ()
    

-- | Open a dmenu to select a history entry
select :: FilePath           -- ^ Path to history file
       -> [String]           -- ^ Options to pass to dmenu
       -> IO (Maybe String)  -- ^ Selected history entry
select historyFile dmenuOptions  = do
    file <- readFile historyFile

    let file' = unlines . nub $ map reformat (sort . lines $ file)

    (>>= (return . head . words)) `fmap` (dmenu dmenuOptions file')

reformat :: String -> String
reformat line = 
  let
        _date:_time:uri:title = words line 
  in 
        unwords $ [uri] ++ title
    
