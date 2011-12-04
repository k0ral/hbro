module Hbro.Extra.History where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
import Hbro.Util

--import Control.Monad.Reader

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
    file <- T.readFile historyFile

    let file' = T.unlines . nub $ map reformat (sort . T.lines $ file)

    selection <- dmenu dmenuOptions file'
    case selection of
        ""    -> return Nothing
        entry -> return $ Just ((head . words) entry)

reformat :: T.Text -> T.Text
reformat line = 
  let
        _date:_time:uri:title = T.words line 
  in 
        T.unwords $ [uri] ++ title
    
