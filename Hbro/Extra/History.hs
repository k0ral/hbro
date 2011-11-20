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

import System.Locale
-- }}} 


-- |
add :: FilePath -> String -> String -> IO ()
add historyFile uri title = do
    now        <- getCurrentTime
    let time    = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    appendFile historyFile $ time ++ " " ++ uri ++ " " ++ title ++ "\n"

-- |
select :: FilePath -> [String] -> IO (Maybe String)
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
    
