module Hbro.Extra.History (
    Entry(..),
    add,
    parseEntry,
    select
) where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
import Hbro.Util

import Control.Exception
--import Control.Monad.Reader

import Data.List
import Data.Time.Format
import Data.Time.LocalTime

import Network.URI

--import System.IO.Error
import System.IO
import System.Locale
-- }}} 

-- {{{ Type definitions
data Entry = Entry {
    mTime  :: LocalTime,
    mURI   :: URI, 
    mTitle :: String
}

instance Show Entry where
    show (Entry time uri title) = unwords [(formatTime defaultTimeLocale dateFormat time), show uri, title]

dateFormat :: String
dateFormat = "%F %T"
-- }}}

-- | Add a new entry to history's database
add :: FilePath   -- ^ Path to history file
    -> Entry      -- ^ History entry to add
    -> IO Bool
add historyFile newEntry = do
    result <- try $ withFile historyFile AppendMode (`hPutStrLn` show newEntry)
    either (\e -> errorHandler historyFile e >> return False) (const $ return True) result
    

-- | Try to parse a String into a history Entry.
parseEntry :: String -> Maybe Entry
parseEntry [] = Nothing
parseEntry line = (parseEntry' . words) line

parseEntry' :: [String] -> Maybe Entry
parseEntry' (d:t:u:t') = do
    time <- parseTime defaultTimeLocale dateFormat (unwords [d, t])
    uri  <- parseURI u
    
    return $ Entry time uri (unwords t')
parseEntry' _ = Nothing

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: FilePath           -- ^ Path to history file
       -> [String]           -- ^ dmenu's commandline options
       -> IO (Maybe Entry)   -- ^ Selected history entry, if any
select historyFile dmenuOptions = do
    result <- try $ readFile historyFile
        
    either (\e -> errorHandler historyFile e >> return Nothing) (return . return) result
    >>= (return . ((return . unlines . reverse . sort . nub . lines) =<<))
    >>= (maybe (return Nothing) (dmenu dmenuOptions))
    >>= (return . (parseEntry =<<))


reformat :: String -> String
reformat line = 
  let
        _date:_time:uri:title = words line 
  in 
        unwords $ [uri] ++ title
    
