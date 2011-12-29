module Hbro.Extra.Bookmarks (
    Entry(..),
    add,
    select,
    selectTag,
    deleteWithTag
) where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Gui
--import Hbro.Types
import Hbro.Util

import Control.Exception
import Control.Monad hiding(forM_, mapM_)

--import qualified Data.ByteString.Char8 as B
import Data.Foldable hiding(find, foldr)
import Data.List
import Data.Maybe

import Network.URI

import Prelude hiding(catch, mapM_)

import System.IO
-- }}}

-- {{{ Type definitions
data Entry = Entry {
    mURI  :: URI,
    mTags :: [String]
}
 
instance Show Entry where
    show (Entry uri tags) = unwords $ (show uri):tags
-- }}}

-- | Try to parse a String into a bookmark Entry.
parseEntry :: String -> Maybe Entry
parseEntry [] = Nothing
parseEntry line = return (words line) 
    >>= (\(h:t) -> parseURI h
    >>= (\uri -> listToMaybe t 
    >> (return $ Entry uri t)))

-- | Check if the given bookmark Entry is tagged with the given tag.
hasTag :: String -> Entry -> Bool
hasTag tag = isJust . (find $ (==) tag) . mTags

-- | Add a new entry to the bookmarks' database (which is a file).
add :: FilePath    -- ^ Bookmarks' database file
    -> Entry       -- ^ New bookmarks entry
    -> IO Bool
add bookmarksFile newEntry = do
    result <- try $ withFile bookmarksFile AppendMode (flip hPutStrLn (show newEntry))
    either (\e -> errorHandler bookmarksFile e >> return False) (const $ return True) result
          
-- | Open a dmenu with all (sorted alphabetically) bookmarks entries, and return the user's selection, if any.
select :: FilePath   -- ^ Bookmarks' database file
       -> [String]   -- ^ dmenu's commandline options
       -> IO (Maybe String)
select bookmarksFile dmenuOptions = do
    result <- try $ readFile bookmarksFile 
    
    either (\e -> errorHandler bookmarksFile e >> return Nothing) (\x -> return $ Just x) result
    >>= (return . ((return . unlines . sort . nub . (map reformat) . lines) =<<))
    >>= (maybe (return Nothing) (dmenu dmenuOptions))
    >>= (return . ((return . last. words) =<<))

  
reformat :: String -> String
reformat line = unwords $ tags' ++ [uri]
  where
    uri:tags = words line 
    tags'    = sort $ map (\tag -> '[':(tag ++ "]")) tags

-- | Open a dmenu with all (sorted alphabetically) bookmarks tags, and return the user's selection, if any.
selectTag :: FilePath          -- ^ Bookmarks' database file
          -> [String]          -- ^ dmenu's commandline options
          -> IO (Maybe [URI])
selectTag bookmarksFile dmenuOptions = do
    -- Read bookmarks file
    result <- try $ readFile bookmarksFile
    file   <- either (\e -> errorHandler bookmarksFile e >> return Nothing) (\x -> return $ Just x) result
    
    let entries = (return . catMaybes . (map parseEntry) . lines) =<< file
    let tags    = (return . unlines . sort . nub . words . unwords . (foldr (union . mTags) [])) =<< entries

    -- Let user select a tag
    tag <- (maybe (return Nothing) (dmenu dmenuOptions) tags)
    return $ (return . (map mURI) . (\t -> filter (hasTag t) (maybe [] id entries))) =<< tag


-- | Remove all bookmarks entries matching the given tag.
deleteWithTag :: FilePath   -- ^ Bookmarks' database file
              -> [String]   -- ^ dmenu's commandline options
              -> IO ()
deleteWithTag bookmarksFile dmenuOptions = do
    result <- try $ readFile bookmarksFile
    file   <- either (\e -> errorHandler bookmarksFile e >> return Nothing) (\x -> return $ Just x) result
    
    forM_ file $ \f -> do
        let entries = (catMaybes . (map parseEntry) . lines) f    
        let tags = (unlines . sort . nub . words . unwords . (foldr (union . mTags) [])) entries

        tag <- (dmenu dmenuOptions tags) 
        forM_ tag (\t -> do
            writeFile (bookmarksFile ++ ".old") $ unlines (map show entries)
            writeFile bookmarksFile $ (unlines . (map show) . (filter (not . (hasTag t)))) entries
            return ())