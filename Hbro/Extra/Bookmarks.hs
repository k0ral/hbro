module Hbro.Extra.Bookmarks (
    select,
    selectTag,
    deleteWithTag,
    add,
) where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Gui
--import Hbro.Types
import Hbro.Util

--import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- }}}



-- | The elementary bookmark action
add :: FilePath -> String -> [String] -> IO ()
add bookmarksFile uri tags = appendFile bookmarksFile $ uri ++ " " ++ (unwords tags) ++ "\n"


-- |
select :: FilePath -> [String] -> IO (Maybe String)
select bookmarksFile dmenuOptions = do
-- Load bookmarks file
    file <- T.readFile bookmarksFile
    let file' = T.unlines . sort . nub $ map reformat (T.lines file)

-- Let user select a URI
    selection <- dmenu dmenuOptions file'

    case reverse . words $ selection of
        []          -> return Nothing
        uri:_       -> return $ Just uri

  
reformat :: T.Text -> T.Text
reformat line = T.unwords $ tags' ++ [uri]
  where
    uri:tags = T.words line 
    tags'    = sort $ map (\tag -> T.snoc (T.cons '[' tag) ']') tags

-- | 
selectTag :: FilePath -> [String] -> IO (Maybe [String])
selectTag bookmarksFile dmenuOptions = do
    -- Read bookmarks file
    file <- T.readFile bookmarksFile

    -- Filter tags list
    let list = T.unlines . sort . nub . T.words . T.unwords $ map getTags (T.lines file)

    -- Let user select a tag
    selection <- dmenu dmenuOptions list

    case selection of
        ""  -> return Nothing
        tag -> return $ Just uris
--mapM (\uri -> spawn "hbro" ["-u", (T.unpack uri)]) uris >> return ()
          where
            file' = filter (tagFilter $ T.pack tag) (T.lines file)
            uris  = map (T.unpack . getUri) file'

-- 
tagFilter :: T.Text -> T.Text -> Bool
tagFilter tag line = let _uri:tags = T.words line in case (intersect [tag] tags) of
    [_tag] -> True
    _      -> False


-- |
deleteWithTag :: FilePath -> [String] -> IO ()
deleteWithTag bookmarksFile dmenuOptions = do
    file <- T.readFile bookmarksFile

    let tagsList = T.unlines . sort . nub . T.words . T.unwords $ map getTags (T.lines file)

    selection <- dmenu dmenuOptions tagsList
    case selection of
        ""      -> return ()
        _       -> do
            T.writeFile (bookmarksFile ++ ".old") file
            T.writeFile bookmarksFile file'
            return ()
          where
            file' = T.unlines $ filter (not . (tagFilter $ T.pack selection)) (T.lines file)

-- |
getTags :: T.Text -> T.Text
getTags line = let _:tags = T.words line in T.unwords tags

-- |
getUri :: T.Text -> T.Text
getUri line = let uri:_ = T.words line in uri
