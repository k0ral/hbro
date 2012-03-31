{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Core (
-- * 'K'-monad
    runK,
    mapK,
    mapK2,
-- * Util
    with,
    withK,
    withTitle,    
    withURI,
-- * Read state
    getFaviconURI,
    getLoadProgress,
    getTitle,
    getURI,
    getState,
-- * Browse
    goBack,
    goForward,
    goHome,
    loadURI,
    reload,
    reloadBypassCache,
    stopLoading,
-- * Display
    zoomIn,
    zoomOut,
    Axis(..),
    Position(..),
    scroll,
-- * Misc
    searchText,
    toggleSourceMode,
    printPage,
    executeJSFile
) where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Control.Monad.Reader hiding(forM_, mapM_)

import Data.Dynamic
import Data.Foldable
import Data.Functor
import Data.IORef
import qualified Data.Map as M

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

import Prelude hiding(mapM_)

import System.Console.CmdArgs
-- }}}

-- {{{ 'K'-monad
-- | 'runReaderT' for 'K'-monad
runK :: Environment -> KT m a -> m a
runK env (KT function) = runReaderT function env

-- | 'mapReaderT' for 'K'-monad.
mapK :: (m a -> n b) -> KT m a -> KT n b
mapK function (KT env) = KT $ mapReaderT function env

-- | Like 'mapK', but monadic-input of filter can do little reading.
mapK2 :: ((c -> m a) -> n b) -> (c -> KT m a) -> KT n b
mapK2 f g = KT . ReaderT $ \b -> f (runK b . g)
-- }}}

-- {{{ Util
with :: (Environment -> a) -> (a -> IO b) -> K b
with selector callback = withK selector $ io . callback

withK :: (Environment -> a) -> (a -> K b) -> K b
withK selector callback = callback =<< asks selector

withTitle :: (String -> K ()) -> K ()
withTitle callback = (mapM_ callback) =<< getTitle

withURI :: (URI -> K ()) -> K ()
withURI callback = (mapM_ callback) =<< getURI 

getFaviconURI :: K (Maybe URI)
getFaviconURI = with (mWebView . mGUI) $ (return . (parseURI =<<)) <=< webViewGetIconUri

getLoadProgress :: K Double
getLoadProgress = with (mWebView . mGUI) webViewGetProgress

getURI :: K (Maybe URI)
getURI = with (mWebView . mGUI) $ (return . (parseURI =<<)) <=< webViewGetUri

getTitle :: K (Maybe String)
getTitle = with (mWebView . mGUI) webViewGetTitle

getState :: Typeable a => String -> a -> K (IORef a)  
getState key defaultValue = with mState $ \state -> do
    result <- (fromDynamic <=< M.lookup key) <$> readIORef state 
    case result of
        Just value -> return value
        _          -> do
            value <- newIORef defaultValue
            modifyIORef state . M.insert key . toDyn $ value            
            return value
-- }}}

-- {{{ Browsing
goBack, goForward, goHome :: K ()
goBack    = with  (mWebView . mGUI) webViewGoBack
goForward = with  (mWebView . mGUI) webViewGoForward
goHome    = withK (mHomePage . mConfig) $ mapM_ loadURI . parseURIReference

loadURI :: URI -> K ()
loadURI uri = do
    io . whenLoud . putStrLn . ("Loading URI: " ++) . show $ uri'
    with (mWebView . mGUI) (`webViewLoadUri` uri')
  where
    uri' = case uriScheme uri of
             [] -> "http://" ++ show uri
             _  -> show uri

reload, reloadBypassCache, stopLoading :: K ()
reload            = with (mWebView . mGUI) webViewReload
reloadBypassCache = with (mWebView . mGUI) webViewReloadBypassCache
stopLoading       = with (mWebView . mGUI) webViewStopLoading
-- }}}

-- {{{ Display
zoomIn, zoomOut :: K ()
zoomIn  = with (mWebView . mGUI) webViewZoomIn
zoomOut = with (mWebView . mGUI) webViewZoomOut

data Axis     = Horizontal | Vertical
data Position = Absolute Double | Relative Double

getAdjustment :: Axis -> (ScrolledWindow -> IO Adjustment)
getAdjustment Horizontal = scrolledWindowGetHAdjustment
getAdjustment Vertical   = scrolledWindowGetVAdjustment

-- | General scrolling command.                                                                                                                                                               
scroll :: Axis -> Position -> K ()
scroll axis percentage = with (mScrollWindow . mGUI) $ \window -> do
     adj     <- getAdjustment axis window
     page    <- adjustmentGetPageSize adj
     current <- adjustmentGetValue adj
     lower   <- adjustmentGetLower adj
     upper   <- adjustmentGetUpper adj
     
     let shift (Absolute x) = lower   + x/100 * (upper - page - lower)
         shift (Relative x) = current + x/100 * page
         limit x            = (x `max` lower) `min` (upper - page)
     
     adjustmentSetValue adj $ limit (shift percentage) 
-- }}}

-- {{{ Misc
searchText :: Bool -> Bool -> Bool -> String -> K Bool
searchText a b c text = with (mWebView . mGUI) $ \view -> webViewSearchText view text a b c     

-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: K ()
toggleSourceMode = do
    with (mWebView . mGUI) $ \view -> 
      webViewSetViewSourceMode view =<< (not <$> webViewGetViewSourceMode view)
    reload

-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: K ()
printPage = with (mWebView . mGUI) $ webViewGetMainFrame >=> webFramePrint

-- | Execute a javascript file on current webpage.
executeJSFile :: FilePath -> WebView -> IO ()
executeJSFile filePath webView = do
    whenNormal $ putStrLn ("Executing Javascript file: " ++ filePath)
    script <- readFile filePath
    let script' = unwords . map (\line -> line ++ "\n") . lines $ script

    webViewExecuteScript webView script'
-- }}}


-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
_savePage :: String -> WebView -> IO ()
_savePage _path webView = do
    frame        <- webViewGetMainFrame webView
    dataSource   <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()
