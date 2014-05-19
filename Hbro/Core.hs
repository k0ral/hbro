{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Hbro.Core (
-- * Types
    CaseSensitivity(..),
    Direction(..),
    Wrap(..),
    ZoomDirection(..),
-- * Getters
    getCurrentURI,
    getFaviconURI,
    getLoadProgress,
    getPageTitle,
-- * Browsing
    goHome,
    load,
    reload,
    reloadBypassCache,
    stopLoading,
    goBack,
    goForward,
-- * Other
    printPage,
    searchText,
    searchText_,
    quit,
    executeJSFile,
) where

-- {{{ Imports
import Hbro.Config as Config
import Hbro.Error
import Hbro.Gui as Gui
import Hbro.Util as H

import Control.Monad.Reader hiding(guard, unless)

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame as W
import Graphics.UI.Gtk.WebKit.WebView as W

import Network.URI.Monadic

import Prelude hiding(concat, mapM_)
-- }}}

-- {{{ Types
data CaseSensitivity = CaseSensitive | CaseInsensitive

instance ToBool CaseSensitivity where
    toBool CaseSensitive   = True
    toBool CaseInsensitive = False

data Direction = Forward | Backward

instance ToBool Direction where
    toBool Forward  = True
    toBool Backward = False

data Wrap = Wrap | NoWrap

instance ToBool Wrap where
    toBool Wrap   = True
    toBool NoWrap = False

data ZoomDirection = In | Out

data TitleUnavailable = TitleUnavailable deriving(Typeable)
instance Exception TitleUnavailable
instance Show TitleUnavailable where show _ = "No available title."
-- }}}

-- {{{ Getters
getCurrentURI :: (MonadBase IO m, MonadReader t m, HasGUI t, MonadThrow m) => m URI
getCurrentURI = Gui.get webViewL >>= gSync . webViewGetUri >>= maybe (throwM UnavailableURI) return >>= parseURI

getFaviconURI :: (MonadBase IO m, MonadReader t m, HasGUI t, MonadThrow m) => m URI
getFaviconURI = Gui.get webViewL >>= gSync . webViewGetIconUri >>= maybe (throwM UnavailableURI) return >>= parseURI

getLoadProgress :: (MonadBase IO m, MonadReader t m, HasGUI t) => m Double
getLoadProgress = gSync . webViewGetProgress =<< Gui.get webViewL

getPageTitle :: (MonadBase IO m, MonadReader t m, HasGUI t, MonadThrow m) => m String
getPageTitle = Gui.get webViewL >>= gSync . webViewGetTitle >>= maybe (throwM TitleUnavailable) return
-- }}}

-- {{{ Browsing
goHome :: (MonadBase IO m, MonadReader t m, HasGUI t, HasConfig t, MonadThrow m) => m ()
goHome = load =<< Config.get homePageL

load :: (MonadBase IO m, MonadReader t m, HasGUI t, MonadThrow m) => URI -> m ()
load uri = do
    io . debugM "hbro.core" $ "Loading URI: " ++ show uri
    -- void . logErrors $ do
    --     currentURI <- getURI
    --     guard (currentURI /= uri')
    --     Browser.advance currentURI

    -- load' uri'
    webview <- Gui.get webViewL
    gSync . webViewLoadUri webview $ show uri'

  where
    uri' = case uriScheme uri of
             [] -> uri { uriScheme = "http://" }
             _  -> uri
    -- baseOf uri = uri {
        -- uriPath = (++ "/") . join "/" . Prelude.init . split "/" $ uriPath uri
    -- }


-- load' :: (MonadBaseControl IO m, MonadReader t m, HasGUI t, HasHTTPClient t, MonadError String m) => URI -> m ()
-- load' uri = do
--     page <- Client.retrieve uri
--     -- render page =<< Client.getURI
--     render page uri


reload, goBack, goForward :: (MonadBase IO m, MonadReader t m, HasGUI t, MonadThrow m) => m ()
-- reload    = load  =<< Client.getURI
-- goBack    = load' =<< Browser.stepBackward =<< getURI
-- goForward = load' =<< Browser.stepForward =<< getURI
reload    = gAsync . webViewReload    =<< Gui.get webViewL
goBack    = gAsync . webViewGoBack    =<< Gui.get webViewL
goForward = gAsync . webViewGoForward =<< Gui.get webViewL

reloadBypassCache, stopLoading :: (MonadBase IO m, MonadReader t m, HasGUI t) => m ()
reloadBypassCache = Gui.get webViewL >>= gAsync . W.webViewReloadBypassCache >> logDebug "Reloading without cache."
stopLoading = Gui.get webViewL >>= gAsync . W.webViewStopLoading >> logDebug "Stopped loading"
-- }}}


-- {{{
searchText :: (MonadBase IO m, MonadReader t m, HasGUI t) => CaseSensitivity -> Direction -> Wrap -> String -> m Bool
searchText s d w text = do
    v <- Gui.get webViewL
    gSync $ W.webViewSearchText v text (toBool s) (toBool d) (toBool w)

searchText_ :: (MonadBase IO m, MonadReader t m, HasGUI t) => CaseSensitivity -> Direction -> Wrap -> String -> m ()
searchText_ s d w text = void $ searchText s d w text

printPage :: (MonadBase IO m, MonadReader t m, HasGUI t) => m ()
printPage = gAsync . W.webFramePrint =<< gSync . W.webViewGetMainFrame =<< Gui.get webViewL
-- }}}

-- | Terminate the program.
quit :: (MonadBase IO m) => m ()
quit = gAsync mainQuit


-- {{{ Misc
-- | Execute a javascript file on current webpage.
executeJSFile :: (MonadBase IO m, MonadReader r m) => FilePath -> WebView -> m ()
executeJSFile filePath webView' = do
    io . debugM "hbro.core" $ "Executing Javascript file: " ++ filePath
    script <- io $ readFile filePath
    let script' = unwords . map (++ "\n") . lines $ script

    gAsync $ webViewExecuteScript webView' script'
-- }}}

-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
_savePage :: String -> WebView -> IO ()
_savePage _path webView' = do
    frame         <- webViewGetMainFrame webView'
    dataSource    <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()


-- {{{ Util
logDebug :: (MonadBase IO m) => String -> m ()
logDebug = io . debugM "hbro.core"
-- }}}
