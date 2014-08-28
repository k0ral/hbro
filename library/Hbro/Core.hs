{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Hbro.Core (
-- * Types
      CaseSensitivity(..)
    , Direction(..)
    , Wrap(..)
    , ZoomDirection(..)
-- * Getters
    , getCurrentURI
    , getFaviconURI
    , getFavicon
    , getLoadProgress
    , getPageTitle
-- * Browsing
    , goHome
    , load
    , reload
    , reloadBypassCache
    , stopLoading
    , goBack
    , goForward
-- * Other
    , printPage
    , searchText
    , searchText_
    , quit
    , executeJSFile
    ) where

-- {{{ Imports
import           Graphics.UI.Gtk.WebKit.Lifted.WebView

import           Hbro.Config                           as Config
import           Hbro.Error
import           Hbro.Gui                              as Gui
import           Hbro.Logger                           hiding (initialize)
import           Hbro.Prelude                          as H

import           Control.Monad.Reader                  hiding (guard, unless)

import           Graphics.UI.Gtk.Gdk.Pixbuf            (Pixbuf)
import           Graphics.UI.Gtk.General.General
import           Graphics.UI.Gtk.WebKit.WebDataSource
import           Graphics.UI.Gtk.WebKit.WebFrame

import           Network.URI.Monadic
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
-- }}}

-- {{{ Getters
getCurrentURI :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => m URI
getCurrentURI = webViewGetUri =<< Gui.get webViewL

getFaviconURI :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => m URI
getFaviconURI = webViewGetIconUri =<< Gui.get webViewL

getFavicon :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => Int -> Int -> m Pixbuf
getFavicon w h = (\v -> webViewTryGetFaviconPixbuf v w h) =<< Gui.get webViewL

getLoadProgress :: (BaseIO m, MonadReader t m, HasGUI t) => m Double
getLoadProgress = gSync . webViewGetProgress =<< Gui.get webViewL

getPageTitle :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => m Text
getPageTitle = webViewGetTitle =<< Gui.get webViewL
-- }}}

-- {{{ Browsing
goHome :: (BaseIO m, MonadReader t m, HasGUI t, HasConfig t, MonadError Text m) => m ()
goHome = load =<< Config.get homePageL

load :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => URI -> m ()
load uri = do
    debugM "hbro.core" $ "Loading URI: " ++ tshow uri
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


-- load' :: (MonadBaseControl IO m, MonadReader t m, HasGUI t, HasHTTPClient t, MonadError Text m) => URI -> m ()
-- load' uri = do
--     page <- Client.retrieve uri
--     -- render page =<< Client.getURI
--     render page uri


reload, goBack, goForward :: (BaseIO m, MonadReader t m, HasGUI t, MonadError Text m) => m ()
-- reload    = load  =<< Client.getURI
-- goBack    = load' =<< Browser.stepBackward =<< getURI
-- goForward = load' =<< Browser.stepForward =<< getURI
reload    = gAsync . webViewReload    =<< Gui.get webViewL
goBack    = gAsync . webViewGoBack    =<< Gui.get webViewL
goForward = gAsync . webViewGoForward =<< Gui.get webViewL

reloadBypassCache, stopLoading :: (BaseIO m, MonadReader t m, HasGUI t) => m ()
reloadBypassCache = Gui.get webViewL >>= gAsync . webViewReloadBypassCache >> logDebug "Reloading without cache."
stopLoading = Gui.get webViewL >>= gAsync . webViewStopLoading >> logDebug "Stopped loading"
-- }}}


-- {{{
searchText :: (BaseIO m, MonadReader t m, HasGUI t) => CaseSensitivity -> Direction -> Wrap -> Text -> m Bool
searchText s d w text = do
    logDebug $ "Searching text: " ++ text
    v <- Gui.get webViewL
    gSync $ webViewSearchText v text (toBool s) (toBool d) (toBool w)

searchText_ :: (BaseIO m, MonadReader t m, HasGUI t) => CaseSensitivity -> Direction -> Wrap -> Text -> m ()
searchText_ s d w text = void $ searchText s d w text

printPage :: (BaseIO m, MonadReader t m, HasGUI t) => m ()
printPage = gAsync . webFramePrint =<< gSync . webViewGetMainFrame =<< Gui.get webViewL
-- }}}

-- | Terminate the program.
quit :: (BaseIO m) => m ()
quit = gAsync mainQuit


-- {{{ Misc
-- | Execute a javascript file on current webpage.
executeJSFile :: (BaseIO m, MonadReader r m) => FilePath -> WebView -> m ()
executeJSFile filePath webView' = do
    debugM "hbro.core" $ "Executing Javascript file: " ++ fpToText filePath
    script <- readFile filePath
    let script' = asText . unwords . map (++ "\n") . lines $ script

    gAsync $ webViewExecuteScript webView' script'
-- }}}

-- | Save current web page to a file,
-- along with all its resources in a separated directory.
-- Doesn't work for now, because web_resource_get_data's binding is missing...
_savePage :: Text -> WebView -> IO ()
_savePage _path webView' = do
    frame         <- webViewGetMainFrame webView'
    dataSource    <- webFrameGetDataSource frame
    _mainResource <- webDataSourceGetMainResource dataSource
    _subResources <- webDataSourceGetSubresources dataSource
    return ()


-- {{{ Util
logDebug :: (BaseIO m) => Text -> m ()
logDebug = debugM "hbro.core"
-- }}}
