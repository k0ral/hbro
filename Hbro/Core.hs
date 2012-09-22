{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Hbro.Core where

-- {{{ Imports
import Hbro.Types
import Hbro.Util as H
import Hbro.Webkit.WebView as WebView

import Control.Monad.Error  hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.Default
-- import Data.Foldable
-- import Data.Functor

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView

import Prelude hiding(concat, mapM_)
-- }}}

-- {{{ Util
{-getState :: (MonadIO m, MonadError HError m, Typeable a) => String -> a -> m a
getState key defaultValue = do
    customMap <- gets _custom
    let result = fromDynamic =<< M.lookup key customMap
    case result of
        Just value -> return value
        _          -> do
            modify $ \s -> s { _custom = M.insert key (toDyn defaultValue) customMap }
            return defaultValue-}
-- }}}

goHome :: (MonadIO m, MonadReader r m, HasConfig r, HasWebView r, MonadError HError m) => m ()
goHome = loadURI =<< asks _homePage

quit :: (MonadIO m) => m ()
quit = io mainQuit

-- {{{ Misc
-- | Execute a javascript file on current webpage.
executeJSFile :: (MonadIO m) => FilePath -> WebView -> m ()
executeJSFile filePath webView = do
    logNormal $ "Executing Javascript file: " ++ filePath
    script <- io $ readFile filePath
    let script' = unwords . map (++ "\n") . lines $ script

    io $ webViewExecuteScript webView script'
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
