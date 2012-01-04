{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Core (
-- * Browsing
    goHome,
    webViewLoadUri,
-- * Scrolling    
    goTop,
    goBottom,
    goLeft,
    goRight,
-- * Misc
    printPage,
    executeJSFile,
    webViewGetUri
) where

-- {{{ Imports
import Hbro.Types
--import Hbro.Util

import Data.Foldable

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri, webViewLoadUri)
import qualified Graphics.UI.Gtk.WebKit.WebView as WebKit (webViewGetUri, webViewLoadUri)

import Network.URI

import System.Console.CmdArgs
-- }}}

-- {{{ Browsing
-- | Load homepage (set from configuration file).
goHome :: WebView -> Config -> IO ()
goHome webView config@Config{ mHomePage = homeURI } = forM_ (parseURIReference homeURI) $ webViewLoadUri webView

-- | Wrapper around webViewLoadUri using Network.URI instead of a bare String.
webViewLoadUri :: WebView -> URI -> IO ()
webViewLoadUri webView uri = do
    whenLoud $ putStrLn ("Loading URI: " ++ show uri)
    case uriScheme uri of
        [] -> WebKit.webViewLoadUri webView ("http://" ++ show uri)
        _  -> WebKit.webViewLoadUri webView (show uri)
-- }}}

-- {{{ Scrolling
-- | Scroll up to top of web page. Provided for convenience.
goTop :: ScrolledWindow -> IO ()
goTop window = do
    adjustment <- scrolledWindowGetVAdjustment window
    lower      <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- | Scroll down to bottom of web page. Provided for convenience.
goBottom :: ScrolledWindow -> IO ()
goBottom window = do
    adjustment <- scrolledWindowGetVAdjustment window
    upper      <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper

-- | Scroll to the left edge of web page. Provided for convenience.
goLeft :: ScrolledWindow -> IO ()
goLeft window = do
    adjustment <- scrolledWindowGetHAdjustment window
    lower      <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- | Scroll to the right edge of web page. Provided for convenience.
goRight :: ScrolledWindow -> IO ()
goRight window = do
    adjustment  <- scrolledWindowGetHAdjustment window
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper 
-- }}}
    
-- {{{ Misc
-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: WebView -> IO ()
printPage webView = do
    frame <- webViewGetMainFrame webView
    webFramePrint frame


-- | Execute a javascript file on current webpage.
executeJSFile :: String -> WebView -> IO ()
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

-- | Replacement for Graphics.UI.Gtk.WebKit.WebView(webViewGetUri), using the Network.URI type.
webViewGetUri :: WebView -> IO (Maybe URI)
webViewGetUri webView = (>>= parseURI) `fmap` WebKit.webViewGetUri webView