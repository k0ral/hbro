{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Core (
-- * Browsing
    goHome,
    loadURI,
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

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri)
import qualified Graphics.UI.Gtk.WebKit.WebView as Webkit (webViewGetUri)

import Network.URI

import System.Console.CmdArgs
-- }}}

-- {{{ Browsing
-- | Load homepage (set from configuration file).
goHome :: WebView -> Config -> IO ()
goHome webView config = do
    whenLoud $ putStrLn ("Loading homepage: " ++ uri)
    loadURI webView uri
  where
    uri = mHomePage config

-- | Wrapper around webViewLoadUri meant to transparently add the proper protocol prefix (http:// or file://).
-- Most of the time, you want to use this function instead of webViewLoadUri.
loadURI :: WebView -> String -> IO ()
loadURI webView uri = do
    whenLoud $ putStrLn ("Loading URI: " ++ uri)
    
    let uri' = parseURIReference uri
    case (uri', uriScheme `fmap` uri') of
        (Just _, Just []) -> webViewLoadUri webView $ "http://" ++ uri
        (Just _, Just _)  -> webViewLoadUri webView uri
        _                 -> whenNormal $ putStrLn ("WARNING: not a valid URI: " ++ uri)
-- }}}

-- {{{ Scrolling
-- | Scroll up to top of web page. Provided for convenience.
goTop :: ScrolledWindow -> IO ()
goTop window = do
    adjustment  <- scrolledWindowGetVAdjustment window
    lower       <- adjustmentGetLower adjustment

    adjustmentSetValue adjustment lower

-- | Scroll down to bottom of web page. Provided for convenience.
goBottom :: ScrolledWindow -> IO ()
goBottom window = do
    adjustment  <- scrolledWindowGetVAdjustment window
    upper       <- adjustmentGetUpper adjustment

    adjustmentSetValue adjustment upper

-- | Scroll to the left edge of web page. Provided for convenience.
goLeft :: ScrolledWindow -> IO ()
goLeft window = do
    adjustment  <- scrolledWindowGetHAdjustment window
    lower       <- adjustmentGetLower adjustment

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
webViewGetUri webView = (>>= parseURI) `fmap` Webkit.webViewGetUri webView