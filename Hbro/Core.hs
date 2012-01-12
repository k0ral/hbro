{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}
module Hbro.Core (
-- * Browsing
    goHome,
-- * Scrolling
    Axis(..),
    Position(..),
    scroll,
-- * Misc
    printPage,
    executeJSFile
) where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Data.Foldable

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri, webViewLoadUri)

import Network.URI

import System.Console.CmdArgs
-- }}}

-- {{{ Browsing
-- | Load homepage (set from configuration file).
goHome :: WebView -> Config -> IO ()
goHome webView config@Config{ mHomePage = homeURI } = forM_ (parseURIReference homeURI) $ webViewLoadUri webView
-- }}}

-- {{{ Scrolling
data Axis     = Horizontal | Vertical
data Position = Absolute Double | Relative Double

getAdjustment :: Axis -> (ScrolledWindow -> IO Adjustment)
getAdjustment Horizontal = scrolledWindowGetHAdjustment
getAdjustment Vertical   = scrolledWindowGetVAdjustment

-- | General scrolling command.                                                                                                                                                               
scroll :: ScrolledWindow -> Axis -> Position -> IO ()
scroll window axis percentage = do
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
-- | Wrapper around webFramePrint function, provided for convenience.
printPage :: WebView -> IO ()
printPage webView = webViewGetMainFrame webView >>= webFramePrint


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
