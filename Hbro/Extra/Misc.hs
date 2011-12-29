module Hbro.Extra.Misc where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
import Hbro.Util

import Data.Maybe

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebBackForwardList
import Graphics.UI.Gtk.WebKit.WebHistoryItem
import Graphics.UI.Gtk.WebKit.WebView

import System.IO
-- }}}


-- | Same as goBack function from Hbro.Core,
-- but with feedback in case of failure.
--goForward :: WebView -> IO ()
--goForward webView = do
--    result        <- webViewCanGoForward webView
--    feedbackLabel <- builderGetObject builder castToLabel "feedback"
-- 
--    case result of
--        True -> webViewGoForward webView
--        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go forward !</span>" 5000 >> return ()
-- 
--  where
--    webView = mWebView $ mGUI browser
--    builder = mBuilder $ mGUI browser
-- 
---- | Same as goBack function from Hbro.Core,
---- but with feedback in case of failure.
--goBack :: Browser -> IO ()
--goBack browser = do
--    result        <- webViewCanGoBack webView
--    feedbackLabel <- builderGetObject builder castToLabel "feedback"
-- 
--    case result of
--        True -> webViewGoBack webView
--        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go back !</span>" 5000 >> return ()
-- 
--  where
--    webView = mWebView $ mGUI browser
--    builder = mBuilder $ mGUI browser


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: WebView -> [String] -> IO (Maybe String)
goBackList webView dmenuOptions = do
    list           <- webViewGetBackForwardList webView
    n              <- webBackForwardListGetBackLength list
    backList       <- webBackForwardListGetBackListWithLimit list n
    dmenuList      <- mapM itemToEntry backList
    
    (>>= (return . head . words)) `fmap` (dmenu dmenuOptions $ (unlines . catMaybes) dmenuList)
    

-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: WebView -> [String] -> IO (Maybe String)
goForwardList webView dmenuOptions = do
    list        <- webViewGetBackForwardList webView
    n           <- webBackForwardListGetForwardLength list
    forwardList <- webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- mapM itemToEntry forwardList
    
    (>>= (return . head . words)) `fmap` (dmenu dmenuOptions $ (unlines . catMaybes) dmenuList)


itemToEntry :: WebHistoryItem -> IO (Maybe String)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u ++ " | " ++ (maybe "Untitled" id title))
        _      -> return Nothing


-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: WebView -> IO ()
toggleSourceMode webView = do
    currentMode <- webViewGetViewSourceMode webView
    webViewSetViewSourceMode webView (not currentMode)
    webViewReload webView
