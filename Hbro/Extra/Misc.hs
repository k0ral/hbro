module Hbro.Extra.Misc where

-- {{{ Imports
import Hbro.Core hiding(goBack, goForward)
import Hbro.Types
import Hbro.Util

import Data.Maybe

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebBackForwardList
import Graphics.UI.Gtk.WebKit.WebHistoryItem
import Graphics.UI.Gtk.WebKit.WebView

import System.IO
import System.Process 
-- }}}


-- | Same as goBack function from Hbro.Core,
-- but with feedback in case of failure.
goForward :: Browser -> IO ()
goForward browser = do
    result        <- webViewCanGoForward webView
    feedbackLabel <- builderGetObject builder castToLabel "feedback"

    case result of
        True -> webViewGoForward webView
        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go forward !</span>" 5000 >> return ()

  where
    webView = mWebView $ mGUI browser
    builder = mBuilder $ mGUI browser

-- | Same as goBack function from Hbro.Core,
-- but with feedback in case of failure.
goBack :: Browser -> IO ()
goBack browser = do
    result        <- webViewCanGoBack webView
    feedbackLabel <- builderGetObject builder castToLabel "feedback"

    case result of
        True -> webViewGoBack webView
        _    -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to go back !</span>" 5000 >> return ()

  where
    webView = mWebView $ mGUI browser
    builder = mBuilder $ mGUI browser


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: Browser -> IO ()
goBackList browser = do
    list           <- webViewGetBackForwardList webView
    n              <- webBackForwardListGetBackLength list
    backList       <- webBackForwardListGetBackListWithLimit list n
    dmenuList      <- mapM itemToEntry backList

    (Just input, Just output, _, _) <- createProcess (proc "dmenu" ["-l", "10"]) {
        std_in = CreatePipe,
        std_out = CreatePipe }

    _     <- hPutStr input $ unlines (catMaybes dmenuList)
    entry <- catch (hGetLine output) (\_error -> return "ERROR" )

    case words entry of
        ["ERROR"]   -> return ()
        uri:_       -> loadURI uri browser
        _           -> return ()
    return ()
  where
    webView = mWebView $ mGUI browser
    

-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: Browser -> IO ()
goForwardList browser = do
    list        <- webViewGetBackForwardList webView
    n           <- webBackForwardListGetForwardLength list
    forwardList <- webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- mapM itemToEntry forwardList

    (Just input, Just output, _, _) <- createProcess (proc "dmenu" ["-l", "10"]) {
        std_in = CreatePipe,
        std_out = CreatePipe }

    _     <- hPutStr input $ unlines (catMaybes dmenuList)
    entry <- catch (hGetLine output) (\_error -> return "ERROR" )

    case words entry of
        ["ERROR"]   -> return ()
        uri:_       -> loadURI uri browser
        _           -> return ()
    return ()
  where
    webView = mWebView $ mGUI browser


itemToEntry :: WebHistoryItem -> IO (Maybe String)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u ++ " | " ++ (maybe "Untitled" id title))
        _      -> return Nothing


-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: Browser -> IO ()
toggleSourceMode browser = do
    currentMode <- webViewGetViewSourceMode (mWebView $ mGUI browser)
    webViewSetViewSourceMode (mWebView $ mGUI browser) (not currentMode)
    reload True browser
