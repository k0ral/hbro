module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Types

import Control.Monad
import Data.ByteString.Char8 (pack, unpack)
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView
import System.ZMQ 
-- }}}
    
listenToSocket :: Socket Rep -> Browser -> IO a
listenToSocket repSocket browser = forever $ do
    command <- receive repSocket []

    case unpack command of
        -- Get information
        "getUri" -> do
            getUri <- postGUISync $ webViewGetUri (mWebView $ mGUI browser)
            case getUri of
                Just uri -> send repSocket (pack uri) []
                _        -> send repSocket (pack "ERROR No URL opened") []
        "getTitle" -> do
            getTitle <- postGUISync $ webViewGetTitle (mWebView $ mGUI browser)
            case getTitle of
                Just title -> send repSocket (pack title) []
                _          -> send repSocket (pack "ERROR No title") []
        "getFaviconUri" -> do
            getUri <- postGUISync $ webViewGetIconUri (mWebView $ mGUI browser)
            case getUri of
                Just uri -> send repSocket (pack uri) []
                _        -> send repSocket (pack "ERROR No favicon uri") []
        "getLoadProgress" -> do
            progress <- postGUISync $ webViewGetProgress (mWebView $ mGUI browser)
            send repSocket (pack (show progress)) []

        -- Trigger actions
        ('l':'o':'a':'d':'U':'r':'i':' ':uri) -> do
            postGUIAsync $ webViewLoadUri (mWebView $ mGUI browser) uri
            send repSocket (pack "OK") []
        "stopLoading" -> do
            postGUIAsync $ webViewStopLoading (mWebView $ mGUI browser) 
            send repSocket (pack "OK") []
        "reload" -> do
            postGUIAsync $ webViewReload (mWebView $ mGUI browser)
            send repSocket (pack "OK") []
        "goBack" -> do
            postGUIAsync $ webViewGoBack (mWebView $ mGUI browser)
            send repSocket (pack "OK") []
        "goForward" -> do
            postGUIAsync $ webViewGoForward (mWebView $ mGUI browser)
            send repSocket (pack "OK") []
        "zoomIn" -> do
            postGUIAsync $ webViewZoomIn (mWebView $ mGUI browser)
            send repSocket (pack "OK") []
        "zoomOut" -> do
            postGUIAsync $ webViewZoomOut (mWebView $ mGUI browser)
            send repSocket (pack "OK") []

        _ -> send repSocket (pack "ERROR Unknown command") []

