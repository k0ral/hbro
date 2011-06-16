module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Gui

import Control.Monad
import Data.ByteString.Char8 (pack, unpack)
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView
import System.ZMQ 
-- }}}
    
createReplySocket :: String -> GUI -> IO a
createReplySocket socketName gui = withContext 1 $ \context -> do  
    withSocket context Rep $ \socket -> do
        bind socket socketName
        forever $ do
            command <- receive socket []
            case unpack command of
                -- Get information
                "getUri" -> do
                    getUri <- postGUISync $ webViewGetUri (mWebView gui)
                    case getUri of
                        Just uri -> send socket (pack uri) []
                        _        -> send socket (pack "ERROR No URL opened") []
                "getTitle" -> do
                    getTitle <- postGUISync $ webViewGetTitle (mWebView gui)
                    case getTitle of
                        Just title -> send socket (pack title) []
                        _          -> send socket (pack "ERROR No title") []
                "getFaviconUri" -> do
                    getUri <- postGUISync $ webViewGetIconUri (mWebView gui)
                    case getUri of
                        Just uri -> send socket (pack uri) []
                        _        -> send socket (pack "ERROR No favicon uri") []
                "getLoadProgress" -> do
                    progress <- postGUISync $ webViewGetProgress (mWebView gui)
                    send socket (pack (show progress)) []

                -- Trigger actions
                ('l':'o':'a':'d':'U':'r':'i':' ':uri) -> do
                    postGUIAsync $ webViewLoadUri (mWebView gui) uri
                    send socket (pack "OK") []
                "stopLoading" -> do
                    postGUIAsync $ webViewStopLoading (mWebView gui) 
                    send socket (pack "OK") []
                "reload" -> do
                    postGUIAsync $ webViewReload (mWebView gui)
                    send socket (pack "OK") []
                "goBack" -> do
                    postGUIAsync $ webViewGoBack (mWebView gui)
                    send socket (pack "OK") []
                "goForward" -> do
                    postGUIAsync $ webViewGoForward (mWebView gui)
                    send socket (pack "OK") []
                "zoomIn" -> do
                    postGUIAsync $ webViewZoomIn (mWebView gui)
                    send socket (pack "OK") []
                "zoomOut" -> do
                    postGUIAsync $ webViewZoomOut (mWebView gui)
                    send socket (pack "OK") []

                _ -> send socket (pack "ERROR Unknown command") []

