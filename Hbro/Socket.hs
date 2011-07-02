module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Types

import Control.Monad

import Data.ByteString.Char8 (pack, unpack)
import Data.List.Split
import qualified Data.Map as Map

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView

import System.ZMQ 
-- }}}
    
-- | Create a response socket to listen for commands.
-- Loops on listenToSocket forever.
createRepSocket :: String -> Browser -> IO a
createRepSocket socketURI browser = withContext 1 $ \context -> do
    withSocket context Rep $ \repSocket -> do
        bind      repSocket socketURI
        setOption repSocket (Linger 0)

        _ <- quitAdd 0 $ do
            close repSocket
            return False

        let commandsList = Map.fromList (defaultCommandsList ++ (mCommands $ mConfiguration browser))
        
        forever $ listenToSocket repSocket commandsList browser

-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
listenToSocket :: Socket Rep -> CommandsMap -> Browser -> IO ()
listenToSocket repSocket commands browser = do
    message <- receive repSocket []
    let command:arguments = splitOn " " $ unpack message

    case Map.lookup command commands of
        Just callback -> callback arguments repSocket browser
        _             -> send repSocket (pack "ERROR Unknown command") []

-- | List of default supported requests.
defaultCommandsList :: CommandsList
defaultCommandsList = [
    -- Get information
    ("getUri", \arguments repSocket browser -> do
        getUri <- postGUISync $ webViewGetUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No URL opened") [] ),

    ("getTitle", \arguments repSocket browser -> do
        getTitle <- postGUISync $ webViewGetTitle (mWebView $ mGUI browser)
        case getTitle of
            Just title -> send repSocket (pack title) []
            _          -> send repSocket (pack "ERROR No title") [] ),

    ("getFaviconUri", \arguments repSocket browser -> do
        getUri <- postGUISync $ webViewGetIconUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No favicon uri") [] ),

    ("getLoadProgress", \arguments repSocket browser -> do
        progress <- postGUISync $ webViewGetProgress (mWebView $ mGUI browser)
        send repSocket (pack (show progress)) [] ),


    -- Trigger actions
    ("loadUri", \arguments repSocket browser -> case arguments of 
        uri:_ -> do
            postGUIAsync $ webViewLoadUri (mWebView $ mGUI browser) uri
            send repSocket (pack "OK") []
        _     -> send repSocket (pack "ERROR: argument needed.") [] ),

    ("stopLoading", \arguments repSocket browser -> do
        postGUIAsync $ webViewStopLoading (mWebView $ mGUI browser) 
        send repSocket (pack "OK") [] ),

    ("reload", \arguments repSocket browser -> do
        postGUIAsync $ webViewReload (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("goBack", \arguments repSocket browser -> do
        postGUIAsync $ webViewGoBack (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("goForward", \arguments repSocket browser -> do
        postGUIAsync $ webViewGoForward (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("zoomIn", \arguments repSocket browser -> do
        postGUIAsync $ webViewZoomIn (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("zoomOut", \arguments repSocket browser -> do
        postGUIAsync $ webViewZoomOut (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] )
    ]



