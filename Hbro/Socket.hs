module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Types

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as Map

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView

import System.Console.CmdArgs (whenNormal, whenLoud)
import System.ZMQ 
-- }}}
    
-- | Create a response socket to listen for commands.
-- Loops on listenToSocket until "Quit" command is received.
createRepSocket :: Context -> String -> Browser -> IO ()
createRepSocket context socketURI browser = do
    whenNormal $ putStrLn ("Listening socket at " ++ socketURI)
    withSocket context Rep $ \repSocket -> do
        bind repSocket socketURI
        listenToSocket repSocket commandsList browser
  where
    commandsList = Map.fromList (defaultCommandsList ++ (mCommands $ mConfiguration browser))


-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
listenToSocket :: Socket Rep -> CommandsMap -> Browser -> IO ()
listenToSocket repSocket commands browser = do
    message      <- receive repSocket []
    let message' =  unpack message

    case words message' of
    -- Empty command
        [] -> send repSocket (pack "ERROR Unknown command") []
    -- Exit command
        ["Quit"] -> send repSocket (pack "OK") []
    -- Valid command
        command:arguments -> do
            whenLoud $ putStrLn ("Receiving command: " ++ message')
            case Map.lookup command commands of
                Just callback -> callback arguments repSocket browser
                _             -> send repSocket (pack "ERROR Unknown command") []

            listenToSocket repSocket commands browser

-- | List of default supported requests.
defaultCommandsList :: CommandsList
defaultCommandsList = [
    -- Get information
    ("getUri", \_arguments repSocket browser -> do
        getUri <- postGUISync $ webViewGetUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No URL opened") [] ),

    ("getTitle", \_arguments repSocket browser -> do
        getTitle <- postGUISync $ webViewGetTitle (mWebView $ mGUI browser)
        case getTitle of
            Just title -> send repSocket (pack title) []
            _          -> send repSocket (pack "ERROR No title") [] ),

    ("getFaviconUri", \_arguments repSocket browser -> do
        getUri <- postGUISync $ webViewGetIconUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No favicon uri") [] ),

    ("getLoadProgress", \_arguments repSocket browser -> do
        progress <- postGUISync $ webViewGetProgress (mWebView $ mGUI browser)
        send repSocket (pack (show progress)) [] ),


    -- Trigger actions
    ("loadUri", \arguments repSocket browser -> case arguments of 
        uri:_ -> do
            postGUIAsync $ webViewLoadUri (mWebView $ mGUI browser) uri
            send repSocket (pack "OK") []
        _     -> send repSocket (pack "ERROR: argument needed.") [] ),

    ("stopLoading", \_arguments repSocket browser -> do
        postGUIAsync $ webViewStopLoading (mWebView $ mGUI browser) 
        send repSocket (pack "OK") [] ),

    ("reload", \_arguments repSocket browser -> do
        postGUIAsync $ webViewReload (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("goBack", \_arguments repSocket browser -> do
        postGUIAsync $ webViewGoBack (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("goForward", \_arguments repSocket browser -> do
        postGUIAsync $ webViewGoForward (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("zoomIn", \_arguments repSocket browser -> do
        postGUIAsync $ webViewZoomIn (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("zoomOut", \_arguments repSocket browser -> do
        postGUIAsync $ webViewZoomOut (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] )
    ]

