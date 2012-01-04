module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Util
import Hbro.Types

import Control.Monad
import Control.Monad.Reader

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as Map

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.WebView

import System.Console.CmdArgs (whenNormal, whenLoud)
import System.FilePath
import System.ZMQ 
-- }}}
    
-- | Create a response socket to listen for commands.
-- Loops on listenToSocket until "Quit" command is received.
openRepSocket :: Context -> String -> (Socket Rep -> IO ()) -> IO ()
openRepSocket context socketURI listen = do    
    whenNormal $ putStrLn ("Opening socket at " ++ socketURI)
    withSocket context Rep $ \repSocket -> do
        bind repSocket socketURI
        listen repSocket

-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
listenToCommands :: Environment -> CommandsMap -> Socket Rep -> IO ()
listenToCommands environment commands repSocket = do
    message      <- receive repSocket []
    let message' =  unpack message

    case words message' of
    -- Empty command
        [] -> send repSocket (pack "ERROR Unknown command") []
    -- Exit command
        ["QUIT"] -> do
            whenLoud $ putStrLn "Receiving QUIT command"
            send repSocket (pack "OK") []
    -- Valid command
        command:arguments -> do
            whenLoud $ putStrLn ("Receiving command: " ++ message')
            case Map.lookup command commands of
                Just callback -> callback arguments repSocket environment
                _             -> send repSocket (pack "ERROR Unknown command") []

            listenToCommands environment commands repSocket
        
-- | Close the response socket by sending it the command "QUIT".
-- Typically called when exiting application.            
closeSocket :: Context -> String -> IO ()
closeSocket context socketURI = void $ sendCommand context socketURI "QUIT"
        
-- | Return the socket path to use for the given browser's process ID.
socketFile :: String -> String -> String
socketFile pid socketDir = "ipc://" ++ socketDir </> "hbro." ++ pid
  
-- | Send a single command (through a Request socket) to the given Response socket,
-- and return the answer.
sendCommand :: Context -> String -> String -> IO String
sendCommand context socketURI command = withSocket context Req $ \reqSocket -> do
    connect reqSocket socketURI
    send reqSocket (pack command) []
    receive reqSocket [] >>= return . unpack
        
-- | Same as 'sendCommand', but for all running instances of the browser.
sendCommandToAll :: Context -> FilePath -> String -> IO [String]
sendCommandToAll context socketDir command = getAllProcessIDs >>= mapM (\pid -> sendCommand context (socketFile pid socketDir) command)

-- | List of default supported requests.
defaultCommandsList :: CommandsList
defaultCommandsList = [
    -- Get information
    ("GET_URI", \_arguments repSocket browser -> liftIO $ do
        getUri <- postGUISync $ webViewGetUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No URL opened") [] ),

    ("GET_TITLE", \_arguments repSocket browser -> liftIO $ do
        getTitle <- postGUISync $ webViewGetTitle (mWebView $ mGUI browser)
        case getTitle of
            Just title -> send repSocket (pack title) []
            _          -> send repSocket (pack "ERROR No title") [] ),

    ("GET_FAVICON_URI", \_arguments repSocket browser -> liftIO $ do
        getUri <- postGUISync $ webViewGetIconUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket (pack uri) []
            _        -> send repSocket (pack "ERROR No favicon uri") [] ),

    ("GET_LOAD_PROGRESS", \_arguments repSocket browser -> liftIO $ do
        progress <- postGUISync $ webViewGetProgress (mWebView $ mGUI browser)
        send repSocket (pack (show progress)) [] ),


    -- Trigger actions
    ("LOAD_URI", \arguments repSocket browser -> liftIO $ case arguments of 
        uri:_ -> do
            postGUIAsync $ webViewLoadUri (mWebView $ mGUI browser) uri
            send repSocket (pack "OK") []
        _     -> send repSocket (pack "ERROR: argument needed.") [] ),

    ("STOP_LOADING", \_arguments repSocket browser -> liftIO $do
        postGUIAsync $ webViewStopLoading (mWebView $ mGUI browser) 
        send repSocket (pack "OK") [] ),

    ("RELOAD", \_arguments repSocket browser -> liftIO $ do
        postGUIAsync $ webViewReload (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("GO_BACK", \_arguments repSocket browser -> liftIO $ do
        postGUIAsync $ webViewGoBack (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("GO_FORWARD", \_arguments repSocket browser -> liftIO $ do
        postGUIAsync $ webViewGoForward (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("ZOOM_IN", \_arguments repSocket browser -> liftIO $ do
        postGUIAsync $ webViewZoomIn (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] ),

    ("ZOOM_OUT", \_arguments repSocket browser -> liftIO $ do
        postGUIAsync $ webViewZoomOut (mWebView $ mGUI browser)
        send repSocket (pack "OK") [] )
    ]

