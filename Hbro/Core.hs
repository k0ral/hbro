{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-} 
module Hbro.Core where

-- {{{ Imports
import Hbro.Gui
import Hbro.Socket
import Hbro.Util

import qualified Config.Dyre as Dyre
import Control.Concurrent
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.Container
--import Graphics.UI.Gtk.Abstract.IMContext
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
--import Graphics.UI.Gtk.WebKit.Download
--import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings

import Network.URL
import Prelude

import System.Console.CmdArgs
--import System.Glib.Attributes
import System.Glib.Signals
import System.Posix.Process
-- }}}

-- {{{ Type definitions
data Browser = Browser {
    mOptions        :: CliOptions,
    mGUI            :: GUI
}

data Configuration = Configuration {
    mHomePage       :: String,          -- ^ Startup page 
    mSocketDir      :: String,          -- ^ Path to socket directory (/tmp for example)
    mKeyBindings    :: KeyBindingsList, -- ^ List of keybindings
    mWebSettings    :: IO WebSettings,  -- ^ Web settings
    mAtStartUp      :: GUI -> IO (),    -- ^ Custom startup instructions
    mError          :: Maybe String     -- ^ Error
}

type KeyBindingsList = [(([Modifier], String), (GUI -> IO ()))]
-- }}}

-- {{{ Commandline options
data CliOptions = CliOptions {
    mURI :: Maybe String
} deriving (Data, Typeable, Show, Eq)

cliOptions :: CliOptions
cliOptions = CliOptions{
    mURI = def &= help "URI to open at startup" &= explicit &= name "u" &= name "uri" &= typ "URI"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "A suckless minimal KISSy browser."
    &= helpArg [explicit, name "help", name "h"]
    &= program "hbro"
-- }}}

-- {{{ Entry point
-- | Entry point of the application.
-- | Parse arguments and step down in favour of initBrowser.
realMain :: Configuration -> IO ()
realMain configuration = do
    options  <- getOptions

    initBrowser configuration options
-- }}}

-- {{{ Main function
-- | Application's main function.
-- Create browser and load homepage.
initBrowser :: Configuration -> CliOptions -> IO ()
initBrowser configuration options = do
    -- Print configuration error, if any
    case (mError configuration) of
        Just e -> putStrLn e
        _      -> return ()

    -- Initialize browser
    _    <- initGUI
    gui  <- loadGUI ""
    let browser = Browser options gui

    -- Initialize IPC socket
    pid <- getProcessID
    _ <- forkIO $ createReplySocket ("ipc://" ++ (mSocketDir configuration) ++ "/hbro." ++ show pid) gui

    -- Load configuration
    settings <- mWebSettings configuration
    webViewSetWebSettings (mWebView gui) settings

    -- Launch custom startup
    (mAtStartUp configuration) gui

    -- Load url
    let url = case (mURI options) of
                Just x -> x
                _      -> mHomePage configuration

    loadURL url gui

    -- Load key bindings
    let keyBindings = importKeyBindings (mKeyBindings configuration)

    
    -- On new window request
    --newWindowWebView <- webViewNew
    _ <- on (mWebView gui) createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> webViewLoadUri (mWebView gui) uri
            --Just uri -> runExternalCommand $ "hbro " ++ uri
            Nothing  -> return ()
        return (mWebView gui)
--         return newWindowWebView

--     _ <- on newWindowWebView loadCommitted $ \frame -> do
--         getUri <- (webViewGetUri newWindowWebView)
--         case getUri of 
--             Just uri -> runExternalCommand $ "hbro \"" ++ uri ++ "\""
--             _        -> return ()


    -- Web inspector
    inspector <- webViewGetInspector (mWebView gui)
    _ <- on inspector inspectWebView $ \_ -> do
        webView <- webViewNew
        containerAdd (mInspectorWindow gui) webView
        return webView
    
    _ <- on inspector showWindow $ do
        widgetShowAll (mInspectorWindow gui)
        return True

    -- TODO: when does this signal happen ?!
    --_ <- on inspector finished $ return ()

    _ <- on inspector attachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        case getWebView of
            Just webView -> do widgetHide (mInspectorWindow gui)
                               containerRemove (mInspectorWindow gui) webView
                               widgetSetSizeRequest webView (-1) 250
                               boxPackEnd (mWindowBox gui) webView PackNatural 0
                               widgetShow webView
                               return True
            _            -> return False

    _ <- on inspector detachWindow $ do
        getWebView <- webInspectorGetWebView inspector
        _ <- case getWebView of
            Just webView -> do containerRemove (mWindowBox gui) webView
                               containerAdd (mInspectorWindow gui) webView
                               widgetShowAll (mInspectorWindow gui)
                               return True
            _            -> return False
        
        widgetShowAll (mInspectorWindow gui)
        return True

    -- Key bindings
--     imContext <- get (mWebView gui) webViewImContext
--     _ <- on (mWebView gui) keyPressEvent $ do
--         value      <- eventKeyVal
--         modifiers  <- eventModifier

--         let keyString = keyToString value
--         
--         case keyString of
--             Just "<Escape>" -> do
--                 liftIO $ imContextFocusIn imContext
--                 return True
--             _               -> return False

    _ <- after (mWebView gui) keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        case keyString of 
            Just string -> do 
                case Map.lookup (Set.fromList modifiers, string) keyBindings of
                    Just callback   -> do
                        liftIO $ callback gui
                        liftIO $ labelSetMarkup (mKeysLabel gui) $ "<span foreground=\"green\">" ++ show modifiers ++ string ++ "</span>"
                    _               -> liftIO $ putStrLn string 
            _ -> return ()

        return False

--     imContextFilterKeypress imContext $ do
--         value      <- eventKeyVal
--         modifiers  <- eventModifier

--         let keyString = keyToString value
--         putStrLn keyString

-- --         case keyString of 
-- --             Just string -> do 
-- --                 case Map.lookup (Set.fromList modifiers, string) keyBindings of
-- --                     Just callback   -> liftIO $ callback gui
-- --                     _               -> liftIO $ putStrLn string 
-- --             _ -> return ()

--         return False

    -- Connect and show.
    _ <- onDestroy (mWindow gui) mainQuit
    widgetShowAll (mWindow gui)
    widgetHide (mPromptLabel gui)
    widgetHide (mPrompt gui)

    mainGUI
-- }}}

-- | Show web inspector for current webpage.
showWebInspector :: GUI -> IO ()
showWebInspector gui = do
    inspector <- webViewGetInspector (mWebView gui)
    webInspectorInspectCoordinates inspector 0 0


-- | Load given URL in the browser.
loadURL :: String -> GUI -> IO ()
loadURL url gui =
    case importURL url of
        Just url' -> loadURL' url' gui
        _         -> return ()


-- | Backend function for loadURL.
loadURL' :: URL -> GUI -> IO ()
loadURL' url@URL {url_type = Absolute _} gui =
    webViewLoadUri (mWebView gui) (exportURL url)
loadURL' url@URL {url_type = HostRelative} gui = 
    webViewLoadUri (mWebView gui) ("file://" ++ exportURL url) >> putStrLn (show url)
loadURL' url@URL {url_type = _} gui = 
    webViewLoadUri (mWebView gui) ("http://" ++ exportURL url) >> print url

-- {{{ Dyre
showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }

hbro :: Configuration -> IO ()
hbro = Dyre.wrapMain Dyre.defaultParams {
    Dyre.projectName  = "hbro",
    Dyre.showError    = showError,
    Dyre.realMain     = realMain,
    Dyre.ghcOpts      = ["-threaded"]
}
-- }}}
