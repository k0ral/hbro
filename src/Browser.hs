module Browser where

-- {{{ Imports
import Gui
import Util

import qualified Config.Dyre as Dyre

import Control.Monad.Trans(liftIO)
import Data.Map
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebSettings
import Network.URL
import Prelude hiding (lookup)
import System.Environment
-- }}}

-- {{{ Type definitions
data Browser = Browser {
    mGUI            :: GUI
}

data Configuration = Configuration {
    mHomePage       :: String,
    mKeyBindings    :: [(([Modifier], String), GUI -> IO ())],
    mWebSettings    :: IO (WebSettings),
    mCustomizations :: GUI -> IO (),
    mError          :: Maybe String
}

instance Ord Modifier where
    m <= m' =  fromEnum m <= fromEnum m'
-- }}}

-- {{{ Entry point
realMain :: Configuration -> IO ()
realMain configuration = do
    args <- getArgs

    case args of
        ["--help"]  -> putStrLn "Usage: browser [url]"
        _           -> initBrowser configuration
-- }}}

-- {{{ Main function
initBrowser :: Configuration -> IO ()
initBrowser configuration = do
    -- Initialize GUI
    args <- initGUI
    gui  <- loadGUI ""

    -- Load configuration
    settings <- mWebSettings configuration
    webViewSetWebSettings (mWebView gui) settings
    (mCustomizations configuration) gui

    -- Load url
    let url = case args of
                [arg] -> arg
                _     -> mHomePage configuration

    webViewLoadUri (mWebView gui) url

    -- Load key bindings
    let keyBindings = fromList (mKeyBindings configuration)

    -- Open all link in current window.
    _ <- on (mWebView gui) createWebView $ \frame -> do
        newUri <- webFrameGetUri frame
        case newUri of
            Just uri -> webViewLoadUri (mWebView gui) uri
            Nothing  -> return ()
        return (mWebView gui)

    -- Key bindings
    _ <- after (mWebView gui) keyPressEvent $ do
        keyVal      <- eventKeyVal
        modifiers   <- eventModifier

        let keyString = keyToString keyVal

        case keyString of 
            Just string -> do 
                case lookup (modifiers, string) keyBindings of
                    Just callback   -> liftIO $ callback gui
                    _               -> liftIO $ putStrLn "No callback associated"
                liftIO $ putStrLn string
            _ -> return ()

        return False

    -- Connect and show.
    _ <- onDestroy (mWindow gui) mainQuit
    widgetShowAll (mWindow gui)
    widgetHide (mPromptLabel gui)
    widgetHide (mPrompt gui)

    mainGUI
-- }}}

showWebInspector :: GUI -> IO ()
showWebInspector gui = do
    inspector <- webViewGetInspector (mWebView gui)
    webInspectorShow inspector

loadURL :: String -> GUI -> IO ()
loadURL url gui =
    case importURL url of
        Just url -> loadURL' url gui
        _        -> return ()

loadURL' :: URL -> GUI -> IO ()
loadURL' url@URL {url_type = Absolute _} gui =
    webViewLoadUri (mWebView gui) (exportURL url)
loadURL' url@URL {url_type = HostRelative} gui = 
    webViewLoadUri (mWebView gui) ("file://" ++ (exportURL url)) >> putStrLn (show url)
loadURL' url@URL {url_type = _} gui = 
    webViewLoadUri (mWebView gui) ("http://" ++ (exportURL url)) >> putStrLn (show url)

-- {{{ Dyre
showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }

browser :: Configuration -> IO ()
browser = Dyre.wrapMain Dyre.defaultParams {
    Dyre.projectName  = "browser",
    Dyre.showError    = showError,
    Dyre.realMain     = realMain,
    Dyre.ghcOpts      = ["-i /path/to/src", "-O2"]
}
-- }}}
