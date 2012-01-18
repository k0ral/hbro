module Hbro.Config (
-- * Default configuration
    defaultConfig,
    defaultHooks,
    defaultNewWindowHook,
    defaultCommandsList
) where

-- {{{ Import
import Hbro.Keys
import Hbro.Types
import Hbro.Util

import Control.Monad.Reader hiding(mapM_)

import Data.ByteString.Char8 (pack)
import Data.Foldable

import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewGetUri, webViewLoadUri)

import Prelude hiding(mapM_)

import Network.URI

import System.FilePath
import System.ZMQ 
-- }}}


-- | Default configuration.
-- Homepage: Google, socket directory: /tmp,
-- UI file: ~/.config/hbro/, no key/command binding.
defaultConfig :: CommonDirectories -> Config
defaultConfig directories = Config {
    mCommonDirectories = directories,
    mHomePage          = "https://encrypted.google.com/",
    mSocketDir         = mTemporary directories,
    mUIFile            = (mConfiguration directories) </> "ui.xml",
    mKeyEventHandler   = simpleKeyEventHandler,
    mKeyEventCallback  = \_ -> simpleKeyEventCallback (keysListToMap []),
    mWebSettings       = [],
    mSetup             = const (return () :: IO ()),
    mCommands          = defaultCommandsList,
    mHooks             = defaultHooks,
    mMIMEDisposition   = defaultMIMEDisposition,
    mError             = Nothing
}

-- | Display content if webview can show the given MIME type, otherwise download it.
-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
defaultMIMEDisposition :: Environment -> NetworkRequest -> String -> WebPolicyDecision -> IO ()
defaultMIMEDisposition env _request mimetype policyDecision = do
    canShow <- webViewCanShowMimeType ((mWebView . mGUI) env) mimetype

    case (canShow, mimetype) of
        (True, _) -> webPolicyDecisionUse      policyDecision
        _         -> webPolicyDecisionDownload policyDecision

-- | Pack of default hooks
defaultHooks :: Hooks
defaultHooks = Hooks (\_ _ _ _ -> return ()) defaultNewWindowHook

-- | Default behavior when a new window is requested: load URI in current window.
defaultNewWindowHook :: Environment -> URI -> IO WebView
defaultNewWindowHook env uri = webViewLoadUri webView uri >> return webView
  where
    webView = (mWebView . mGUI) env

-- | List of default supported requests.
defaultCommandsList :: CommandsList
defaultCommandsList = [
    -- Get information
    ("GET_URI", \_arguments repSocket browser -> liftIO $ do
        getUri <- postGUISync $ webViewGetUri (mWebView $ mGUI browser)
        case getUri of
            Just uri -> send repSocket ((pack . show) uri) []
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
            postGUIAsync $ mapM_ (webViewLoadUri (mWebView (mGUI browser))) (parseURIReference uri)
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