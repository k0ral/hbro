module Hbro.Config (
-- * Default configuration
    defaultConfig,
    defaultHooks,
    defaultKeyHandler,
    defaultKeyBindings,
    defaultLinkClickedHook,
    defaultMIMEDisposition,
--    defaultNewWindowHook,
    defaultTitleChangedHook,
    defaultCommandsList
) where

-- {{{ Import
import Hbro.Core
--import Hbro.Keys
import Hbro.Gui
import qualified Hbro.Prompt as Prompt
import Hbro.Types
import Hbro.Util

import Control.Monad.Reader hiding(mapM_)

import Data.Foldable
import Data.Functor
import qualified Data.Map as M

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import Prelude hiding(mapM_)

import Network.URI

import System.Console.CmdArgs (whenLoud)
import System.FilePath
import System.Glib.Attributes
-- }}}


-- | Default configuration.
-- Homepage: DuckDuckGo, socket directory: system's temporary directory,
-- UI file: ~/.config/hbro/, Webkit's default websettings, no key/command binding.
defaultConfig :: Config
defaultConfig = Config {
    mHomePage          = "https://duckduckgo.com/",
    mSocketDir         = mTemporary,
    mUIFile            = (</> "ui.xml") . mConfiguration,
    mWebSettings       = [],
    mCommands          = defaultCommandsList,
    mHooks             = defaultHooks
}

-- | Pack of default hooks
defaultHooks :: Hooks
defaultHooks = Hooks {
    mBackForward     = (\_ decision -> io $ webPolicyDecisionUse decision),
    mDownload        = (\_ _ _ -> return ()),
    mFormResubmitted = (\_ decision -> io $ webPolicyDecisionUse decision),
    mFormSubmitted   = (\_ decision -> io $ webPolicyDecisionUse decision),
    mKeyPressed      = void . (defaultKeyHandler defaultKeyBindings),
    mLinkClicked     = defaultLinkClickedHook,
    mLoadFinished    = return (),
    mMIMEDisposition = defaultMIMEDisposition,
    mNewWindow       = const $ return (), --defaultNewWindowHook,
    mOtherNavigation = (\_ decision -> io $ webPolicyDecisionUse decision),
    mReload          = (\_ decision -> io $ webPolicyDecisionUse decision),
    mStartUp         = return (),
    mTitleChanged    = defaultTitleChangedHook
}

-- | Look for a callback associated to the given keystrokes and trigger it, if any.
defaultKeyHandler :: KeysList -> String -> K (String, Bool)
defaultKeyHandler keysList keystrokes = do
    io . whenLoud . putStrLn . ("Key pressed: " ++) $ keystrokes                                    
    case M.lookup keystrokes (M.fromList keysList) of
        Just callback -> (io . whenLoud . putStrLn $ " (mapped)") >> callback >> return (keystrokes, True) 
        _ -> (io . whenLoud . putStrLn $ " (unmapped)") >> return (keystrokes, False)

-- | Default key bindings.
defaultKeyBindings :: KeysList
defaultKeyBindings = [
-- Browse
    ("M-<Left>",      goBack),
    ("M-<Right>",     goForward),
    ("C-<Escape>",    stopLoading),
    ("<F5>",          reload),
    ("C-r",           reload),
    ("C-<F5>",        reloadBypassCache),
    ("C-R",           reloadBypassCache),
    ("C-^",           scroll Horizontal (Absolute 0)),
    ("C-$",           scroll Horizontal (Absolute 100)),
    ("C-<Home>",      scroll Vertical   (Absolute 0)),
    ("C-<End>",       scroll Vertical   (Absolute 100)),
    ("M-<Home>",      goHome),
-- Display
    ("C-+",           zoomIn),
    ("C--",           zoomOut),
    -- ("<F11>",         with (mWindow . mGUI) windowFullscreen),
    -- ("<Escape>",      with (mWindow . mGUI) windowUnfullscreen),
    ("C-b",           with (mStatusBar . mGUI) toggleVisibility),
    ("C-u",           toggleSourceMode),
-- Prompt
    ("C-o",           Prompt.readURI "Open URI" [] loadURI),
    ("M-o",           withURI $ \uri -> Prompt.readURI "Open URI " (show uri) loadURI),
-- Search
    ("/",             Prompt.iread "Search " [] $ searchText False True True >=> const (return ())),
    ("C-f",           Prompt.iread "Search " [] $ searchText False True True >=> const (return ())),
    ("?",             Prompt.iread "Search " [] $ searchText False False True >=> const (return ())),
    ("C-n",           withK (mEntry . mPromptBar . mGUI) $ (io . entryGetText) >=> searchText False True True >=> const (return ())),
    ("C-N",           withK (mEntry . mPromptBar . mGUI) $ (io . entryGetText) >=> searchText False False True >=> const (return ())),
-- Misc
    ("<Escape>",      with (mBox . mPromptBar . mGUI) widgetHide), -- DUPE !
    ("C-i",           showWebInspector),
    ("C-p",           printPage),
    ("C-t",           io $ spawn "hbro" []),
    ("C-w",           io mainQuit)
    ]

-- | Left click loads the new page in current window, middle click loads the new page in a new window, right click does nothing.
defaultLinkClickedHook :: Button -> URI -> WebPolicyDecision -> K ()
defaultLinkClickedHook ButtonL _uri decision = io $ webPolicyDecisionUse    decision
defaultLinkClickedHook ButtonM  uri decision = io $ webPolicyDecisionIgnore decision >> spawn "hbro" ["-u", show uri]
defaultLinkClickedHook _       _uri decision = io $ webPolicyDecisionIgnore decision

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
defaultMIMEDisposition :: URI -> String -> WebPolicyDecision -> K ()
defaultMIMEDisposition _uri mimetype decision = with (mWebView . mGUI) $ \view -> do
    canShow <- webViewCanShowMimeType view mimetype

    case canShow of
        True -> webPolicyDecisionUse      decision
        _    -> webPolicyDecisionDownload decision

--- | Default behavior when a new window is requested: load URI in current window.
--defaultNewWindowHook :: URI -> K ()
--defaultNewWindowHook uri = loadURI uri

-- | Update the main window's title
defaultTitleChangedHook :: String -> K ()            
defaultTitleChangedHook title = with (mWindow . mGUI) (`set` [ windowTitle := ("hbro | " ++ title)])

-- | List of default supported requests.
defaultCommandsList :: CommandsList
defaultCommandsList = [
    -- Get information
    ("GET_URI",           \_arguments -> (maybe "ERROR" show) <$> mapK postGUISync getURI),
    ("GET_TITLE",         \_arguments -> (maybe "ERROR" show) <$> mapK postGUISync getTitle),
    ("GET_FAVICON_URI",   \_arguments -> (maybe "ERROR" show) <$> mapK postGUISync getFaviconURI),
    ("GET_LOAD_PROGRESS", \_arguments -> show <$> mapK postGUISync getLoadProgress),
    -- Trigger actions
    ("LOAD_URI",          \arguments -> case arguments of 
        uri:_ -> ((mapK postGUIAsync) . (mapM_ loadURI)) (parseURIReference uri) >> return "OK"
        _     -> return "ERROR Argument needed."),
    ("STOP_LOADING",      \_arguments -> mapK postGUIAsync stopLoading >> return "OK"),
    ("RELOAD",            \_arguments -> mapK postGUIAsync reload >> return "OK"),
    ("GO_BACK",           \_arguments -> mapK postGUIAsync goBack >> return "OK"),
    ("GO_FORWARD",        \_arguments -> mapK postGUIAsync goForward >> return "OK"),
    ("ZOOM_IN",           \_arguments -> mapK postGUIAsync zoomIn >> return "OK"),
    ("ZOOM_OUT",          \_arguments -> mapK postGUIAsync zoomOut >> return "OK")]
