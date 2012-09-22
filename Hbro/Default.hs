{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Hbro.Default where

-- {{{ Import
import qualified Hbro.Clipboard as Clipboard
import Hbro.Core
import Hbro.Keys
import Hbro.Gtk.ScrolledWindow
import Hbro.Gui
import qualified Hbro.Prompt as Prompt
import Hbro.Types
import Hbro.Util
import Hbro.Webkit.WebView

import Control.Applicative
import Control.Conditional
import Control.Monad.Error  hiding(mapM_)
import Control.Monad.Reader hiding(mapM_)
-- import Control.Monad.State  hiding(mapM_)
-- import Control.Monad.Trans.Control

import Data.Default
-- import Data.Foldable
-- import Data.Functor

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import Prelude hiding(mapM_)

-- import Network.URI (URI)
import qualified Network.URI as N

import System.Directory
import System.Environment.XDG.BaseDir
import System.Glib.Attributes
-- }}}

-- | Default configuration.
-- Homepage: DuckDuckGo, socket directory: system's temporary directory,
-- UI file: ~/.config/hbro/, Webkit's default websettings, default key/command bindings.
instance Default Config where
    def = Config {
        __homePage          = maybe undefined id . N.parseURI $ "https://duckduckgo.com/",
        __socketDir         = getTemporaryDirectory,
        __UIFile            = getUserConfigDir "hbro" >/> "ui.xml",
        __commands          = def}

instance Default Setup where
  def = Setup $ do
    _ <- afterKeyPressed     $ emacsKeyHandler def []
    _ <- onNavigationRequest $ def
    _ <- onNewWebView        $ def
    _ <- onNewWindow         $ def
    _ <- onResourceOpened    $ def
    _ <- onTitleChanged      $ def
    return ()

instance Default NewWindowHook where
    def = NewWindowHook $ \_frame request _action decision -> do
        io $ webPolicyDecisionIgnore decision
        uri <- networkRequestGetUri request
        logVerbose $ "New window request: " ++ show uri
        spawn "hbro" ["-u", show uri]

        --either (\e -> io . putStrLn $ "WARNING: wrong URI given, unable to open new window.") (const $ return ()) result

instance Default NavigationHook where
    def = let f WebNavigationReasonLinkClicked (Just MiddleButton) uri decision = io $ webPolicyDecisionIgnore decision >> spawn "hbro" ["-u", show uri]
              f _ _ _ decision                                                  = io $ webPolicyDecisionUse decision
          in NavigationHook f

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
instance Default ResourceOpenedHook where
    def = ResourceOpenedHook $ \_uri mimetype decision -> do
        canShow <- io . (`webViewCanShowMimeType` mimetype) =<< asks _webview
        io $ (canShow ? webPolicyDecisionUse ?? webPolicyDecisionDownload) decision

instance Default TitleChangedHook where
    def = TitleChangedHook $ \title -> asks _mainWindow >>= io . (`set` [ windowTitle := ("hbro | " ++ title)])


-- | Default key bindings.
instance Default KeysList where
  def = KeysList [
    -- Browse
        ("M-<Left>",      goBack),
        ("M-<Right>",     goForward),
        ("C-<Escape>",    stopLoading),
        ("<F5>",          reload),
        ("C-r",           reload),
        ("C-<F5>",        reloadBypassCache),
        ("M-r",           reloadBypassCache),
        ("C-^",           scroll Horizontal (Absolute 0)),
        ("C-$",           scroll Horizontal (Absolute 100)),
        ("C-<Home>",      scroll Vertical   (Absolute 0)),
        ("C-<End>",       scroll Vertical   (Absolute 100)),
        ("M-<Home>",      goHome),
    -- Copy/paste
        ("C-c",           getURI   >>= Clipboard.insert . show >> notify 5000 "URI copied to clipboard"),
        ("M-c",           getTitle >>= Clipboard.insert >> notify 5000 "Page title copied to clipboard"),
        ("C-v",           Clipboard.with $ parseURIReference >=> loadURI),
        ("M-v",           Clipboard.with $ \uri -> spawn "hbro" ["-u", uri]),
    -- Display
        ("C-+",           zoomIn),
        ("C--",           zoomOut),
        -- ("<F11>",         with (_window . _UI) windowFullscreen),
        -- ("<Escape>",      with (_window . _UI) windowUnfullscreen),
        ("C-b",           toggleVisibility =<< asks _statusBar),
        ("C-u",           toggleSourceMode),
    -- Prompt
        ("C-o",           Prompt.readURI "Open URI" "" loadURI),
        ("M-o",           getURI >>= \uri -> Prompt.readURI "Open URI " (show uri) loadURI),
    -- Search
        ("/",             Prompt.iread "Search " "" $ searchText_ CaseInsensitive Forward Wrap),
        ("C-f",           Prompt.iread "Search " "" $ searchText_ CaseInsensitive Forward Wrap),
        ("?",             Prompt.iread "Search " "" $ searchText_ CaseInsensitive Backward Wrap),
        ("C-n",           void . searchText CaseInsensitive Forward  Wrap =<< io . entryGetText . _entry =<< asks _promptBar),
        ("C-N",           void . searchText CaseInsensitive Backward Wrap =<< io . entryGetText . _entry =<< asks _promptBar),
    -- Misc
        ("<Escape>",      io . widgetHide . _box =<< asks _promptBar),
        ("C-i",           showWebInspector),
        ("C-p",           printPage),
        ("C-t",           spawn "hbro" []),
        ("C-w",           quit)]


-- | List of default supported requests.
instance Default CommandsList where
    def = CommandsList [
    -- Get information
        ("GET_URI",           \_arguments -> show <$> getURI),
        ("GET_TITLE",         \_arguments -> show <$> getTitle),
        ("GET_FAVICON_URI",   \_arguments -> show <$> getFaviconURI),
        ("GET_LOAD_PROGRESS", \_arguments -> show <$> getLoadProgress),
    -- Trigger actions
        ("LOAD_URI",          \arguments -> case arguments of
            uri:_ -> parseURIReference uri >>= loadURI >> return "OK"
            _     -> return "ERROR Argument needed."),
        ("STOP_LOADING",      \_arguments -> stopLoading >> return "OK"),
        ("RELOAD",            \_arguments -> reload      >> return "OK"),
        ("GO_BACK",           \_arguments -> goBack      >> return "OK"),
        ("GO_FORWARD",        \_arguments -> goForward   >> return "OK"),
        ("ZOOM_IN",           \_arguments -> zoomIn      >> return "OK"),
        ("ZOOM_OUT",          \_arguments -> zoomOut     >> return "OK")]
