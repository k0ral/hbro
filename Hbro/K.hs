{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Hbro.K (
    KData,
    K,
    init,
    getHooks,
    resetKeyBindings,
) where

-- {{{
import Hbro.Clipboard as Clipboard
import Hbro.Config as Config
import Hbro.Core
import Hbro.Error
import Hbro.Gdk.KeyVal
import Hbro.Gui as Gui
import Hbro.Gui.NotificationBar
import Hbro.Gui.PromptBar as Prompt
-- import Hbro.IPC as IPC (Command(..))
import Hbro.IPC.Hooks as IPC
import Hbro.IPC.Signals as IPC
import Hbro.Keys as Keys
import Hbro.Keys.Model ((.|))
import Hbro.Keys.Monadic as Keys
import Hbro.Hooks as Hooks
import Hbro.Signals as Signals
import Hbro.Gui.PromptBar.Hooks
import Hbro.Util
import Hbro.WebView.Hooks as WebView
import Hbro.WebView.Signals as WebView

import Control.Arrow
import Control.Concurrent.STM
-- import Control.Lens.Getter
-- import Control.Lens.Setter
import Control.Lens.TH
-- import Control.Lens.Type hiding(Action)
import Control.Monad.Reader

import Data.Map as M hiding(foldl, map)

import Graphics.UI.Gtk.Gdk.EventM as Gdk
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI.Monadic

import Prelude hiding(init)

import qualified System.Glib.Attributes as G
-- }}}

data KData = KData
    { __config      :: TVar Config
    , __gui         :: GUI
    , __hooks       :: Hooks.Hooks K
    , __signals     :: Signals.Signals
    }

type K  = ReaderT KData IO
-- type KE = ReaderT KData (ErrorT String IO)

makeLenses ''KData


instance HasConfig KData           where _config          = Hbro.K._config
instance HasGUI KData              where _gui             = Hbro.K._gui
instance Keys.HasHooks K KData     where _hooks           = Hbro.K._hooks.keyHooksL
instance HasPromptHooks K KData    where _promptHooks     = Hbro.K._hooks.promptHooksL
instance WebView.HasHooks K KData  where _hooks           = Hbro.K._hooks.webViewHooksL
instance HasNotificationBar KData  where _notificationbar = Hbro.K._gui.notificationBarL
instance HasPromptBar KData        where _promptbar       = Hbro.K._gui.promptBarL


init :: (MonadBase IO m) => GUI -> Hooks.Hooks K -> Signals.Signals -> m KData
init ui hooks signals = io (KData <$> newTVarIO def <*> pure ui <*> pure hooks <*> pure signals)

getHooks :: (MonadReader KData m) => m (Hooks.Hooks K)
getHooks = askl Hbro.K._hooks

-- {{{ Default configuration
-- instance Default (Signals.DownloadHook K) where
    -- def = Signals.DownloadHook . const . const . const $ return ()

instance Default (Hooks.LinkClickedHook K) where
    def = Hooks.LinkClickedHook f
            where f (LinkClicked uri Gdk.MiddleButton) = spawn "hbro" [show uri]
                  f (LinkClicked uri _               ) = load uri

instance Default (Hooks.LoadRequestedHook K) where
    def = Hooks.LoadRequestedHook $ \(LoadRequested uri) -> load uri

instance Default (Hooks.NewWindowHook K) where
    def = Hooks.NewWindowHook $ \(NewWindow uri) -> spawn "hbro" [show uri]

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
instance Default (Hooks.ResourceOpenedHook K) where
    def = Hooks.ResourceOpenedHook $ \(ResourceOpened _uri mimetype) -> do
        return Hooks.Load <<| canRender mimetype |>> return Hooks.Download'

instance Default (Hooks.TitleChangedHook K) where
    def = Hooks.TitleChangedHook (\(TitleChanged title) -> gAsync . (`G.set` [ windowTitle G.:= ("hbro | " ++ title)]) =<< Gui.get mainWindowL)


-- | List of default supported requests.
instance Default (IPC.Hooks K) where
    def = IPC.Hooks . M.fromList . map (first IPC.Command) $ [
    -- Get information
        ("GET_URI",             \_arguments -> Right . show <$> getCurrentURI),
        ("GET_TITLE",           \_arguments -> Right . show <$> getPageTitle),
        ("GET_FAVICON_URI",     \_arguments -> Right . show <$> getFaviconURI),
        ("GET_LOAD_PROGRESS",   \_arguments -> Right . show <$> getLoadProgress),
    -- Trigger actions
        ("LOAD_URI",            \arguments -> case arguments of
            uri:_ -> parseURIReference uri >>= load >> (return $ Right "OK")
            _     -> return . Left $ "Argument needed."),
        ("STOP_LOADING",        \_arguments -> stopLoading       >> (return $ Right "OK")),
        ("RELOAD",              \_arguments -> reload            >> (return $ Right "OK")),
        ("RELOAD_BYPASS_CACHE", \_arguments -> reloadBypassCache >> (return $ Right "OK")),
        ("GO_BACK",             \_arguments -> goBack            >> (return $ Right "OK")),
        ("GO_FORWARD",          \_arguments -> goForward         >> (return $ Right "OK")),
        ("ZOOM_IN",             \_arguments -> zoomIn            >> (return $ Right "OK")),
        ("ZOOM_OUT",            \_arguments -> zoomOut           >> (return $ Right "OK"))]


resetKeyBindings :: (MonadBase IO m, MonadReader t m, Keys.HasHooks K t) => m ()
resetKeyBindings = do
-- Browse
    Keys.bind (_Alt     .| _Left)   $ goBack
    Keys.bind (_Alt     .| _Right)  $ goForward
    Keys.bind (_Control .| _Escape) $ stopLoading
    Keys.bind _F5                   $ reload
    Keys.bind (_Control .| _r)      $ reload
    Keys.bind (_Control .| _F5)     $ reloadBypassCache
    Keys.bind (_Alt     .| _r)      $ reloadBypassCache
    Keys.bind (_Control .| _dead_circumflex) $ scroll Horizontal (Absolute 0)
    Keys.bind (_Control .| _dollar) $ scroll Horizontal $ Absolute 100
    Keys.bind (_Control .| _Home)   $ scroll Vertical $ Absolute 0
    Keys.bind (_Control .| _End)    $ scroll Vertical $ Absolute 100
    Keys.bind (_Alt     .| _Home)   $ goHome
-- Copy/paste
    Keys.bind (_Control .| _c)      $ getCurrentURI >>= Clipboard.write . show
    Keys.bind (_Alt     .| _c)      $ getPageTitle >>= Clipboard.write
    Keys.bind (_Control .| _v)      $ Clipboard.read >>= parseURIReference >>= load
    Keys.bind (_Alt     .| _v)      $ Clipboard.read >>= spawn "hbro" . (:[])
-- Display
    Keys.bind (_Control .| _plus)   $ zoomIn
    Keys.bind (_Control .| _minus)  $ zoomOut
    Keys.bind _F11                  $ Gui.get mainWindowL >>= gAsync . windowFullscreen
    Keys.bind _Escape               $ Gui.get mainWindowL >>= gAsync . windowUnfullscreen
    Keys.bind (_Control .| _b)      $ Gui.toggle =<< Gui.get statusBarL
    Keys.bind (_Alt     .| _b)      $ Gui.toggle =<< Gui.get notificationBarL
    Keys.bind (_Control .| _u)      $ toggleSourceMode >> reload
-- Prompt
    Keys.bind (_Control .| _o)      $ promptURI "Open URI" "" >>= load
    Keys.bind (_Alt     .| _o)      $ getCurrentURI >>= \uri -> promptURI "Open URI " (show uri) >>= load
-- Search
    Keys.bind _slash                $ void $ prompt' "Search " "" $ searchText_ CaseInsensitive Forward Wrap
    Keys.bind (_Control .| _f)      $ prompt "Search " "" >>= searchText_ CaseInsensitive Forward Wrap
    Keys.bind _question             $ void $ prompt' "Search " "" $ searchText_ CaseInsensitive Backward Wrap
    Keys.bind (_Control .| _n)      $ searchText_ CaseInsensitive Forward  Wrap =<< Prompt.getEntryValue
    Keys.bind (_Control .| _p)      $ searchText_ CaseInsensitive Backward Wrap =<< Prompt.getEntryValue
    Keys.bind (_Control .| _h)      $ gAsync . webViewUnMarkTextMatches =<< Gui.get webViewL
-- Misc
    -- Keys.bind (_Control .| _i)      $ openInspector
    Keys.bind (_Alt     .| _p)      printPage
    -- Keys.bind (_Control .| _t)      $ spawn "hbro" []
    Keys.bind (_Control .| _w)      quit
-- }}}
