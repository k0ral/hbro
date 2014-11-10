{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Hbro.K (
    KData,
    K,
    init,
    getHooks,
    resetKeyBindings,
) where

-- {{{
import           Hbro.Clipboard                 as Clipboard
import           Hbro.Config                    as Config
import           Hbro.Core
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Gui                       as Gui
import           Hbro.Gui.NotificationBar
import           Hbro.Gui.PromptBar             as Prompt
-- import Hbro.IPC as IPC (Command(..))
import           Hbro.Gui.PromptBar.Hooks
import           Hbro.Hooks                     as Hooks
import           Hbro.IPC.Hooks                 as IPC
import           Hbro.IPC.Signals               as IPC
import           Hbro.Keys                      as Keys
import           Hbro.Keys.Model                ((.|))
import           Hbro.Keys.Monadic              as Keys
import           Hbro.Prelude
import           Hbro.Signals                   as Signals
import           Hbro.WebView.Hooks             as WebView
import           Hbro.WebView.Signals           as WebView

import           Control.Lens.TH

import           Data.Map                       as M hiding (foldl, map)

import           Graphics.UI.Gtk.Gdk.EventM     as Gdk
import           Graphics.UI.Gtk.WebKit.WebView
import           Graphics.UI.Gtk.Windows.Window

import           Network.URI.Monadic

import qualified System.Glib.Attributes         as G
-- }}}

declareLenses [d|
  data KData = KData
    { configL  :: TVar Config
    , guiL     :: GUI
    , hooksL   :: Hooks.Hooks KE
    , signalsL :: Signals.Signals
    }

  type K  = ReaderT KData IO
  type KE = ExceptT Text K
  |]

instance HasConfig KData           where _config          = configL
instance HasGUI KData              where gUI             = guiL
instance Keys.HasHooks KE KData    where _hooks           = hooksL.keyHooksL
instance HasPromptHooks KE KData   where _promptHooks     = hooksL.promptHooksL
instance WebView.HasHooks KE KData where _hooks           = hooksL.webViewHooksL
instance HasNotificationBar KData  where notificationBar = guiL.notificationBarL
instance HasPromptBar KData        where promptBar       = guiL.promptBarL


init :: (BaseIO m) => GUI -> Hooks.Hooks KE -> Signals.Signals -> m KData
init ui hooks signals = io (KData <$> newTVarIO def <*> pure ui <*> pure hooks <*> pure signals)

getHooks :: (MonadReader KData m) => m (Hooks.Hooks KE)
getHooks = askL hooksL

-- {{{ Default configuration
-- instance Default (Signals.DownloadHook KE) where
    -- def = Signals.DownloadHook . const . const . const $ return ()

instance Default (Hook KE LinkClicked) where
    def = Hook f
            where f (uri, Gdk.MiddleButton) = spawn "hbro" ["-u", show uri]
                  f (uri, _               ) = load uri

instance Default (Hook KE LoadRequested) where
    def = Hook load

instance Default (Hook KE NewWindow) where
    def = Hook $ \uri -> spawn "hbro" ["-u", show uri]

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
-- instance Default (Hook KE ResourceOpened) where
--     def = Hook $ \(_uri, mimetype) -> do
--         return Hooks.Load <<| canRender mimetype |>> return Hooks.Download'

instance Default (Hook KE TitleChanged) where
    def = Hook $ \title -> gAsync . (`G.set` [ windowTitle G.:= ("hbro | " ++ title)]) =<< Gui.get mainWindowL


-- | List of default supported requests.
instance Default (IPC.Hooks KE) where
    def = IPC.Hooks . M.fromList . map (first IPC.Command) $ [
    -- Get information
        ("GET_URI",             \_arguments -> Right . tshow <$> getCurrentURI),
        ("GET_TITLE",           \_arguments -> Right <$> getPageTitle),
        ("GET_FAVICON_URI",     \_arguments -> Right . tshow <$> getFaviconURI),
        ("GET_LOAD_PROGRESS",   \_arguments -> Right . tshow <$> getLoadProgress),
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


resetKeyBindings :: (BaseIO m, MonadReader t m, Keys.HasHooks (ExceptT Text K) t) => m ()
resetKeyBindings = do
-- Browse
    Keys.bind (_Alt     .| _Left)   $ goBack
    Keys.bind (_Alt     .| _Right)  $ goForward
    Keys.bind (_Control .| _Escape) $ stopLoading
    Keys.bind _F5                   $ reload
    Keys.bind (_Control .| _r)      $ reload
    Keys.bind (_Control .| _F5)     $ reloadBypassCache
    Keys.bind (_Alt     .| _r)      $ reloadBypassCache
    Keys.bind (_Control .| _dead_circumflex) $ scrollH (Absolute 0)
    Keys.bind (_Control .| _dollar) $ scrollH (Absolute 100)
    Keys.bind (_Control .| _Home)   $ scrollV (Absolute 0)
    Keys.bind (_Control .| _End)    $ scrollV (Absolute 100)
    Keys.bind (_Alt     .| _Home)   $ goHome
-- Copy/paste
    Keys.bind (_Control .| _c)      $ getCurrentURI >>= Clipboard.write . tshow
    Keys.bind (_Alt     .| _c)      $ getPageTitle >>= Clipboard.write
    Keys.bind (_Control .| _v)      $ Clipboard.read >>= parseURIReference >>= load
    Keys.bind (_Alt     .| _v)      $ Clipboard.read >>= spawn "hbro" . ("-u":) . (:[]) . unpack
-- Display
    Keys.bind (_Control .| _plus)   $ zoomIn
    Keys.bind (_Control .| _minus)  $ zoomOut
    Keys.bind (_Control .| _b)      $ Gui.toggle =<< Gui.get statusBarL
    Keys.bind (_Alt     .| _b)      $ Gui.toggle =<< Gui.get notificationBarL
    Keys.bind (_Control .| _u)      $ toggleSourceMode >> reload
-- Prompt
    Keys.bind (_Control .| _o)      $ promptURI "Open URI" "" >>= load
    Keys.bind (_Alt     .| _o)      $ getCurrentURI >>= \uri -> promptURI "Open URI " (tshow uri) >>= load
-- Search
    Keys.bind _slash                $ void $ prompt' "Search " "" . Hook $ searchText_ CaseInsensitive Forward Wrap
    Keys.bind (_Control .| _f)      $ prompt "Search " "" >>= searchText_ CaseInsensitive Forward Wrap
    Keys.bind _question             $ void $ prompt' "Search " "" . Hook $ searchText_ CaseInsensitive Backward Wrap
    Keys.bind (_Control .| _n)      $ searchText_ CaseInsensitive Forward  Wrap =<< Prompt.getEntryValue
    Keys.bind (_Control .| _p)      $ searchText_ CaseInsensitive Backward Wrap =<< Prompt.getEntryValue
    Keys.bind (_Control .| _h)      $ gAsync . webViewUnMarkTextMatches =<< Gui.get webViewL
-- Misc
    -- Keys.bind (_Control .| _i)      $ openInspector
    Keys.bind (_Alt     .| _Print)  printPage
    Keys.bind (_Control .| _t)      $ spawn "hbro" []
    Keys.bind (_Control .| _w)      quit
-- }}}
