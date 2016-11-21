{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Hbro.Defaults where

-- {{{ Imports
import           Hbro.Clipboard                           as Clipboard
import           Hbro.Config                              (Config)
import           Hbro.Core
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Gui                                 as Gui
import           Hbro.Gui.MainView
import           Hbro.Gui.NotificationBar
import           Hbro.Gui.PromptBar                       as Prompt
import           Hbro.Gui.StatusBar
import           Hbro.IPC
import           Hbro.Keys                                as Keys
import           Hbro.Keys.Model                          (singleKey, (.|))
import           Hbro.Logger
import           Hbro.Prelude                             hiding (Handler)
import           Hbro.WebView.Signals

import           Control.Monad.Trans.Resource

import           Data.Map                                 as Map hiding (foldl,
                                                                  map)

import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Builder                  as Gtk
import           Graphics.UI.Gtk.Gdk.EventM               as Gtk
import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.WebKit.WebView
import           Graphics.UI.Gtk.Windows.Window

import           Network.URI.Extended

import           System.Glib.Attributes.Extended
-- }}}

-- | A 'God' monad has access to everything, and obviously never throws errors.
type God r m = (ControlIO m, MonadLogger m, MonadResource m, MonadReader r m, Has (TVar Config) r, Has PromptBar r, Has NotificationBar r, Has StatusBar r, Has Gtk.Builder r, Has (Signal KeyMapPressed) r, Has MainView r, Alternative m)

defaultLinkClickedHandler :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader r m, Has MainView r)
                          => Handler m LinkClicked
defaultLinkClickedHandler (uri, Gtk.MiddleButton) = spawnHbro' uri
defaultLinkClickedHandler (uri, _) = load uri

defaultLoadRequestedHandler :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader r m, Has MainView r)
                            => URI -> m ()
defaultLoadRequestedHandler = load

defaultNewWindowHandler :: (MonadIO m, MonadLogger m) => URI -> m ()
defaultNewWindowHandler = spawnHbro'

defaultTitleChangedHandler :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader r m, Has Gtk.Builder r)
                           => Text -> m ()
defaultTitleChangedHandler title = getMainWindow >>= \w -> set w windowTitle ("hbro | " ++ title) >> return ()

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
-- instance Default (Hook KE ResourceOpened) where
--     def = Hook $ \(_uri, mimetype) -> do
--         return Hooks.Load <<| canRender mimetype |>> return Hooks.Download'


-- | List of default supported requests.
defaultCommandMap :: (God r m, MonadCatch m) => CommandMap m
defaultCommandMap = Map.fromList
-- Get information
    [ "GET_URI"             >: \_arguments -> Right . tshow <$> getCurrentURI
    , "GET_TITLE"           >: \_arguments -> Right <$> getPageTitle
    , "GET_FAVICON_URI"     >: \_arguments -> Right . tshow <$> getFaviconURI
    , "GET_LOAD_PROGRESS"   >: \_arguments -> Right . tshow <$> getLoadProgress
-- Trigger actions
    , "LOAD_URI"            >: \arguments -> case arguments of
            uri:_ -> parseURIReference uri >>= load >> return (Right "OK")
            _     -> return . Left $ "Argument needed."
    , "STOP_LOADING"        >: \_arguments -> stopLoading       >> return (Right "OK")
    , "RELOAD"              >: \_arguments -> reload            >> return (Right "OK")
    , "RELOAD_BYPASS_CACHE" >: \_arguments -> reloadBypassCache >> return (Right "OK")
    , "GO_BACK"             >: \_arguments -> goBack            >> return (Right "OK")
    , "GO_FORWARD"          >: \_arguments -> goForward         >> return (Right "OK")
    , "ZOOM_IN"             >: \_arguments -> zoomIn            >> return (Right "OK")
    , "ZOOM_OUT"            >: \_arguments -> zoomOut           >> return (Right "OK")
    ]


defaultKeyMap :: (God r m, MonadCatch m) => KeyMap m
defaultKeyMap = Map.fromList
-- Browse
   [ [_Alt     .| _Left]   >: goBack
   , [_Alt     .| _Right]  >: goForward
   , [_Control .| _Escape] >: stopLoading
   , [singleKey _F5]       >: reload
   , [_Control .| _r]      >: reload
   , [_Control .| _F5]     >: reloadBypassCache
   , [_Alt     .| _r]      >: reloadBypassCache
   , [_Control .| _dead_circumflex] >: scrollH (Absolute 0)
   , [_Control .| _dollar] >: scrollH (Absolute 100)
   , [_Control .| _Home]   >: scrollV (Absolute 0)
   , [_Control .| _End]    >: scrollV (Absolute 100)
   , [_Alt     .| _Home]   >: goHome
-- Copy/paste
   , [_Control .| _c]      >: getCurrentURI >>= Clipboard.write . tshow
   , [_Alt     .| _c]      >: getPageTitle >>= Clipboard.write
   , [_Control .| _v]      >: Clipboard.read >>= parseURIReference >>= load
   , [_Alt     .| _v]      >: Clipboard.read >>= parseURIReference >>= spawnHbro'
-- Display
   , [_Control .| _plus]   >: zoomIn
   , [_Control .| _minus]  >: zoomOut
   , [_Control .| _b]      >: asks asStatusBar >>= \s -> toggle_ s widgetVisible
   , [_Alt     .| _b]      >: asks asNotificationBar >>= \n -> toggle_ n widgetVisible
   , [_Control .| _u]      >: getWebView >>= \v -> toggle_ v webViewViewSourceMode
-- Prompt
   , [_Control .| _o]      >: uriPromptM "Open URI" "" >>= load
   , [_Alt     .| _o]      >: getCurrentURI >>= \uri -> uriPromptM "Open URI " (tshow uri) >>= load
-- Search
   , [singleKey _slash]    >: ipromptM "Search " "" (searchText_ CaseInsensitive Forward Wrap)
   , [_Control .| _f]      >: ipromptM "Search " "" (searchText_ CaseInsensitive Forward Wrap)
   , [singleKey _question] >: ipromptM "Search " "" (searchText_ CaseInsensitive Backward Wrap)
   , [_Control .| _n]      >: searchText_ CaseInsensitive Forward  Wrap =<< getPromptValueM
   , [_Control .| _p]      >: searchText_ CaseInsensitive Backward Wrap =<< getPromptValueM
   , [_Control .| _h]      >: gAsync . webViewUnMarkTextMatches =<< getWebView
-- Misc
    --, (_Control .| _i)      >: openInspector
   , [_Control .| _s]      >: promptM "Save webpage " "" >>= saveWebPage . unpack
   , [_Alt     .| _Print]  >: printPage
   , [_Control .| _t]      >: spawnHbro
   , [_Control .| _w]      >: quit
   ]
