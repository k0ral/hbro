{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TypeFamilies      #-}
module Hbro.Defaults where

-- {{{ Imports
import           Hbro.Attributes
import           Hbro.Clipboard                  as Clipboard
import           Hbro.Config                     (ConfigReader)
import           Hbro.Core
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Gui                        as Gui
import           Hbro.Gui.Builder
import           Hbro.Gui.MainView
import           Hbro.Gui.NotificationBar
import           Hbro.Gui.PromptBar              as Prompt
import           Hbro.Gui.StatusBar
import           Hbro.IPC
import           Hbro.Keys                       as Keys
import           Hbro.Keys.Model                 (singleKey, (.|))
import           Hbro.Logger
import           Hbro.Prelude
import           Hbro.WebView.Signals

import           Data.Map                        as Map hiding (foldl, map)

import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Gdk.EventM      as Gdk
import           Graphics.UI.Gtk.WebKit.WebView
import           Graphics.UI.Gtk.Windows.Window

import           Network.URI.Extended

import qualified System.Glib.Attributes          as G
-- }}}

type OmniReader m = (ControlIO m, MonadError Text m, ConfigReader m, PromptBarReader m, NotificationBarReader m, StatusBarReader m, UIReader m, KeySignalReader m, Alternative m, MainViewReader m)

defaultDownloadHook :: (MonadIO m) => Input Download -> m ()
defaultDownloadHook _ = warningM "No download hook defined."

defaultLinkClickedHook :: (MonadIO m, MonadError Text m, MainViewReader m) => Input LinkClicked -> m ()
defaultLinkClickedHook (uri, Gdk.MiddleButton) = spawn "hbro" ["-u", show uri]
defaultLinkClickedHook (uri, _) = load uri

defaultLoadRequestedHook :: (MonadIO m, MonadError Text m, MainViewReader m) => URI -> m ()
defaultLoadRequestedHook = load

defaultNewWindowHook :: (MonadIO m) => URI -> m ()
defaultNewWindowHook uri = spawn "hbro" ["-u", show uri]

defaultTitleChangedHook :: (MonadIO m, MonadError Text m, UIReader m) => Text -> m ()
defaultTitleChangedHook title = getMainWindow >>= gAsync . (`G.set` [ windowTitle G.:= ("hbro | " ++ title)])

-- /!\ NetworkRequest's Haskell binding is missing the function "webkit_network_request_get_message", which makes it rather useless...
-- | Display content if webview can show the given MIME type, otherwise download it.
-- instance Default (Hook KE ResourceOpened) where
--     def = Hook $ \(_uri, mimetype) -> do
--         return Hooks.Load <<| canRender mimetype |>> return Hooks.Download'


-- | List of default supported requests.
defaultCommandMap :: OmniReader m => CommandMap m
defaultCommandMap = Map.fromList
-- Get information
    [ "GET_URI"             >: \_arguments -> Right . tshow <$> getCurrentURI
    , "GET_TITLE"           >: \_arguments -> Right <$> getPageTitle
    , "GET_FAVICON_URI"     >: \_arguments -> Right . tshow <$> getFaviconURI
    , "GET_LOAD_PROGRESS"   >: \_arguments -> Right . tshow <$> getLoadProgress
-- Trigger actions
    , "LOAD_URI"            >: \arguments -> case arguments of
            uri:_ -> parseURIReferenceM uri >>= load >> return (Right "OK")
            _     -> return . Left $ "Argument needed."
    , "STOP_LOADING"        >: \_arguments -> stopLoading       >> return (Right "OK")
    , "RELOAD"              >: \_arguments -> reload            >> return (Right "OK")
    , "RELOAD_BYPASS_CACHE" >: \_arguments -> reloadBypassCache >> return (Right "OK")
    , "GO_BACK"             >: \_arguments -> goBack            >> return (Right "OK")
    , "GO_FORWARD"          >: \_arguments -> goForward         >> return (Right "OK")
    , "ZOOM_IN"             >: \_arguments -> zoomIn            >> return (Right "OK")
    , "ZOOM_OUT"            >: \_arguments -> zoomOut           >> return (Right "OK")
    ]


defaultKeyMap :: (OmniReader m) => KeyMap m
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
   , [_Control .| _v]      >: Clipboard.read >>= parseURIReferenceM >>= load
   , [_Alt     .| _v]      >: Clipboard.read >>= spawn "hbro" . ("-u":) . (:[]) . unpack
-- Display
   , [_Control .| _plus]   >: zoomIn
   , [_Control .| _minus]  >: zoomOut
   , [_Control .| _b]      >: getStatusBar >>= \s -> toggle_ s widgetVisible
   , [_Alt     .| _b]      >: getNotificationBar >>= \n -> toggle_ n widgetVisible
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
   , [_Alt     .| _Print]  >: printPage
   , [_Control .| _t]      >: spawn "hbro" []
   , [_Control .| _w]      >: quit
   ]
