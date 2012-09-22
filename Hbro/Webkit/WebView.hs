{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- | Rewrite many 'Graphics.UI.Gtk.WebKit.WebView' functions in a monadic way.
module Hbro.Webkit.WebView where

-- {{{ Imports
import Hbro.Keys
import Hbro.Types
import Hbro.Util

import Control.Conditional
import Control.Monad.Error  hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)
import Control.Monad.Trans.Control

import Data.Default
import Data.Foldable (forM_, mapM_)
import Data.Functor

import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk.General.General as GTK
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import qualified Graphics.UI.Gtk.WebKit.WebFrame as W
import qualified Graphics.UI.Gtk.WebKit.Download as W
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as W
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew)
import qualified Graphics.UI.Gtk.WebKit.WebView as W

import Network.URI as N hiding(parseURI, parseURIReference)

import Prelude hiding(mapM_)

import System.Glib.Attributes
import System.Glib.Signals
-- }}}

-- {{{ Init
instance Buildable (WebView, ScrolledWindow) where
    build builder = io $ do
        window  <- builderGetObject builder castToScrolledWindow "webViewParent"
        webView <- W.webViewNew
        containerAdd window webView

        return (webView, window)

setup :: (MonadIO m, MonadReader r m, HasWebView r) => m ()
setup = do
    webView <- asks _webview
    io $ webView `set` [ widgetCanDefault := True ]
    io . void . on webView W.closeWebView $ GTK.mainQuit >> return False

-- }}}

-- {{{ Monad-agnostic version of various WebKit functions
webViewGetUri :: (MonadIO m, MonadError HError m) => W.WebView -> m URI
webViewGetUri = maybe (throwError InvalidPageURI) parseURI <=< io . W.webViewGetUri

webViewGetTitle :: (MonadIO m, MonadError HError m) => W.WebView -> m String
webViewGetTitle = maybe (throwError InvalidPageTitle) return <=< io . W.webViewGetTitle

webViewGetIconUri :: (MonadIO m, MonadError HError m) => W.WebView -> m URI
webViewGetIconUri = maybe (throwError InvalidIconURI) parseURI <=< io . W.webViewGetUri
-- }}}

-- {{{ Getters
getFaviconURI :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m URI
getFaviconURI = webViewGetIconUri =<< asks _webview

getLoadProgress :: (MonadIO m, MonadReader r m, HasWebView r) => m Double
getLoadProgress = io . W.webViewGetProgress =<< asks _webview

getURI :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m URI
getURI = webViewGetUri =<< asks _webview

getTitle :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m String
getTitle = webViewGetTitle =<< asks _webview
-- }}}

-- {{{ Browsing
loadURI :: (MonadIO m, MonadReader r m, HasWebView r) => URI -> m ()
loadURI uri = do
    logVerbose $ "Loading URI: " ++ (show uri')
    io . (`W.webViewLoadUri` uri') =<< asks _webview
  where
    uri' = case uriScheme uri of
             [] -> "http://" ++ show uri
             _  -> show uri

reload, reloadBypassCache :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m ()
reload            = io . W.webViewReload =<< asks _webview
reloadBypassCache = io . W.webViewReloadBypassCache =<< asks _webview

stopLoading :: (MonadIO m, MonadReader r m, HasWebView r) => m ()
stopLoading = io . W.webViewStopLoading =<< asks _webview
--    notify 5000 "Stopped loading"

goBack, goForward :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m ()
goBack    = do
    view <- asks _webview
    unlessM (io $ W.webViewCanGoBack view) $ throwError CannotGoBack
    io $ W.webViewGoBack view
goForward = do
    view <- asks _webview
    unlessM (io $ W.webViewCanGoForward view) $ throwError CannotGoForward
    io $ W.webViewGoForward view
-- }}}

-- {{{ Display
-- | Toggle source display.
-- Current implementation forces a refresh of current web page, which may be undesired.
toggleSourceMode :: (MonadIO m, MonadReader r m, HasWebView r, MonadError HError m) => m ()
toggleSourceMode = do
    view <- asks _webview
    io . W.webViewSetViewSourceMode view =<< (io $ not <$> W.webViewGetViewSourceMode view)
    reload

zoomIn, zoomOut :: (MonadIO m, MonadReader r m, HasWebView r) => m ()
zoomIn  = io . W.webViewZoomIn  =<< asks _webview
zoomOut = io . W.webViewZoomOut =<< asks _webview

-- | Show web inspector for current webpage.
showWebInspector :: (MonadIO m, MonadReader r m, HasWebView r) => m ()
showWebInspector = do
    inspector <- io . W.webViewGetInspector =<< asks _webview
    io $ webInspectorInspectCoordinates inspector 0 0
-- }}}

-- {{{ Hooks
afterKeyPressed :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, MonadError HError m, HasKeys r) => KeyHook -> m (ConnectId WebView)
afterKeyPressed f = do
  webView <- asks _webView
  liftBaseWith $ \runInIO -> after webView keyPressEvent $ do
    modifiers <- eventModifier
    key'      <- keyToString <$> eventKeyVal

    io . forM_ key' $ \key -> do
        let keystrokes = (++ key) . concat . map stringify $ modifiers
        logVerbose $ "Key pressed: " ++ keystrokes
        runInIO $ f keystrokes `catchError` \e -> (io $ print e) >> notify 5000 (show e)
    return False

-- | Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu  "Open in new window"
onNewWebView :: (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, HasKeys r, MonadError HError m) => NewWebViewHook -> m (ConnectId WebView)
onNewWebView (NewWebViewHook f) = do
    webView <- asks _webView
    env    <- ask
    io $ on webView W.createWebView $ \frame -> do
        result <- runErrorT . (`runReaderT` env) $ f frame
        case result of
            Left  e -> print e >> webViewNew
            Right r -> return r


instance Default NewWebViewHook where
    def = NewWebViewHook $ \_frame -> do
        --forM_ uri $ (runK env) . callback
        webView <- io webViewNew

        io . void . on webView W.webViewReady $ return True
        io . void . on webView W.navigationPolicyDecisionRequested $ \_ request _ decision -> do
            W.networkRequestGetUri request >>= mapM_ (\uri -> spawn "hbro" ["-u", uri])
            webPolicyDecisionIgnore decision
            return True

        return webView


onDownload :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => DownloadHook -> m (ConnectId WebView)
onDownload (DownloadHook f) = do
    webView <- asks _webView
    liftBaseWith $ \runInIO -> on webView W.downloadRequested $ \download -> do
      void . runInIO $ do
        uri      <- downloadGetUri download
        filename <- downloadGetSuggestedFilename download
        size     <- io $ W.downloadGetTotalSize download

        logVerbose $ "Requested download: " ++ show uri
        notify 5000 $ "Requested download: " ++ filename ++ " (" ++ show size ++ ")"
        f uri filename size
      return False


onLoadFinished :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => LoadFinishedHook -> m (ConnectId WebView)
onLoadFinished (LoadFinishedHook f) = do
    webView<- asks _webView
    liftBaseWith $ \runInIO -> on webView W.loadFinished $ \_frame-> void . runInIO $ do
        f `catchError` \e -> (io $ print e) >> notify 5000 (show e)

onNavigationRequest :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => NavigationHook -> m (ConnectId WebView)
onNavigationRequest (NavigationHook f) = do
  webView <- asks _webView
  liftBaseWith $ \runInIO -> on webView W.navigationPolicyDecisionRequested $ \_frame request action decision -> do
    void . runInIO $ do
      uri    <- networkRequestGetUri request
      reason <- io $ webNavigationActionGetReason action
      button <- io $ webNavigationActionGetButton action

      logVerbose $ "Requested navigation to <" ++ show uri ++ "> caused by " ++ show reason
      f reason (toMouseButton button) uri decision
    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


onNewWindow :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => NewWindowHook -> m (ConnectId WebView)
onNewWindow (NewWindowHook f) = do
  webView <- asks _webView
  liftBaseWith $ \runInIO -> on webView W.newWindowPolicyDecisionRequested $ \frame request action decision -> do
    void $ runInIO (f frame request action decision)
    return True


onResourceOpened :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => ResourceOpenedHook -> m (ConnectId WebView)
onResourceOpened (ResourceOpenedHook f) = do
  webView <- asks _webView
  liftBaseWith $ \runInIO -> on webView W.mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
    void . runInIO $ do
      uri <- networkRequestGetUri request
      f uri mimetype decision
    return True


onTitleChanged :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => TitleChangedHook -> m (ConnectId WebView)
onTitleChanged (TitleChangedHook f) = do
    webView <- asks _webView
    liftBaseWith $ \runInIO -> on webView W.titleChanged $ \_frame title -> void . runInIO $ do
      logVerbose $ "Title changed to: " ++ title
      f title
-- }}}

-- | Wrapper around 'webViewSearchText', provided for convenience
searchText :: (MonadIO m, MonadReader r m, HasWebView r) => CaseSensitivity -> Direction -> Wrap -> String -> m Bool
searchText s d w text = do
    view <- asks _webview
    io $ W.webViewSearchText view text (isCaseSensitive s) (isForward d) (isWrapped w)

searchText_ :: (MonadIO m, MonadReader r m, HasWebView r) => CaseSensitivity -> Direction -> Wrap -> String -> m ()
searchText_ s d w text = searchText s d w text >> return ()

-- | Wrapper around 'webFramePrint' function, provided for convenience.
printPage :: (MonadIO m, MonadReader r m, HasWebView r) => m ()
printPage = io . W.webFramePrint =<< io . W.webViewGetMainFrame =<< asks _webview
