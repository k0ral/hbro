{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hbro.WebView.Signals where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Keys                                  as Keys
import           Hbro.Keys.Model                            ((.|))
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Monad.Trans.Maybe

import           Data.Set                                   as S hiding (map)

import           Graphics.UI.Gtk.Abstract.Object
import           Graphics.UI.Gtk.Abstract.Widget            hiding (KeyVal)
import           Graphics.UI.Gtk.Gdk.EventM                 as Gdk
import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.WebKit.Download            as W hiding
                                                                  (Download,
                                                                  downloadGetSuggestedFilename,
                                                                  downloadGetUri)
import           Graphics.UI.Gtk.WebKit.Extended            as W
import           Graphics.UI.Gtk.WebKit.WebNavigationAction
import           Graphics.UI.Gtk.WebKit.WebPolicyDecision

import           Network.URI.Extended

import qualified System.Glib.Attributes                     as Glib
import           System.Glib.GError
import           System.Glib.Signals                        hiding (Signal)
-- }}}

data Download = Download deriving(Show)
instance Event Download where
  type Input Download = (URI, Text, Maybe Int)
  describeInput _ (uri, _, _) = Just $ "Requested download <" <> show uri <> ">"

data LinkClicked = LinkClicked deriving(Show)
instance Event LinkClicked where
  type Input LinkClicked = (URI, MouseButton)
  describeInput _ (uri, _) = Just $ "Link clicked <" <> show uri <> ">"

data LinkHovered = LinkHovered deriving(Show)
instance Event LinkHovered where
  type Input LinkHovered = (URI, Maybe Text)
  describeInput _ (uri, _) = Just $ "Link hovered <" <> show uri <> ">"

data LinkUnhovered = LinkUnhovered deriving(Show)
instance Event LinkUnhovered where
  describeInput _ _ = Just "Link unhovered"

data LoadCommitted = LoadCommitted deriving(Show)
instance Event LoadCommitted where
  describeInput _ _ = Just "Load committed"

data LoadFailed = LoadFailed deriving(Show)
instance Event LoadFailed where
  type Input LoadFailed = (URI, GError)
  describeInput _ (uri, e) = Just $ "Error loading <" <> show uri <> "> : " <> show e

data LoadFinished = LoadFinished deriving(Show)
instance Event LoadFinished where
  describeInput _ _ = Just "Load finished"

data LoadRequested = LoadRequested deriving(Show)
instance Event LoadRequested where
  type Input LoadRequested = URI
  describeInput _ uri = Just $ "Load requested <" <> show uri <> ">"

data LoadStarted = LoadStarted deriving(Show)
instance Event LoadStarted where
  describeInput _ _ = Just "Load started"

data NewWindow = NewWindow deriving(Show)
instance Event NewWindow where
  type Input NewWindow = URI
  describeInput _ uri = Just $ "New window <" <> show uri <> ">"

data ProgressChanged = ProgressChanged deriving(Show)
instance Event ProgressChanged where
  type Input ProgressChanged = Int
  describeInput _ percent = Just $ "Load progress: " <> show percent <> "%"

data ResourceOpened = ResourceOpened deriving(Show)
instance Event ResourceOpened where
  type Input ResourceOpened = (URI, Text)
  describeInput _ _ = Just "Resource opened"

data TitleChanged = TitleChanged deriving(Show)
instance Event TitleChanged where
  type Input TitleChanged = Text
  describeInput _ = Just . (<>) "Title changed to: "

data URIChanged = URIChanged deriving(Show)
instance Event URIChanged where
  type Input URIChanged = URI
  describeInput _ = Just . (<>) "URI changed to: " . show

data ZoomLevelChanged = ZoomLevelChanged deriving(Show)
instance Event ZoomLevelChanged where
  type Input ZoomLevelChanged = Float
  describeInput _ value = Just $ "Zoom level changed to: " <> show value

data ResourceAction = Load | Download' deriving(Show)
instance Describable ResourceAction where describe = show


attachDownload :: (ControlIO m, MonadCatch m, MonadLogger m)
               => WebView -> Signal Download -> m (ConnectId WebView)
attachDownload webView signal = liftBaseWith $ \runInIO -> gSync . on webView downloadRequested $ \d -> do
  runInIO . logErrors $ do
    amount <- io $ downloadGetTotalSize d
    uri    <- downloadGetUri d
    name   <- downloadGetSuggestedFilename d

    emit signal (uri, name, Just amount)
  return False


attachLinkHovered :: (ControlIO m, MonadCatch m, MonadLogger m)
                  => WebView -> Signal LinkHovered -> Signal LinkUnhovered -> m (ConnectId WebView)
attachLinkHovered webView hoveredSignal unhoveredSignal = liftBaseWith $ \runInIO -> gSync $ on webView hoveringOverLink (\a b -> void . runInIO $ callback a b)
  where callback title (Just uri) = void . logErrors $ do
          u <- parseURI $ pack uri
          emit hoveredSignal (u, pack <$> title)
        callback _ _ = emit unhoveredSignal ()


attachLoadCommitted :: (ControlIO m, MonadLogger m) => WebView -> Signal LoadCommitted -> m (ConnectId WebView)
attachLoadCommitted webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadCommitted $ \_frame -> void . runInIO $ emit signal ()


attachLoadFailed :: (ControlIO m, MonadCatch m, MonadLogger m)
                 => WebView -> Signal LoadFailed -> m (ConnectId WebView)
attachLoadFailed webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadError $ \_frame uri e -> do
  runInIO . logErrors $ do
    uri' <- parseURIReference uri
    emit signal (uri', e)
  return False


attachLoadFinished :: (ControlIO m, MonadLogger m) => WebView -> Signal LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadFinished $ \_frame -> void . runInIO $ emit signal ()


attachLoadStarted :: (ControlIO m, MonadLogger m) => WebView -> Signal LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadStarted $ \_frame -> void . runInIO $ emit signal ()

attachNavigationRequest :: (ControlIO m, MonadCatch m, MonadLogger m) => WebView -> Signal LinkClicked -> Signal LoadRequested -> m (ConnectId WebView)
attachNavigationRequest webView signal1 signal2 = liftBaseWith $ \runInIO -> gSync . on webView navigationPolicyDecisionRequested $ \_frame request action decision -> do
    reason <- webNavigationActionGetReason action
    button <- toMouseButton <$> webNavigationActionGetButton action

    -- io . putStrLn . ("Request data: " ++) =<< networkRequestGetBody request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetContentType request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetURI request

    runInIO $ do
        uri <- networkRequestGetUri request

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> do
                emit signal1 (uri, b)
                io $ webPolicyDecisionIgnore decision
            (WebNavigationReasonOther, _) -> do
                debug $ "Navigation request to <" <> show uri <> ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonBackForward, _) -> io $ webPolicyDecisionUse decision
            (WebNavigationReasonReload, _) -> io $ webPolicyDecisionUse decision
            (WebNavigationReasonFormSubmitted, _) -> do
                debug $ "Form submitted to <" <> show uri <> ">"
                io $ webPolicyDecisionUse decision
            _ -> do
                debug $ "Navigation request [" <> show reason <> "] to <" <> show uri <> ">"
                emit signal2 uri
                io $ webPolicyDecisionIgnore decision
      `catchAny` \e -> do
        error (show e)
        io $ webPolicyDecisionUse decision

    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (ControlIO m, MonadCatch m, MonadLogger m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = liftBaseWith $ \runInIO -> gSync . on webView createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        runInIO . logErrors $ networkRequestGetUri request >>= emit signal
        webPolicyDecisionIgnore decision
        return True

    return webView'


attachNewWindow :: (ControlIO m, MonadCatch m, MonadLogger m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = liftBaseWith $ \runInIO -> gSync . on webView newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    runInIO . logErrors $ networkRequestGetUri request >>= emit signal
    webPolicyDecisionIgnore decision
    return True


attachProgressChanged :: (ControlIO m, MonadLogger m) => WebView -> Signal ProgressChanged -> m (ConnectId WebView)
attachProgressChanged webView signal = liftBaseWith $ \runInIO -> gSync . on webView progressChanged $ void . runInIO . emit signal

-- attachResourceOpened :: (MonadIO m) => WebView -> Signal ResourceOpened -> m (ConnectId WebView)
-- attachResourceOpened webView signal = liftBaseWith $ \runInIO -> gSync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
--     action <- logErrors $ do
--         uri <- networkRequestGetUri request
--         debug $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" <> show uri <> ">"
--         -- io . waitForResult =<<
--         emit signal (uri, mimetype)

--     debug "debug" $ "decision made: " ++ show action
--     case action of
--         Just Load -> webPolicyDecisionUse decision
--         Just Download' -> webPolicyDecisionDownload decision
--         _ -> webPolicyDecisionIgnore decision
--     return True

-- waitForResult output = do
--   debug "hbro.debug" "iteration" >> mainIteration
--   maybe (waitForResult output) return =<< (atomically $ tryTakeTMVar output)


attachTitleChanged :: (ControlIO m, MonadLogger m) => WebView -> Signal TitleChanged -> m (ConnectId WebView)
attachTitleChanged webView signal = liftBaseWith $ \runInIO -> gSync . on webView W.titleChanged $ \_frame title -> void . runInIO $ emit signal title

attachUriChanged :: (ControlIO m, MonadCatch m, MonadLogger m) => WebView -> Signal URIChanged -> m (ConnectId WebView)
attachUriChanged webView signal = liftBaseWith $ \runInIO ->
  gSync . on webView (notifyProperty W.webViewUri) . void . runInIO . logErrors $
    io (Glib.get webView webViewUri) >>= maybe (throwM UnavailableUri) return >>= parseURI >>= emit signal

attachZoomLevelChanged :: (ControlIO m, MonadLogger m) => WebView -> Signal ZoomLevelChanged -> m (ConnectId WebView)
attachZoomLevelChanged webView signal = liftBaseWith $ \runInIO -> gSync . on webView (notifyProperty webViewZoomLevel) . void . runInIO $ emit signal =<< io (Glib.get webView webViewZoomLevel)


attachKeyPressed :: (ControlIO m, MonadLogger m) => WebView -> Signal KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = liftBaseWith $ \runInIO -> gSync . on webView keyPressEvent $ do
    modifiers <- Modifier . S.delete Gdk.Shift . S.fromList <$> Gdk.eventModifier
    key       <- KeyVal <$> Gdk.eventKeyVal

    io . runInIO . runMaybeT $ do
        guard . not $ isModifier key || isModalKey key
        emit signal $ modifiers .| key

    return False
