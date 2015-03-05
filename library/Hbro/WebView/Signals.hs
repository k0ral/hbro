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
import           Hbro.Prelude                               hiding (on)

import           Graphics.UI.Gtk.WebKit.Lifted              as Lifted

import           Data.Set                                   as S hiding (map)

import           Graphics.UI.Gtk.Abstract.Object
import           Graphics.UI.Gtk.Abstract.Widget            hiding (KeyVal)
import           Graphics.UI.Gtk.Gdk.EventM                 as Gdk
import           Graphics.UI.Gtk.WebKit.Download            as W hiding
                                                                  (Download, downloadGetSuggestedFilename, downloadGetUri)
import           Graphics.UI.Gtk.WebKit.WebNavigationAction
import           Graphics.UI.Gtk.WebKit.WebPolicyDecision
import           Graphics.UI.Gtk.WebKit.WebView             as W hiding
                                                                  (LoadFinished)

import           Network.URI.Extended

import           System.Glib.Attributes.Extended
import           System.Glib.Signals                        hiding (Signal)
-- }}}

data Download = Download deriving(Show)
instance Event Download where
  type Input Download = (URI, Text, Maybe Int)
  describeInput _ (uri, _, _) = Just $ "Requested download <" ++ tshow uri ++ ">"

data LinkHovered = LinkHovered deriving(Show)
instance Event LinkHovered where
  type Input LinkHovered = (URI, Maybe Text)
  describeInput _ (uri, _) = Just $ "Link hovered <" ++ tshow uri ++ ">"

data LinkUnhovered = LinkUnhovered deriving(Show)
instance Event LinkUnhovered where
  describeInput _ _ = Just "Link unhovered"

data LinkClicked = LinkClicked deriving(Show)
instance Event LinkClicked where
  type Input LinkClicked = (URI, MouseButton)
  describeInput _ (uri, _) = Just $ "Link clicked <" ++ tshow uri ++ ">"

data LoadRequested = LoadRequested deriving(Show)
instance Event LoadRequested where
  type Input LoadRequested = URI
  describeInput _ uri = Just $ "Load requested <" ++ tshow uri ++ ">"

data LoadStarted = LoadStarted deriving(Show)
instance Event LoadStarted where
  describeInput _ _ = Just "Load started"

data LoadFinished = LoadFinished deriving(Show)
instance Event LoadFinished where
  describeInput _ _ = Just "Load finished"

data NewWindow = NewWindow deriving(Show)
instance Event NewWindow where
  type Input NewWindow = URI
  describeInput _ uri = Just $ "New window <" ++ tshow uri ++ ">"

data ResourceOpened = ResourceOpened deriving(Show)
instance Event ResourceOpened where
  type Input ResourceOpened = (URI, Text)
  describeInput _ _ = Just "Resource opened"

data TitleChanged = TitleChanged deriving(Show)
instance Event TitleChanged where
  type Input TitleChanged = Text
  describeInput _ = Just . (++) "Title changed to: "

data ZoomLevelChanged = ZoomLevelChanged deriving(Show)
instance Event ZoomLevelChanged where
  type Input ZoomLevelChanged = Float
  describeInput _ value = Just $ "Zoom level changed to: " ++ tshow value

data ResourceAction = Load | Download' deriving(Show)
instance Describable ResourceAction where describe = tshow


attachDownload :: (ControlIO m, MonadLogger m) => WebView -> Signal Download -> m (ConnectId WebView)
attachDownload webView signal = liftBaseWith $ \runInIO -> gSync . on webView downloadRequested $ \d -> do
    runInIO . runExceptT . logErrors $ do
        amount <- io $ downloadGetTotalSize d
        uri    <- downloadGetUri d
        name   <- downloadGetSuggestedFilename d

        emit signal (uri, name, Just amount)
    return False


attachLinkHovered :: (ControlIO m, MonadLogger m) => WebView -> Signal LinkHovered -> Signal LinkUnhovered -> m (ConnectId WebView)
attachLinkHovered webView hoveredSignal unhoveredSignal = liftBaseWith $ \runInIO -> gSync $ on webView hoveringOverLink (\a b -> void . runInIO $ callback a b)
  where callback title (Just uri) = void . runExceptT . logErrors $ do
          u <- parseURIM $ pack uri
          emit hoveredSignal (u, pack <$> title)
        callback _ _ = emit unhoveredSignal ()


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (ControlIO m, MonadLogger m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = liftBaseWith $ \runInIO -> gSync . on webView createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        runInIO . runExceptT . logErrors $ networkRequestGetUri request >>= emit signal
        webPolicyDecisionIgnore decision
        return True

    return webView'


attachLoadStarted :: (ControlIO m, MonadLogger m) => WebView -> Signal LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadStarted $ \_frame -> void (runInIO $ emit signal ())

attachLoadFinished :: (ControlIO m, MonadLogger m) => WebView -> Signal LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = liftBaseWith $ \runInIO -> gSync . on webView loadFinished $ \_frame -> void (runInIO $ emit signal ())


attachNavigationRequest :: (ControlIO m, MonadLogger m) => WebView -> (Signal LinkClicked, Signal LoadRequested) -> m (ConnectId WebView)
attachNavigationRequest webView (signal1, signal2) = liftBaseWith $ \runInIO -> gSync . on webView navigationPolicyDecisionRequested $ \_frame request action decision -> do
    reason <- webNavigationActionGetReason action
    button <- toMouseButton <$> webNavigationActionGetButton action

    -- io . putStrLn . ("Request data: " ++) =<< networkRequestGetBody request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetContentType request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetURI request

    runInIO . runExceptT $ do
        uri <- networkRequestGetUri request

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> do
                emit signal1 (uri, b)
                io $ webPolicyDecisionIgnore decision
            (WebNavigationReasonOther, _) -> do
                debug $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonBackForward, _) -> do
                debug $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonReload, _) -> do
                debug $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonFormSubmitted, _) -> do
                debug $ "Form submitted to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            _ -> do
                debug $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                emit signal2 uri
                io $ webPolicyDecisionIgnore decision
      `catchError` \e -> do
        error e
        io $ webPolicyDecisionUse decision

    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


attachNewWindow :: (ControlIO m, MonadLogger m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = liftBaseWith $ \runInIO -> gSync . on webView newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    runInIO . runExceptT . logErrors $ networkRequestGetUri request >>= emit signal
    webPolicyDecisionIgnore decision
    return True


-- attachResourceOpened :: (MonadIO m) => WebView -> Signal ResourceOpened -> m (ConnectId WebView)
-- attachResourceOpened webView signal = liftBaseWith $ \runInIO -> gSync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
--     action <- logErrors $ do
--         uri <- networkRequestGetUri request
--         debug $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ tshow uri ++ ">"
--         -- io . waitForResult =<<
--         emit signal (uri, mimetype)

--     debug "debug" $ "decision made: " ++ tshow action
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

attachZoomLevelChanged :: (ControlIO m, MonadLogger m) => WebView -> Signal ZoomLevelChanged -> m (ConnectId WebView)
attachZoomLevelChanged webView signal = liftBaseWith $ \runInIO -> gSync . on webView (notifyProperty webViewZoomLevel) . void . runInIO $ emit signal =<< get webView webViewZoomLevel


attachKeyPressed :: (ControlIO m, MonadLogger m) => WebView -> Signal KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = liftBaseWith $ \runInIO -> gSync . on webView keyPressEvent $ do
    modifiers <- Modifier . S.delete Gdk.Shift . S.fromList <$> Gdk.eventModifier
    key       <- KeyVal <$> Gdk.eventKeyVal

    io . runInIO . runFailT $ do
        guard . not $ isModifier key || isModalKey key
        emit signal $ modifiers .| key

    return False
