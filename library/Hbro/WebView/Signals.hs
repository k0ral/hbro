{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Hbro.WebView.Signals where

-- {{{ Imports
import           Hbro.Attributes
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

import           System.Glib.Signals                        hiding (Signal)
-- }}}

data Download = Download deriving(Show)
instance Event Download where
  type Input Download = (URI, Text, Maybe Int)

data LinkHovered = LinkHovered deriving(Show)
instance Event LinkHovered where
  type Input LinkHovered = (URI, Maybe Text)

data LinkUnhovered = LinkUnhovered deriving(Show)
instance Event LinkUnhovered

data LinkClicked = LinkClicked deriving(Show)
instance Event LinkClicked where
  type Input LinkClicked = (URI, MouseButton)

data LoadRequested = LoadRequested deriving(Show)
instance Event LoadRequested where
  type Input LoadRequested = URI

data LoadStarted = LoadStarted deriving(Show)
instance Event LoadStarted

data LoadFinished = LoadFinished deriving(Show)
instance Event LoadFinished

data NewWindow = NewWindow deriving(Show)
instance Event NewWindow where
  type Input NewWindow = URI

data ResourceOpened = ResourceOpened deriving(Show)
instance Event ResourceOpened where
  type Input ResourceOpened = (URI, Text)

data TitleChanged = TitleChanged deriving(Show)
instance Event TitleChanged where
  type Input TitleChanged = Text

data ZoomLevelChanged = ZoomLevelChanged deriving(Show)
instance Event ZoomLevelChanged where
  type Input ZoomLevelChanged = Float

data ResourceAction = Load | Download' deriving(Show)
instance Describable ResourceAction where describe = tshow


attachDownload :: (MonadIO m) => WebView -> Signal Download -> m (ConnectId WebView)
attachDownload webView signal = gSync . on webView downloadRequested $ \d -> do
    runErrorT . logErrors $ do
        amount <- io $ downloadGetTotalSize d
        uri    <- downloadGetUri d
        name   <- downloadGetSuggestedFilename d

        debugM $ "Requested download <" ++ tshow uri ++ ">"

        emit signal (uri, name, Just amount)
    return False


attachLinkHovered :: (MonadIO m) => WebView -> Signal LinkHovered -> Signal LinkUnhovered -> m (ConnectId WebView)
attachLinkHovered webView hoveredSignal unhoveredSignal = gSync $ on webView hoveringOverLink callback
  where callback title (Just uri) = void . runErrorT . logErrors $ do
          debugM $ "Link hovered <" ++ tshow uri ++ ">"
          u <- parseURIM $ pack uri

          emit hoveredSignal (u, pack <$> title)
        callback _ _ = emit unhoveredSignal ()


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (MonadIO m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = gSync . on webView createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        runErrorT . logErrors $ do
            uri <- networkRequestGetUri request
            debugM $ "New window <" ++ tshow uri ++ ">"
            emit signal uri

        webPolicyDecisionIgnore decision
        return True

    return webView'


attachLoadStarted :: (MonadIO m) => WebView -> Signal LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = gSync . on webView loadStarted $ \_frame -> emit signal ()

attachLoadFinished :: (MonadIO m) => WebView -> Signal LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = gSync . on webView loadFinished $ \_frame -> emit signal ()


attachNavigationRequest :: (MonadIO m) => WebView -> (Signal LinkClicked, Signal LoadRequested) -> m (ConnectId WebView)
attachNavigationRequest webView (signal1, signal2) = gSync . on webView navigationPolicyDecisionRequested $ \_frame request action decision -> do
    reason <- webNavigationActionGetReason action
    button <- toMouseButton <$> webNavigationActionGetButton action

    -- io . putStrLn . ("Request data: " ++) =<< networkRequestGetBody request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetContentType request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetURI request

    runErrorT . logErrors $ do
        uri <- networkRequestGetUri request

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> do
                debugM $ "Link clicked <" ++ tshow uri ++ ">"
                emit signal1 (uri, b)
                io $ webPolicyDecisionIgnore decision
            (WebNavigationReasonOther, _) -> do
                debugM $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonBackForward, _) -> do
                debugM $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonReload, _) -> do
                debugM $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            (WebNavigationReasonFormSubmitted, _) -> do
                debugM $ "Form submitted to <" ++ tshow uri ++ ">"
                io $ webPolicyDecisionUse decision
            _ -> do
                debugM $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                emit signal2 uri
                io $ webPolicyDecisionIgnore decision
      `catchError` \e -> do
        errorM $ asText e
        io $ webPolicyDecisionUse decision

    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


attachNewWindow :: (MonadIO m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = gSync . on webView newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    runErrorT . logErrors $ do
        uri <- networkRequestGetUri request
        debugM $ "New window request <" ++ tshow uri ++ ">"
        emit signal uri

    webPolicyDecisionIgnore decision
    return True


-- attachResourceOpened :: (MonadIO m) => WebView -> Signal ResourceOpened -> m (ConnectId WebView)
-- attachResourceOpened webView signal = gSync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
--     action <- logErrors $ do
--         uri <- networkRequestGetUri request
--         debugM $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ tshow uri ++ ">"
--         -- io . waitForResult =<<
--         emit signal (uri, mimetype)

--     debugM "debug" $ "decision made: " ++ tshow action
--     case action of
--         Just Load -> webPolicyDecisionUse decision
--         Just Download' -> webPolicyDecisionDownload decision
--         _ -> webPolicyDecisionIgnore decision
--     return True

-- waitForResult output = do
--   debugM "hbro.debug" "iteration" >> mainIteration
--   maybe (waitForResult output) return =<< (atomically $ tryTakeTMVar output)


attachTitleChanged :: (MonadIO m) => WebView -> Signal TitleChanged -> m (ConnectId WebView)
attachTitleChanged webView signal = gSync . on webView W.titleChanged $ \_frame title -> do
    debugM $ "Title changed to: " ++ title
    void $ emit signal title

attachZoomLevelChanged :: (MonadIO m) => WebView -> Signal ZoomLevelChanged -> m (ConnectId WebView)
attachZoomLevelChanged webView signal = gSync . on webView (notifyProperty webViewZoomLevel) $ do
    value <- get webView webViewZoomLevel
    debugM $ "Zoom level changed to: " ++ tshow value
    emit signal value


attachKeyPressed :: (MonadIO m) => WebView -> Signal KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = gSync . on webView keyPressEvent $ do
    modifiers <- Modifier . S.delete Gdk.Shift . S.fromList <$> Gdk.eventModifier
    key       <- KeyVal <$> Gdk.eventKeyVal

    io . runFailT $ do
        guard . not $ isModifier key || isModalKey key

        let theStroke = modifiers .| key
        debugM $ "Pressed: " ++ describe theStroke

        emit signal theStroke

    return False
