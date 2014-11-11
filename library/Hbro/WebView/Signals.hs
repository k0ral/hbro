{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Hbro.WebView.Signals where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Keys                                  as Keys hiding
                                                                     (Hooks)
import           Hbro.Keys.Model                            ((.|))
import           Hbro.Keys.Signals                          as Keys
import           Hbro.Logger
import           Hbro.Prelude                               hiding (on)

import           Graphics.UI.Gtk.WebKit.Lifted              as Lifted

import           Control.Lens.Getter
import           Control.Lens.TH

import           Data.Set                                   as S hiding (map)

import           Graphics.UI.Gtk.Abstract.Widget            hiding (KeyVal)
import           Graphics.UI.Gtk.Gdk.EventM                 as Gdk
import           Graphics.UI.Gtk.WebKit.Download            as W hiding
                                                                  (Download, downloadGetSuggestedFilename, downloadGetUri)
import           Graphics.UI.Gtk.WebKit.WebNavigationAction
import           Graphics.UI.Gtk.WebKit.WebPolicyDecision
import           Graphics.UI.Gtk.WebKit.WebView             as W hiding
                                                                  (LoadFinished)

import           Network.URI

import           System.Glib.Signals                        hiding (Signal)
-- }}}

data Download = Download deriving(Show)
instance Event Download where
  type Input Download = (URI, Text, Maybe Int)

data LinkHovered = LinkHovered deriving(Show)
instance Event LinkHovered where
  type Input LinkHovered = (URI, Maybe Text)

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

data ResourceAction = Load | Download' deriving(Show)
instance Describable ResourceAction where describe = tshow


declareLenses [d|
  data Signals = Signals
    { downloadL       :: Signal Download
    , keyPressedL     :: Signal KeyPressed
    , linkClickedL    :: Signal LinkClicked
    , linkHoveredL    :: Signal LinkHovered
    , loadRequestedL  :: Signal LoadRequested
    , loadStartedL    :: Signal LoadStarted
    , loadFinishedL   :: Signal LoadFinished
    -- newWebViewL        :: TQueue URI,
    , newWindowL      :: Signal NewWindow
    , resourceOpenedL :: Signal ResourceOpened
    , titleChangedL   :: Signal TitleChanged
    }
  |]

initSignals :: (BaseIO m) => m Signals
initSignals = Signals <$> newSignal Download
                      <*> newSignal KeyPressed
                      <*> newSignal LinkClicked
                      <*> newSignal LinkHovered
                      <*> newSignal LoadRequested
                      <*> newSignal LoadStarted
                      <*> newSignal LoadFinished
                      <*> newSignal NewWindow
                      <*> newSignal ResourceOpened
                      <*> newSignal TitleChanged

-- | Sequentially bind all signals.
attach :: (BaseIO m) => WebView -> Signals -> m ()
attach webView signals = sequence_
    [ attachDownload          webView (signals^.downloadL)
    , attachLinkHovered       webView (signals^.linkHoveredL)
    , attachLoadStarted       webView (signals^.loadStartedL)
    , attachLoadFinished      webView (signals^.loadFinishedL)
    , attachNavigationRequest webView (signals^.linkClickedL, signals^.loadRequestedL)
    , attachNewWebView        webView (signals^.newWindowL)
    , attachNewWindow         webView (signals^.newWindowL)
    -- , attachResourceOpened    webView (signals^.resourceOpenedL)
    , attachTitleChanged      webView (signals^.titleChangedL)
    , attachKeyPressed        webView (signals^.keyPressedL)
    ]


attachDownload :: (BaseIO m) => WebView -> Signal Download -> m (ConnectId WebView)
attachDownload webView signal = gSync . on webView downloadRequested $ \d -> do
    logErrors $ do
        amount <- io $ downloadGetTotalSize d
        uri    <- downloadGetUri d
        name   <- downloadGetSuggestedFilename d

        debugM "hbro.signals" $ "Requested download <" ++ tshow uri ++ ">"

        emit signal (uri, name, Just amount)
    return False


attachLinkHovered :: (BaseIO m) => WebView -> Signal LinkHovered -> m (ConnectId WebView)
attachLinkHovered webView signal = gSync . on webView hoveringOverLink $ \title uri -> void . runMaybeT $ do
    debugM "hbro.signals" $ "Link hovered <" ++ tshow uri ++ ">"
    u <- MaybeT . return $ parseURI . unpack =<< uri

    emit signal (u, title)


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (BaseIO m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = gSync . on webView createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        logErrors $ do
            uri <- networkRequestGetUri request
            debugM "hbro.signals" $ "New window <" ++ tshow uri ++ ">"
            emit signal uri

        webPolicyDecisionIgnore decision
        return True

    return webView'


attachLoadStarted :: (BaseIO m) => WebView -> Signal LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = gSync . on webView loadStarted $ \_frame -> do
    debugM "hbro.signals" "Load started"
    emit signal ()


attachLoadFinished :: (BaseIO m) => WebView -> Signal LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = gSync . on webView loadFinished $ \_frame -> do
    debugM "hbro.signals" "Load finished"
    emit signal ()


attachNavigationRequest :: (BaseIO m) => WebView -> (Signal LinkClicked, Signal LoadRequested) -> m (ConnectId WebView)
attachNavigationRequest webView (signal1, signal2) = gSync . on webView navigationPolicyDecisionRequested $ \_frame request action decision -> do
    reason <- webNavigationActionGetReason action
    button <- toMouseButton <$> webNavigationActionGetButton action

    -- io . putStrLn . ("Request data: " ++) =<< networkRequestGetBody request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetContentType request
    -- io . putStrLn . ("Request type: " ++) . describe =<< networkRequestGetURI request

    logErrors $ do
        uri <- networkRequestGetUri request

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> io $ do
                debugM "hbro.signals" $ "Link clicked <" ++ tshow uri ++ ">"
                emit signal1 (uri, b)
                webPolicyDecisionIgnore decision
            (WebNavigationReasonOther, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                webPolicyDecisionUse decision
            (WebNavigationReasonBackForward, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                webPolicyDecisionUse decision
            (WebNavigationReasonReload, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                webPolicyDecisionUse decision
            (WebNavigationReasonFormSubmitted, _) -> io $ do
                debugM "hbro.signals" $ "Form submitted to <" ++ tshow uri ++ ">"
                webPolicyDecisionUse decision
            _ -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ tshow reason ++ "] to <" ++ tshow uri ++ ">"
                emit signal2 uri
                webPolicyDecisionIgnore decision
      `catchError` \e -> io $ do
        errorM "hbro.signals" $ tshow e
        webPolicyDecisionUse decision

    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


attachNewWindow :: (BaseIO m) => WebView -> Signal NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = gSync . on webView newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    logErrors $ do
        uri <- networkRequestGetUri request
        debugM "hbro.signals" $ "New window request <" ++ tshow uri ++ ">"
        emit signal uri

    webPolicyDecisionIgnore decision
    return True


-- attachResourceOpened :: (BaseIO m) => WebView -> Signal ResourceOpened -> m (ConnectId WebView)
-- attachResourceOpened webView signal = gSync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
--     action <- logErrors $ do
--         uri <- networkRequestGetUri request
--         debugM "hbro.signals" $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ tshow uri ++ ">"
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



attachTitleChanged :: (BaseIO m) => WebView -> Signal TitleChanged -> m (ConnectId WebView)
attachTitleChanged webView signal = gSync . on webView W.titleChanged $ \_frame title -> do
    debugM "hbro.signals" $ "Title changed to: " ++ title
    void $ emit signal title


attachKeyPressed :: (BaseIO m) => WebView -> Signal KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = gSync . on webView keyPressEvent $ do
    modifiers <- S.delete _Shift . S.fromList . map Keys.Modifier <$> Gdk.eventModifier
    key       <- KeyVal <$> Gdk.eventKeyVal

    io . runMaybeT $ do
        guard . not $ isModifier key || isModalKey key

        let theStroke = modifiers .| key
        debugM "hbro.signals" $ "Pressed: " ++ describe theStroke

        emit signal theStroke

    return False
