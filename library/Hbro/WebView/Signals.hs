{-# LANGUAGE TemplateHaskell #-}
module Hbro.WebView.Signals where

-- {{{ Imports
import           Hbro.Error
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


data ResourceAction = Load | Download'

data Download       = Download URI Text (Maybe Int)
instance Describable Download where describe _ = "Download"

data LinkHovered    = LinkHovered URI (Maybe Text)
instance Describable LinkHovered where describe _ = "LinkHovered"

data LinkClicked    = LinkClicked URI MouseButton
instance Describable LinkClicked where describe _ = "LinkClicked"

data LoadRequested  = LoadRequested URI
instance Describable LoadRequested where describe _ = "LoadRequested"

data LoadStarted   = LoadStarted
instance Describable LoadStarted where describe _ = "LoadStarted"

data LoadFinished   = LoadFinished
instance Describable LoadFinished where describe _ = "LoadFinished"

data NewWindow      = NewWindow URI
instance Describable NewWindow where describe _ = "NewWindow"

data ResourceOpened = ResourceOpened URI Text
instance Describable ResourceOpened where describe _ = "ResourceOpened"

data TitleChanged   = TitleChanged Text
instance Describable TitleChanged where describe _ = "TitleChanged"


declareLenses [d|
  data Signals = Signals
    { downloadL       :: TMVar Download
    , keyPressedL     :: TMVar KeyPressed
    , linkClickedL    :: TMVar LinkClicked
    , linkHoveredL    :: TMVar LinkHovered
    , loadRequestedL  :: TMVar LoadRequested
    , loadStartedL    :: TMVar LoadStarted
    , loadFinishedL   :: TMVar LoadFinished
    -- newWebViewL        :: TMVar URI,
    , newWindowL      :: TMVar NewWindow
    , resourceOpenedL :: TMVar ResourceOpened
    , titleChangedL   :: TMVar TitleChanged
    }
  |]

initSignals :: (BaseIO m) => m Signals
initSignals = io (Signals <$> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO
                          <*> newEmptyTMVarIO)

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
    , attachResourceOpened    webView (signals^.resourceOpenedL)
    , attachTitleChanged      webView (signals^.titleChangedL)
    , attachKeyPressed        webView (signals^.keyPressedL)
    ]


attachDownload :: (BaseIO m) => WebView -> TMVar Download -> m (ConnectId WebView)
attachDownload webView signal = gSync . on webView downloadRequested $ \d -> do
    logErrors $ do
        amount <- io $ downloadGetTotalSize d
        uri    <- downloadGetUri d
        name   <- downloadGetSuggestedFilename d

        debugM "hbro.signals" $ "Requested download <" ++ tshow uri ++ ">"

        atomically . tryPutTMVar signal $ Download uri name (Just amount)
    return False


attachLinkHovered :: (BaseIO m) => WebView -> TMVar LinkHovered -> m (ConnectId WebView)
attachLinkHovered webView signal = gSync . on webView hoveringOverLink $ \title uri -> void . runMaybeT $ do
    debugM "hbro.signals" $ "Link hovered <" ++ tshow uri ++ ">"
    u <- MaybeT . return $ parseURI . unpack =<< uri

    atomically . tryPutTMVar signal $ LinkHovered u title


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (BaseIO m) => WebView -> TMVar NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = gSync . on webView createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        logErrors $ do
            uri <- networkRequestGetUri request
            debugM "hbro.signals" $ "New window <" ++ tshow uri ++ ">"
            atomically . tryPutTMVar signal $ NewWindow uri

        webPolicyDecisionIgnore decision
        return True

    return webView'


attachLoadStarted :: (BaseIO m) => WebView -> TMVar LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = gSync . on webView loadStarted $ \_frame -> do
    debugM "hbro.signals" "Load started"
    void . atomically $ tryPutTMVar signal LoadStarted


attachLoadFinished :: (BaseIO m) => WebView -> TMVar LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = gSync . on webView loadFinished $ \_frame -> do
    debugM "hbro.signals" "Load finished"
    void . atomically $ tryPutTMVar signal LoadFinished


attachNavigationRequest :: (BaseIO m) => WebView -> (TMVar LinkClicked, TMVar LoadRequested) -> m (ConnectId WebView)
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
                atomically . tryPutTMVar signal1 $ LinkClicked uri b
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
                void . atomically . tryPutTMVar signal2 $ LoadRequested uri
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


attachNewWindow :: (BaseIO m) => WebView -> TMVar NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = gSync . on webView newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    logErrors $ do
        uri <- networkRequestGetUri request
        debugM "hbro.signals" $ "New window request <" ++ tshow uri ++ ">"
        atomically . tryPutTMVar signal $ NewWindow uri

    webPolicyDecisionIgnore decision
    return True


attachResourceOpened :: (BaseIO m) => WebView -> TMVar ResourceOpened -> m (ConnectId WebView)
attachResourceOpened webView signal = gSync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
    logErrors $ do
        uri <- networkRequestGetUri request
        debugM "hbro.signals" $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ tshow uri ++ ">"
        atomically . tryPutTMVar signal $ ResourceOpened uri mimetype

    webPolicyDecisionUse decision
    return True


attachTitleChanged :: (BaseIO m) => WebView -> TMVar TitleChanged -> m (ConnectId WebView)
attachTitleChanged webView signal = gSync . on webView W.titleChanged $ \_frame title -> do
    debugM "hbro.signals" $ "Title changed to: " ++ title
    void . atomically . tryPutTMVar signal $ TitleChanged title


attachKeyPressed :: (BaseIO m) => WebView -> TMVar KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = gSync . on webView keyPressEvent $ do
    modifiers <- S.delete _Shift . S.fromList . map Keys.Modifier <$> Gdk.eventModifier
    key       <- KeyVal <$> Gdk.eventKeyVal

    io . runMaybeT $ do
        guard . not $ isModifier key || isModalKey key

        let theStroke = modifiers .| key
        debugM "hbro.signals" $ "Pressed: " ++ describe theStroke

        atomically . tryPutTMVar signal $ KeyPressed theStroke

    return False
