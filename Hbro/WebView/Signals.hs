{-# LANGUAGE TemplateHaskell #-}
module Hbro.WebView.Signals where

-- {{{ Imports
import Hbro.Error
import Hbro.Gdk.KeyVal
import Hbro.Keys as Keys hiding(Hooks)
import Hbro.Keys.Model ((.|))
import Hbro.Keys.Signals as Keys
import Hbro.Util
import Hbro.Webkit.Lifted as Lifted

import Control.Concurrent.STM
import Control.Lens.Lens
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH

import Data.Set as S hiding(map)

import Graphics.UI.Gtk.Abstract.Widget hiding(KeyVal)
import Graphics.UI.Gtk.Gdk.EventM as Gdk
import Graphics.UI.Gtk.WebKit.Download as W hiding(Download, downloadGetUri, downloadGetSuggestedFilename)
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebView as W hiding(LoadFinished)

import Network.URI as N

import System.Glib.Signals hiding(Signal)
-- }}}


data ResourceAction = Load | Download'

data Download       = Download URI String (Maybe Int)
instance Show Download where show _ = "Download"

data LinkHovered    = LinkHovered URI (Maybe String)
instance Show LinkHovered where show _ = "LinkHovered"

data LinkClicked    = LinkClicked URI MouseButton
instance Show LinkClicked where show _ = "LinkClicked"

data LoadRequested  = LoadRequested URI
instance Show LoadRequested where show _ = "LoadRequested"

data LoadStarted   = LoadStarted
instance Show LoadStarted where show _ = "LoadStarted"

data LoadFinished   = LoadFinished
instance Show LoadFinished where show _ = "LoadFinished"

data NewWindow      = NewWindow URI
instance Show NewWindow where show _ = "NewWindow"

data ResourceOpened = ResourceOpened URI String
instance Show ResourceOpened where show _ = "ResourceOpened"

data TitleChanged   = TitleChanged String
instance Show TitleChanged where show _ = "TitleChanged"


data Signals = Signals
    { download          :: TMVar Download
    , keyPressed        :: TMVar KeyPressed
    , linkClicked       :: TMVar LinkClicked
    , linkHovered       :: TMVar LinkHovered
    , loadRequested     :: TMVar LoadRequested
    , loadStarted       :: TMVar LoadStarted
    , loadFinished      :: TMVar LoadFinished
    -- newWebView        :: TMVar URI,
    , newWindow         :: TMVar NewWindow
    , resourceOpened    :: TMVar ResourceOpened
    , titleChanged      :: TMVar TitleChanged
    }


makeLensesWith ?? ''Signals $ lensRules
    &  lensField .~ (\name -> Just (name ++ "L"))

initSignals :: (MonadBase IO m) => m Signals
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
attach :: (MonadBase IO m) => WebView -> Signals -> m ()
attach webView signals = void $ sequence
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


attachDownload :: (MonadBase IO m) => WebView -> TMVar Download -> m (ConnectId WebView)
attachDownload webView signal = gSync . on webView downloadRequested $ \d -> do
    logErrors $ do
        amount   <- io $ downloadGetTotalSize d
        uri      <- downloadGetUri d
        filename <- downloadGetSuggestedFilename d

        io . debugM "hbro.signals" $ "Requested download <" ++ show uri ++ ">"

        io . atomically . putTMVar signal $ Download uri filename (Just amount)
    return False


attachLinkHovered :: (MonadBase IO m) => WebView -> TMVar LinkHovered -> m (ConnectId WebView)
attachLinkHovered webView signal = gSync . on webView hoveringOverLink $ \title uri -> void . runMaybeT $ do
    io $ debugM "hbro.signals" $ "Link hovered <" ++ show uri ++ ">"
    u <- MaybeT . return $ parseURI =<< uri

    io . atomically . putTMVar signal $ LinkHovered u title


-- Triggered in 2 cases:
--  1/ Javascript window.open()
--  2/ Context menu "Open in new window"
attachNewWebView :: (MonadBase IO m) => WebView -> TMVar NewWindow -> m (ConnectId WebView)
attachNewWebView webView signal = gSync . on webView W.createWebView $ \_frame -> do
    webView' <- webViewNew

    on webView' webViewReady $ return True
    on webView' navigationPolicyDecisionRequested $ \_ request _ decision -> do
        logErrors $ do
            uri <- networkRequestGetUri request
            io . debugM "hbro.signals" $ "New window <" ++ show uri ++ ">"
            io . atomically . putTMVar signal $ NewWindow uri

        webPolicyDecisionIgnore decision
        return True

    return webView'


attachLoadStarted :: (MonadBase IO m) => WebView -> TMVar LoadStarted -> m (ConnectId WebView)
attachLoadStarted webView signal = gSync . on webView W.loadStarted $ \_frame -> do
    debugM "hbro.signals" "Load started"
    atomically $ putTMVar signal LoadStarted


attachLoadFinished :: (MonadBase IO m) => WebView -> TMVar LoadFinished -> m (ConnectId WebView)
attachLoadFinished webView signal = gSync . on webView W.loadFinished $ \_frame -> do
    debugM "hbro.signals" "Load finished"
    atomically $ putTMVar signal LoadFinished


attachNavigationRequest :: (MonadBase IO m) => WebView -> (TMVar LinkClicked, TMVar LoadRequested) -> m (ConnectId WebView)
attachNavigationRequest webView (signal1, signal2) = gSync . on webView W.navigationPolicyDecisionRequested $ \_frame request action decision -> do
    reason <- webNavigationActionGetReason action
    button <- toMouseButton <$> webNavigationActionGetButton action

    -- io . putStrLn . ("Request data: " ++) =<< networkRequestGetBody request
    -- io . putStrLn . ("Request type: " ++) . show =<< networkRequestGetContentType request
    -- io . putStrLn . ("Request type: " ++) . show =<< networkRequestGetURI request

    logErrors $ do
        uri <- networkRequestGetUri request

        case (reason, button) of
            (WebNavigationReasonLinkClicked, Just b) -> io $ do
                debugM "hbro.signals" $ "Link clicked <" ++ show uri ++ ">"
                atomically . putTMVar signal1 $ LinkClicked uri b
                webPolicyDecisionIgnore decision
            (WebNavigationReasonOther, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ show reason ++ "] to <" ++ show uri ++ ">"
                webPolicyDecisionUse decision
            (WebNavigationReasonBackForward, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ show reason ++ "] to <" ++ show uri ++ ">"
                webPolicyDecisionUse decision
            (WebNavigationReasonReload, _) -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ show reason ++ "] to <" ++ show uri ++ ">"
                webPolicyDecisionUse decision
            _ -> io $ do
                debugM "hbro.signals" $ "Navigation request [" ++ show reason ++ "] to <" ++ show uri ++ ">"
                atomically . putTMVar signal2 $ LoadRequested uri
                webPolicyDecisionIgnore decision
      `catchError` \e -> io $ do
        errorM "hbro.signals" $ show e
        webPolicyDecisionUse decision

    return True
  where
    toMouseButton 1 = Just LeftButton
    toMouseButton 2 = Just MiddleButton
    toMouseButton 3 = Just RightButton
    toMouseButton _ = Nothing


attachNewWindow :: (MonadBase IO m) => WebView -> TMVar NewWindow -> m (ConnectId WebView)
attachNewWindow webView signal = gSync . on webView W.newWindowPolicyDecisionRequested $ \_frame request _action decision -> do
    logErrors $ do
        uri <- networkRequestGetUri request
        io . debugM "hbro.signals" $ "New window request <" ++ show uri ++ ">"
        io . atomically . putTMVar signal $ NewWindow uri

    webPolicyDecisionIgnore decision
    return True


attachResourceOpened :: (MonadBase IO m) => WebView -> TMVar ResourceOpened -> m (ConnectId WebView)
attachResourceOpened webView signal = gSync . on webView W.mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> do
    logErrors $ do
        uri <- networkRequestGetUri request
        io . debugM "hbro.signals" $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ show uri ++ ">"
        io . atomically . putTMVar signal $ ResourceOpened uri mimetype

    webPolicyDecisionUse decision
    return True


attachTitleChanged :: (MonadBase IO m) => WebView -> TMVar TitleChanged -> m (ConnectId WebView)
attachTitleChanged webView signal = gSync . on webView W.titleChanged $ \_frame title -> do
    debugM "hbro.signals" $ "Title changed to: " ++ title
    atomically . putTMVar signal $ TitleChanged title


attachKeyPressed :: (MonadBase IO m) => WebView -> TMVar KeyPressed -> m (ConnectId WebView)
attachKeyPressed webView signal = gSync . on webView keyPressEvent $ do
    modifiers <- S.delete _Shift . S.fromList . map Keys.Modifier <$> Gdk.eventModifier
    keyVal    <- KeyVal <$> Gdk.eventKeyVal

    io . runMaybeT $ do
        guard . not $ isModifier keyVal || isModalKey keyVal

        let theStroke = modifiers .| keyVal
        io . debugM "hbro.signals" $ "Pressed: " ++ show theStroke

        io . atomically . tryPutTMVar signal $ KeyPressed theStroke

    return False
