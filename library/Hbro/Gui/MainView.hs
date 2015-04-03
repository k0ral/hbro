{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Hbro.Gui.MainView
  ( MainView
  , scrollWindowL
  , webViewL
  , downloadHookL
  , keyPressedHookL
  , linkClickedHookL
  , linkHoveredHookL
  , linkUnhoveredHookL
  , loadCommittedHookL
  , loadFailedHookL
  , loadFinishedHookL
  , loadRequestedHookL
  , loadStartedHookL
  , newWindowHookL
  , progressChangedHookL
  , scrolledHookL
  , titleChangedHookL
  , zoomLevelChangedHookL
  , Axis(..)
  , Position(..)
  , getWebView
  , getWebSettings
  , getDOM
  , getAdjustment
  , Scrolled(..)
  , buildFrom
  , initialize
  , canRender
  , render
  , zoomIn
  , zoomOut
  , scrollH
  , scrollV
  ) where

-- {{{ Imports
import           Hbro.Event
import           Hbro.Gui.Builder
import           Hbro.Keys                                as Keys
import           Hbro.Logger
import           Hbro.Prelude                             hiding (on)
import           Hbro.WebView.Signals

import           Control.Lens                             hiding (set, snoc)

import           Data.Text                                (splitOn)

import qualified Graphics.UI.Gtk.Abstract.Container       as Gtk
import           Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.Builder                  as Gtk
import           Graphics.UI.Gtk.General.General.Extended
import qualified Graphics.UI.Gtk.Misc.Adjustment          as Gtk
import           Graphics.UI.Gtk.Scrolling.ScrolledWindow
import           Graphics.UI.Gtk.WebKit.DOM.Document
import           Graphics.UI.Gtk.WebKit.Lifted.WebView    hiding
                                                           (LoadStatus (..))
import           Graphics.UI.Gtk.WebKit.NetworkRequest
import           Graphics.UI.Gtk.WebKit.WebPolicyDecision
import           Graphics.UI.Gtk.WebKit.WebSettings

import           Network.URI

import           System.Glib.Attributes.Extended
import           System.Glib.Signals                      hiding (Signal)
-- }}}

-- * Types
data Scrolled = Scrolled deriving(Show)
instance Event Scrolled where
  describeInput _ _ = Just "Scrolled"

declareLenses [d|
  data MainView = MainView
    { scrollWindowL         :: ScrolledWindow  -- ^ 'ScrolledWindow' containing the webview
    , webViewL              :: WebView
    , downloadHookL         :: Signal Download
    , keyPressedHookL       :: Signal KeyPressed
    , linkClickedHookL      :: Signal LinkClicked
    , linkHoveredHookL      :: Signal LinkHovered
    , linkUnhoveredHookL    :: Signal LinkUnhovered
    , loadCommittedHookL    :: Signal LoadCommitted
    , loadFailedHookL       :: Signal LoadFailed
    , loadFinishedHookL     :: Signal LoadFinished
    , loadRequestedHookL    :: Signal LoadRequested
    , loadStartedHookL      :: Signal LoadStarted
    , newWindowHookL        :: Signal NewWindow
    , progressChangedHookL  :: Signal ProgressChanged
    -- , resourceOpenedHookL   :: Signal ResourceOpened
    , scrolledHookL         :: Signal Scrolled
    , titleChangedHookL     :: Signal TitleChanged
    , zoomLevelChangedHookL :: Signal ZoomLevelChanged
    }
  |]


-- * Commonly used getters
getWebView :: (MonadReader r m, Has MainView r) => m WebView
getWebView = asks $ view webViewL

getWebSettings :: (MonadIO m, MonadReader r m, Has MainView r) => m WebSettings
getWebSettings = gSync . webViewGetWebSettings =<< asks (view webViewL)

getDOM :: (MonadIO m, MonadReader r m, Has MainView r) => m (Maybe Document)
getDOM = gSync . webViewGetDomDocument =<< asks (view webViewL)

getAdjustment :: (MonadIO m) => Axis -> ScrolledWindow -> m Gtk.Adjustment
getAdjustment Horizontal = gSync . scrolledWindowGetHAdjustment
getAdjustment Vertical   = gSync . scrolledWindowGetVAdjustment


-- * Others
data Axis     = Horizontal | Vertical deriving(Show)
data Position = Absolute Double | Relative Double deriving(Show)

buildFrom :: (BaseIO m) => Gtk.Builder -> m MainView
buildFrom builder = do
    sWindow <- getWidget builder "webViewParent"
    webView <- gSync webViewNew

    gAsync $ Gtk.containerAdd sWindow webView
    MainView <$> pure sWindow
             <*> pure webView
             <*> newSignal Download
             <*> newSignal KeyPressed
             <*> newSignal LinkClicked
             <*> newSignal LinkHovered
             <*> newSignal LinkUnhovered
             <*> newSignal LoadCommitted
             <*> newSignal LoadFailed
             <*> newSignal LoadFinished
             <*> newSignal LoadRequested
             <*> newSignal LoadStarted
             <*> newSignal NewWindow
             <*> newSignal ProgressChanged
             -- <*> newSignal ResourceOpened
             <*> newSignal Scrolled
             <*> newSignal TitleChanged
             <*> newSignal ZoomLevelChanged


initialize :: (ControlIO m, MonadLogger m) => MainView -> m MainView
initialize mainView = do
  set webView widgetCanDefault True
  -- set webView webViewSetMaintainsBackForwardList False
  gAsync . on webView closeWebView $ gAsync mainQuit >> return False
  gAsync . on webView consoleMessage $ \a b n c -> do
      putStrLn "console message"
      putStrLn $ unlines [a, b, tshow n, c]
      return True

  gAsync . on webView mimeTypePolicyDecisionRequested $ \_frame request mimetype decision -> io $ do
    uri <- networkRequestGetUri request :: IO (Maybe Text)
    -- debug $ "Opening resource [MIME type=" ++ mimetype ++ "] at <" ++ tshow uri ++ ">"
    renderable <- webViewCanShowMimeType webView (asText mimetype)
    case (uri, renderable) of
      (Just _, True) -> webPolicyDecisionUse decision
      (Just _, _) -> webPolicyDecisionDownload decision
      _ -> webPolicyDecisionIgnore decision
    return True

  -- void . on webView resourceRequestStarting $ \frame resource request response -> do
  --     uri <- webResourceGetUri resource
  --     putStrLn $ "resource request starting: " ++ uri
  --     -- print =<< webResourceGetData resource
  --     putStrLn =<< (maybe (return "No request") (return . ("Request URI: " ++) . show <=< W.networkRequestGetUri) request)
  --     putStrLn =<< (maybe (return "No response") (return . ("Response URI: " ++) . show <=< networkResponseGetUri) response)

  --     -- case (endswith ".css" uri || uri `endswith` ".png" || uri `endswith` ".ico") of
  --        -- True -> (putStrLn "OK")
  --     (maybe (return ()) (`networkRequestSetUri` "about:blank") request)

  attachDownload          webView  $ mainView^.downloadHookL
  attachKeyPressed        webView  $ mainView^.keyPressedHookL
  attachLinkHovered       webView (mainView^.linkHoveredHookL) (mainView^.linkUnhoveredHookL)
  attachLoadCommitted     webView  $ mainView^.loadCommittedHookL
  attachLoadFailed        webView  $ mainView^.loadFailedHookL
  attachLoadFinished      webView  $ mainView^.loadFinishedHookL
  attachLoadStarted       webView  $ mainView^.loadStartedHookL
  attachNavigationRequest webView (mainView^.linkClickedHookL, mainView^.loadRequestedHookL)
  attachNewWebView        webView  $ mainView^.newWindowHookL
  attachNewWindow         webView  $ mainView^.newWindowHookL
  attachProgressChanged   webView  $ mainView^.progressChangedHookL
  -- attachResourceOpened    webView (mainView^.resourceOpenedHook)
  attachScrolled          mainView $ mainView^.scrolledHookL
  attachTitleChanged      webView  $ mainView^.titleChangedHookL
  attachZoomLevelChanged  webView  $ mainView^.zoomLevelChangedHookL

  initSettings webView

  return mainView
  where webView = mainView^.webViewL

canRender :: (MonadIO m, MonadReader r m, Has MainView r) => Text -> m Bool
canRender mimetype = gSync . (`webViewCanShowMimeType` mimetype) =<< asks (view webViewL)


render :: (MonadReader r m, Has MainView r, MonadIO m, MonadLogger m) => Text -> URI -> m ()
render page uri = do
    debug $ "Rendering <" ++ tshow uri ++ ">"
    -- loadString page uri =<< get' webViewL

    -- debug $ "Base URI: " ++ show (baseOf uri)

    loadString page (baseOf uri) =<< asks (view webViewL)
  where
    baseOf uri' = uri' {
        uriPath = unpack . (`snoc` '/') . intercalate "/" . initSafe . splitOn "/" . pack $ uriPath uri'
    }


-- | Set default settings
initSettings :: (MonadIO m, MonadLogger m, Functor m) => WebView -> m WebView
initSettings webView = do
    s <- gSync $ webViewGetWebSettings webView

    set s webSettingsAutoLoadImages                    True
    set s webSettingsAutoShrinkImages                  True
    set s webSettingsEnableDefaultContextMenu          True
    set s webSettingsDefaultEncoding                   (asText "utf8")
    set s webSettingsEnableDeveloperExtras             False
    set s webSettingsEnableDomPaste                    False
    set s webSettingsEnableHtml5Database               False
    set s webSettingsEnableHtml5LocalStorage           False
    set s webSettingsEnableOfflineWebApplicationCache  False
    set s webSettingsEnablePageCache                   True
    set s webSettingsEnablePlugins                     False
    set s webSettingsEnablePrivateBrowsing             False
    set s webSettingsEnableScripts                     False
    set s webSettingsEnableSpellChecking               False
    set s webSettingsEnableSpatialNavigation           False
    set s webSettingsEnableUniversalAccessFromFileUris True
    set s webSettingsEnableSiteSpecificQuirks          False
    set s webSettingsEnableXssAuditor                  False
    set s webSettingsJSCanOpenWindowAuto               False
    set s webSettingsMonospaceFontFamily               (asText "inconsolata")
    set s webSettingsPrintBackgrounds                  True
    set s webSettingsResizableTextAreas                True
    set s webSettingsSpellCheckingLang                 (Nothing :: Maybe Text)
    set s webSettingsTabKeyCyclesThroughElements       True
    set s webSettingsUserStylesheetUri                 (Nothing :: Maybe Text)
    set s webSettingsZoomStep                          0.1

    return webView


zoomIn, zoomOut :: (MonadIO m, Functor m, MonadReader r m, Has MainView r) => m ()
zoomIn  = getWebView >>= gAsync . webViewZoomIn
zoomOut = getWebView >>= gAsync . webViewZoomOut

-- | Shortcut to 'scroll' horizontally or vertically.
scrollH, scrollV :: (MonadIO m, Functor m, MonadLogger m, MonadReader r m, Has MainView r) => Position -> m ()
scrollH p = void . scroll Horizontal p =<< ask
scrollV p = void . scroll Vertical p =<< ask

-- | General scrolling command
scroll :: (MonadIO m, MonadLogger m) => Axis -> Position -> MainView -> m MainView
scroll axis percentage mainView = do
     debug $ "Set scroll " ++ tshow axis ++ " = " ++ tshow percentage

     adj     <- getAdjustment axis $ mainView^.scrollWindowL
     page    <- get adj Gtk.adjustmentPageSize
     current <- get adj Gtk.adjustmentValue
     lower   <- get adj Gtk.adjustmentLower
     upper   <- get adj Gtk.adjustmentUpper

     let shift (Absolute x) = lower   + x/100 * (upper - page - lower)
         shift (Relative x) = current + x/100 * page
         limit x            = (x `max` lower) `min` (upper - page)

     set adj Gtk.adjustmentValue $ limit (shift percentage)
     return mainView


attachScrolled :: (ControlIO m, MonadLogger m) => MainView -> Signal Scrolled -> m (ConnectId Gtk.Adjustment)
attachScrolled mainView signal = do
  adjustment <- getAdjustment Vertical $ mainView^.scrollWindowL
  liftBaseWith $ \runInIO -> gSync . Gtk.onValueChanged adjustment . void . runInIO $ emit signal ()
