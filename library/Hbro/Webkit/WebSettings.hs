-- | Designed to be imported as @qualified@.
module Hbro.Webkit.WebSettings where

-- {{{ Imports
import           Hbro.Gui                           hiding (get)
import qualified Hbro.Gui                           as Gui (get)
import           Hbro.Logger
import           Hbro.Prelude

import           Graphics.UI.Gtk.WebKit.WebSettings
import           Graphics.UI.Gtk.WebKit.WebView

import           System.Glib.Attributes             (Attr, AttrOp (..))
import qualified System.Glib.Attributes             as G
-- }}}


set :: (BaseIO m, MonadReader t m, HasGUI t, Show a) => Attr WebSettings a -> a -> m ()
set element newValue = modify_ element $ const newValue

modify :: (BaseIO m, MonadReader t m, HasGUI t, Show a) => Attr WebSettings a -> (a -> a) -> m a
modify element modifier = do
    webView  <- Gui.get webViewL
    settings <- gSync $ webViewGetWebSettings webView
    oldValue <- gSync $ G.get settings element

    gAsync $ G.set settings [element := modifier oldValue]
    gAsync $ webViewSetWebSettings webView settings
    infoM "hbro.settings" $ "Set " ++ tshow element ++ " = " ++ tshow (modifier oldValue)
    return oldValue

-- | Same as 'modify', but discards the result
modify_ :: (BaseIO m, MonadReader t m, HasGUI t, Show a) => Attr WebSettings a -> (a -> a) -> m ()
modify_ e m = void $ modify e m

toggle :: (BaseIO m, MonadReader t m, HasGUI t) => Attr WebSettings Bool -> m Bool
toggle = (`modify` not)

-- | Same as 'toggle', but discards the result
toggle_ :: (BaseIO m, MonadReader t m, HasGUI t) => Attr WebSettings Bool -> m ()
toggle_ = (`modify_` not)

-- | Reset default settings
resetAll :: (BaseIO m, MonadReader t m, HasGUI t) => m ()
resetAll = do
    set webSettingsAutoLoadImages                    True
    set webSettingsAutoShrinkImages                  True
    set webSettingsEnableDefaultContextMenu          True
    set webSettingsDefaultEncoding                   (asText "utf8")
    set webSettingsEnableDeveloperExtras             False
    set webSettingsEnableDomPaste                    False
    set webSettingsEnableHtml5Database               False
    set webSettingsEnableHtml5LocalStorage           False
    set webSettingsEnableOfflineWebApplicationCache  False
    set webSettingsEnablePageCache                   True
    set webSettingsEnablePlugins                     False
    set webSettingsEnablePrivateBrowsing             False
    set webSettingsEnableScripts                     False
    set webSettingsEnableSpellChecking               False
    set webSettingsEnableSpatialNavigation           False
    set webSettingsEnableUniversalAccessFromFileUris True
    set webSettingsEnableSiteSpecificQuirks          False
    set webSettingsEnableXssAuditor                  False
    set webSettingsJSCanOpenWindowAuto               False
    set webSettingsMonospaceFontFamily               (asText "inconsolata")
    set webSettingsPrintBackgrounds                  True
    set webSettingsResizableTextAreas                True
    set webSettingsSpellCheckingLang                 (Nothing :: Maybe Text)
    set webSettingsTabKeyCyclesThroughElements       True
    set webSettingsUserStylesheetUri                 (Nothing :: Maybe Text)
    set webSettingsZoomStep                          0.1
