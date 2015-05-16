{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Hbro.Gui (
-- * Basic
      initialize
-- * Getter
    , getMainWindow
    , canRender
    , getDOM
) where

-- {{{ Imports
import           Graphics.UI.Gtk.WebKit.Extended          ()

import           Hbro.Event
import           Hbro.Gui.Builder
import           Hbro.Gui.MainView                        hiding (initialize)
import qualified Hbro.Gui.MainView                        as MainView
import           Hbro.Gui.NotificationBar                 (NotificationBar)
import qualified Hbro.Gui.NotificationBar                 as NotifBar
import           Hbro.Gui.PromptBar                       (PromptBar, closedL)
import qualified Hbro.Gui.PromptBar                       as Prompt
import           Hbro.Gui.StatusBar
import           Hbro.Logger
import           Hbro.Prelude                             hiding (on)

import           Control.Lens.Getter
import           Control.Monad.Trans.Resource

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.Builder                  as Gtk
import           Graphics.UI.Gtk.General.General.Extended (gAsync, gSync)
import qualified Graphics.UI.Gtk.General.General.Extended as Gtk
import           Graphics.UI.Gtk.Windows.Window
import           System.Glib.Signals                      hiding (Signal)
-- }}}

initialize :: (ControlIO m, Alternative m, MonadCatch m, MonadThreadedLogger m, MonadResource m)
           => FilePath -> m (Gtk.Builder, MainView, PromptBar, StatusBar, NotificationBar)
initialize (pack -> file) = do
    debug $ "Building GUI from " ++ file
    builder <- gSync Gtk.builderNew
    gSync . Gtk.builderAddFromFile builder $ unpack file

    mainView   <- MainView.initialize =<< MainView.buildFrom builder
    mainWindow <- initializeWindow =<< getWidget builder "mainWindow"
    promptBar  <- Prompt.initialize =<< Prompt.buildFrom builder
    notifBar   <- NotifBar.initialize =<< NotifBar.buildFrom builder
    statusBar  <- StatusBar <$> getWidget builder "statusBox"

    let webView = mainView^.webViewL
    gAsync . widgetShowAll $ mainWindow
    Prompt.close promptBar

    gAsync $ windowSetDefault mainWindow (Just webView)
    addHandler (promptBar^.closedL) (const . gAsync $ widgetGrabFocus webView)

    -- io $ scrolledWindowSetPolicy (gui^.scrollWindowL) PolicyNever PolicyNever
    -- io $ G.set (gui^.scrollWindowL) [ scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyNever]

    return (builder, mainView, promptBar, statusBar, notifBar)

-- TODO: catch IOException
-- builderAddFromFile ::
-- builderAddFromFile builder file = catchGErrorJustDomain (Right <$> Gtk.builderAddFromFile builder file) handler
--   where
--     handler :: Gtk.BuilderError -> Text -> IO (Either Text a)
--     handler e message = return . Left $ "Error while building GUI from [" ++ file ++ "]: " ++ message

initializeWindow :: (MonadIO m) => Window -> m Window
initializeWindow window = do
  gAsync $ do
    widgetModifyBg window StateNormal (Color 0 0 5000)
    void . on window deleteEvent $ gAsync Gtk.mainQuit >> return False
  return window
