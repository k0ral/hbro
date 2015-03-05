{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbro.Gui.Builder
  ( FromGObject(..)
  , getWidget
  , getMainWindow
  ) where

-- {{{ Imports
import           Hbro.Prelude

import qualified Graphics.UI.Gtk.Builder                  as Gtk
import qualified Graphics.UI.Gtk.Display.Label            as Gtk
import qualified Graphics.UI.Gtk.Entry.Entry              as Gtk
import qualified Graphics.UI.Gtk.Layout.HBox              as Gtk
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as Gtk
import qualified Graphics.UI.Gtk.Windows.Window           as Gtk

import           System.Glib.Types
-- }}}


-- | UI elements that can be built from a @GtkBuilder@ object (that is: an XML file)
class (GObjectClass a) => FromGObject a where
  cast :: GObject -> a

instance FromGObject Gtk.Entry where cast = Gtk.castToEntry
instance FromGObject Gtk.HBox where cast = Gtk.castToHBox
instance FromGObject Gtk.Label where cast = Gtk.castToLabel
instance FromGObject Gtk.ScrolledWindow where cast = Gtk.castToScrolledWindow
instance FromGObject Gtk.Window where cast = Gtk.castToWindow

-- | Return the casted 'GObject' corresponding to the given name (set in the builder's XML file)
getWidget :: (MonadIO m, FromGObject a) => Gtk.Builder -> Text -> m a
getWidget builder name = gSync $ Gtk.builderGetObject builder cast name

getMainWindow :: (MonadIO m, MonadReader r m, Has Gtk.Builder r) => m Gtk.Window
getMainWindow = (`getWidget` "mainWindow") =<< ask
