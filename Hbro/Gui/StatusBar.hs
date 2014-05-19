module Hbro.Gui.StatusBar where

-- {{{ Imports
import Hbro.Gui.Buildable
import Hbro.Util

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Layout.HBox

import System.Glib.Types
-- }}}

-- | Status bar is just a horizontal layout that can host widgets.
newtype StatusBar = StatusBar HBox

-- | A 'StatusBar' can be built from an XML file.
instance Buildable StatusBar where
    buildWith b = StatusBar <$> gSync (builderGetObject b castToHBox statusBarName)

instance GObjectClass StatusBar where
    toGObject (StatusBar h) = toGObject h
    unsafeCastGObject g     = StatusBar $ unsafeCastGObject g

-- instance ObjectClass StatusBar
instance WidgetClass StatusBar

-- | Name used in XML file to build status bar.
statusBarName :: String
statusBarName = "statusBox"
