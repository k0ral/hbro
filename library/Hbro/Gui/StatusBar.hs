{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Gui.StatusBar
    ( StatusBar(..)
    , asStatusBar
    , statusBarName
    ) where

-- {{{ Imports
import           Hbro.Prelude

import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Layout.HBox

import           System.Glib.Types
-- }}}

-- | Status bar is just a horizontal layout that can host widgets.
newtype StatusBar = StatusBar HBox

asStatusBar :: StatusBar -> StatusBar
asStatusBar = id

instance GObjectClass StatusBar where
    toGObject (StatusBar h) = toGObject h
    unsafeCastGObject g     = StatusBar $ unsafeCastGObject g

instance WidgetClass StatusBar

-- | Name used in XML file to build status bar.
statusBarName :: String
statusBarName = "statusBox"
