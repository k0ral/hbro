{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Gui.StatusBar
    ( StatusBar(..)
    , StatusBarReader
    , withStatusBar
    , getStatusBar
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

data StatusBarTag = StatusBarTag
type StatusBarReader m = MonadReader StatusBarTag StatusBar m

withStatusBar :: StatusBar -> ReaderT StatusBarTag StatusBar m a -> m a
withStatusBar = runReaderT StatusBarTag

getStatusBar :: (StatusBarReader m) => m StatusBar
getStatusBar = read StatusBarTag

instance GObjectClass StatusBar where
    toGObject (StatusBar h) = toGObject h
    unsafeCastGObject g     = StatusBar $ unsafeCastGObject g

instance WidgetClass StatusBar

-- | Name used in XML file to build status bar.
statusBarName :: String
statusBarName = "statusBox"
