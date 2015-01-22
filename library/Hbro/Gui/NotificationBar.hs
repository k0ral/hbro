{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Gui.NotificationBar (
-- * Type
      NotificationBar
    , NotifBarTag(..)
    , NotificationBarReader
    , buildFrom
    , getNotificationBar
    , initialize
) where

-- {{{ Imports
-- import Hbro.Error
import           Hbro.Gui.Builder
import           Hbro.Logger                     hiding (initialize)
import           Hbro.Prelude

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.Abstract.Widget
import qualified Graphics.UI.Gtk.Builder         as Gtk
import           Graphics.UI.Gtk.Display.Label

import           System.Glib.Types
import           System.Log.Formatter
import           System.Log.Handler.Simple
import           System.Log.Logger               (addHandler, rootLoggerName,
                                                  updateGlobalLogger)
-- }}}

-- TODO: color notifications depending on their level
-- TODO: make it possible to expand the notification bar to display the last N log lines
-- TODO: make it possible to change the log level

-- {{{ Types
data NotificationBar = NotificationBar Label

data NotifBarTag = NotifBarTag
type NotificationBarReader m = MonadReader NotifBarTag NotificationBar m

getNotificationBar :: (NotificationBarReader m) => m NotificationBar
getNotificationBar = read NotifBarTag

-- | A 'NotificationBar' can be built from an XML file.
buildFrom :: (MonadIO m, Functor m) => Gtk.Builder -> m NotificationBar
buildFrom builder = NotificationBar <$> getWidget builder "notificationLabel"

instance GObjectClass NotificationBar where
    toGObject (NotificationBar l) = toGObject l
    unsafeCastGObject = NotificationBar . unsafeCastGObject

-- | A 'NotificationBar' can be manipulated as a 'Widget'.
instance WidgetClass NotificationBar
-- }}}

initialize :: (MonadIO m) => NotificationBar -> m NotificationBar
initialize notifBar = do
  io . updateGlobalLogger rootLoggerName $ addHandler (logHandler notifBar)
  return notifBar


logHandler :: NotificationBar -> GenericHandler NotificationBar
logHandler notifBar = GenericHandler
    { priority  = INFO
    , formatter = simpleLogFormatter "$msg"
    , privData  = notifBar
    , writeFunc = \n t -> runReaderT NotifBarTag n (write t)
    , closeFunc = \_ -> return ()
    }

write :: (MonadIO m, MonadReader NotifBarTag NotificationBar m) => String -> m ()
write text = do
    (NotificationBar label) <- getNotificationBar

    gAsync $ do
        labelSetAttributes label [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]
        labelSetMarkup label text
