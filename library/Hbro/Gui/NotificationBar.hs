{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Gui.NotificationBar
    ( NotificationBar
    , buildFrom
    , asNotificationBar
    , initialize
    ) where

-- {{{ Imports
-- import Hbro.Error
import           Hbro.Gui.Builder
import           Hbro.Logger                     hiding (initialize)
import           Hbro.Prelude

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.Abstract.Misc
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
-- | A 'NotificationBar' can be manipulated as a 'Label'.
data NotificationBar = NotificationBar Label

-- | Useful to help the type checker
asNotificationBar :: NotificationBar -> NotificationBar
asNotificationBar = id

-- | A 'NotificationBar' can be built from an XML file.
buildFrom :: (MonadIO m, Functor m) => Gtk.Builder -> m NotificationBar
buildFrom builder = NotificationBar <$> getWidget builder "notificationLabel"

instance GObjectClass NotificationBar where
    toGObject (NotificationBar l) = toGObject l
    unsafeCastGObject = NotificationBar . unsafeCastGObject

instance WidgetClass NotificationBar
instance MiscClass NotificationBar
instance LabelClass NotificationBar
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

write :: (MonadIO m) => Text -> NotificationBar -> m NotificationBar
write message = write' message gray

write' :: (MonadIO m) => Text -> Color -> NotificationBar -> m NotificationBar
write' message color bar = do
    gAsync $ do
        labelSetAttributes bar [AttrForeground {paStart = 0, paEnd = -1, paColor = color}]
        labelSetText bar message
    return bar
