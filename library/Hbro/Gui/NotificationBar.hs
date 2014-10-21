{-# LANGUAGE TemplateHaskell #-}
module Hbro.Gui.NotificationBar (
-- * Type
      NotificationBar
    , HasNotificationBar(..)
    , initialize
) where

-- {{{ Imports
-- import Hbro.Error
import           Hbro.Gui.Buildable
import           Hbro.Logger                     hiding (initialize)
import           Hbro.Prelude

import           Control.Lens

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Builder
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
declareClassy [d|
  data NotificationBar = NotificationBar { labelL :: Label }
  |]

-- | A 'NotificationBar' can be built from an XML file.
instance Buildable NotificationBar where
    buildWith b = NotificationBar <$> gSync (builderGetObject b castToLabel $ asText "notificationLabel")

instance GObjectClass NotificationBar where
    toGObject (NotificationBar l) = toGObject l
    unsafeCastGObject = NotificationBar . unsafeCastGObject

-- | A 'NotificationBar' can be manipulated as a 'Widget'.
instance WidgetClass NotificationBar
-- }}}

get' :: (MonadReader r m, BaseIO m, HasNotificationBar r) => Lens' NotificationBar a -> m a
get' l = askL $ notificationBar.l

-- modify' :: (MonadReader r m, BaseIO m, HasNotificationBar r) => (Status -> Status) -> m ()
-- modify' f = io . atomically . (`modifyTVar` f) =<< asks (view _notificationBar)

initialize :: (BaseIO m) => NotificationBar -> m ()
initialize notifBar = io . updateGlobalLogger rootLoggerName $ addHandler (logHandler notifBar)


logHandler :: NotificationBar -> GenericHandler NotificationBar
logHandler notifBar = GenericHandler
    { priority  = INFO
    , formatter = simpleLogFormatter "$msg"
    , privData  = notifBar
    , writeFunc = \n t -> runReaderT (write t) n
    , closeFunc = \_ -> return ()
    }

write :: (BaseIO m, MonadReader r m, HasNotificationBar r) => String -> m ()
write text = do
    label  <- get' labelL

    gAsync $ do
        labelSetAttributes label [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]
        labelSetMarkup label text
