{-# LANGUAGE TemplateHaskell #-}
module Hbro.Gui.NotificationBar (
-- * Type
      NotificationBar
    , HasNotificationBar(..)
    , initialize
) where

-- {{{ Imports
-- import Hbro.Error
import Hbro.Gui.Buildable
import Hbro.Util

import Control.Lens
import Control.Monad.Reader

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label

import System.Glib.Types
import System.Log.Formatter
import System.Log.Handler.Simple
-- }}}

-- TODO: color notifications depending on their level
-- TODO: make it possible to expand the notification bar to display the last N log lines
-- TODO: make it possible to change the log level

-- {{{ Types
newtype NotificationBar = NotificationBar { _label :: Label }

makeLensesWith ?? ''NotificationBar $ classyRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
    & lensClass .~ (\name -> Just ("Has" ++ name, "_" ++ map toLower name))

-- | A 'NotificationBar' can be built from an XML file.
instance Buildable NotificationBar where
    buildWith b = NotificationBar <$> gSync (builderGetObject b castToLabel "notificationLabel")

instance GObjectClass NotificationBar where
    toGObject (NotificationBar l) = toGObject l
    unsafeCastGObject = NotificationBar . unsafeCastGObject

-- | A 'NotificationBar' can be manipulated as a 'Widget'.
instance WidgetClass NotificationBar
-- }}}

get' :: (MonadReader r m, MonadBase IO m, HasNotificationBar r) => Lens' NotificationBar a -> m a
get' l = askl (_notificationbar.l)

-- modify' :: (MonadReader r m, MonadBase IO m, HasNotificationBar r) => (Status -> Status) -> m ()
-- modify' f = io . atomically . (`modifyTVar` f) =<< asks (view _notificationBar)

initialize :: (MonadBase IO m) => NotificationBar -> m ()
initialize notifBar = io . updateGlobalLogger rootLoggerName $ addHandler (logHandler notifBar)


logHandler :: NotificationBar -> GenericHandler NotificationBar
logHandler notifBar = GenericHandler
    { priority  = INFO
    , formatter = simpleLogFormatter "$msg"
    , privData  = notifBar
    , writeFunc = \n t -> runReaderT (write t) n
    , closeFunc = \_ -> return ()
    }

write :: (MonadBase IO m, MonadReader r m, HasNotificationBar r) => String -> m ()
write text = do
    label  <- get' labelL

    gAsync $ do
        labelSetAttributes label [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]
        labelSetMarkup label text
