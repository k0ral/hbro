{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, TemplateHaskell #-}
module Hbro.Notification where

-- {{{ Imports
import Hbro.Util

import Control.Lens
import Control.Monad.Base
import Control.Monad.Error hiding(forM_, mapM_)

import Data.Foldable
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.General.General

import Prelude hiding(mapM_)
-- }}}

-- {{{ Types
data NotificationBar = NotificationBar {
    _label :: Label,
    _timer :: IORef (Maybe HandlerId)}

makeLenses ''NotificationBar

-- | 'MonadReader' for 'NotificationBar'
class NotificationReader m where
    readNotification :: Simple Lens NotificationBar a -> m a

-- | 'MonadWriter' for 'NotificationBar'
class (Monad m) => NotificationWriter m where
    writeNotification :: Simple Lens NotificationBar a -> a -> m a

-- | 'MonadState' for 'NotificationBar'
type NotificationState m = (NotificationReader m, NotificationWriter m)
-- }}}


notify :: (Functor m, MonadBase IO m, NotificationReader m, Error e, MonadError e m) => Int -> String -> m ()
notify duration text = do
    label'  <- readNotification label
    handler <- readNotification timer

    io $ do
        labelSetAttributes label' [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 32767 32767 32767}]
        labelSetMarkup label' text
        mapM_ timeoutRemove =<< readIORef handler

    newID <- io $ timeoutAdd (labelSetMarkup label' "" >> return False) duration
    io . void $ writeIORef handler (Just newID)
    return ()
