{-# LANGUAGE FlexibleContexts #-}
-- | Designed to be imported as @qualified@.
module Hbro.Clipboard where

-- {{{ Imports
-- import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Graphics.UI.Gtk.General.Clipboard
-- }}}


--requestText :: (ClipboardClass a, MonadIO m, MonadError HError m, MonadBaseControl IO m) => a -> ClipboardHook -> m ()
requestText clip f = liftBaseWith $ \runInIO -> clipboardRequestText clip $ \x -> void . runInIO . maybe (throwError $ OtherError "Empty clipboard") f $ x

-- | Write given String to primary clipboard.
insert :: (MonadIO m) => String -> m ()
insert text = io $ clipboardGet selectionPrimary >>= (`clipboardSetText` text)

with :: (MonadIO m, MonadBaseControl IO m, MonadError HError m) => (String -> m ()) -> m ()
with f = do
    clip <- io $ clipboardGet selectionPrimary
    requestText clip f
