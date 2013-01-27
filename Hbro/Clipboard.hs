-- | Designed to be imported as @qualified@.
module Hbro.Clipboard where

-- {{{ Imports
import Hbro.Error
import Hbro.Util

import Control.Monad.Base
import Control.Monad.Error
import Control.Monad.Trans.Control

import Graphics.UI.Gtk.General.Clipboard
-- }}}

-- | Write given string to primary clipboard
insert :: (MonadBase IO m) => String -> m ()
insert text = io $ clipboardGet selectionPrimary >>= (`clipboardSetText` text)

-- | Feed given function with primary cliboard's content
with :: (MonadBaseControl IO m, MonadError HError m) => (String -> m a) -> m ()
with f = do
    clip <- io $ clipboardGet selectionPrimary
    requestText clip f

-- | Wrapping around 'clipboardRequestText' to be able to use any monad based on IO as callback
requestText :: (MonadBaseControl IO m, ClipboardClass a, MonadError HError m) => a -> (String -> m b) -> m ()
requestText clip f = liftBaseWith $ \runInIO -> clipboardRequestText clip $ \x -> void . runInIO . maybe (throwError EmptyClipboard) f $ x
