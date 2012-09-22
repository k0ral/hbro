{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Hbro.Gtk.Entry where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

-- import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM

import System.Glib.Signals
-- }}}

-- Validate/cancel prompt
onEntryValidated :: (MonadIO m, MonadBaseControl IO m, MonadError HError m, MonadReader r m, HasConfig r, HasGUI r, HasPromptBar r, HasOptions r, HasZMQContext r, HasHooks r, EntryClass t) => t -> EntryHook -> m (ConnectId t)
onEntryValidated entry f = liftBaseWith $ \runInIO -> on entry keyPressEvent $ do
    key <- eventKeyName
    io $ when (key == "Return") $ do
        void . runInIO $ (io (entryGetText entry) >>= f) `catchError` \e -> io (print e) >> notify 5000 (show e)
    return False

-- Incremental behavior
onEntryChanged :: (MonadIO m, MonadBaseControl IO m, MonadError HError m, MonadReader r m, HasConfig r, HasGUI r, HasPromptBar r, HasOptions r, HasZMQContext r, HasHooks r, EditableClass t, EntryClass t) => t -> EntryHook -> m (ConnectId t)
onEntryChanged entry f = liftBaseWith $ \runInIO -> on entry editableChanged $ do
    void . runInIO $ (io (entryGetText entry) >>= f) `catchError` \e -> io (print e) >> notify 5000 (show e)
