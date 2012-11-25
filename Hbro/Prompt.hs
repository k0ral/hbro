{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- | Designed to be imported as @qualified@.
module Hbro.Prompt where

-- {{{ Imports
--import Hbro.Core
import Hbro.Types
import Hbro.Util
import Hbro.Gtk.Entry

import Control.Conditional hiding(when)
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_, when)
--import Control.Monad.IO.Class
import Control.Monad.Reader hiding(forM_, mapM_, when)
import Control.Monad.Trans.Control

import Data.Foldable
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Layout.HBox

import Network.URI hiding(parseURIReference)

import Prelude hiding(mapM_)

import System.Glib.Signals
-- }}}

instance Buildable PromptBar where
    build builder = io $ do
        label <- builderGetObject builder castToLabel "promptDescription"
        entry <- builderGetObject builder castToEntry "promptEntry"
        box   <- builderGetObject builder castToHBox  "promptBox"

        return $ PromptBar box label entry

setup :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasPromptBar r, HasHooks r, HasWebView r, MonadError HError m) => m ()
setup = do
    label <- asks _promptDescription
    io $ labelSetAttributes label [allItalic, allBold]
    io $ labelSetAttributes label [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 32767 32767 32767}]

    entry <- asks _promptEntry
    io $ widgetModifyBase entry StateNormal $ Color 0 0 0
    io $ widgetModifyText entry StateNormal $ Color 32767 32767 32767

-- Validate/cancel prompt
    webView <- asks _webview
    io . void . on entry keyPressEvent $ do
        key <- eventKeyName
        when (key == "Return" || key == "Escape") $ io $ do
            --runInIO clean
            widgetGrabFocus webView
            return ()
        return False
    return ()


open :: (MonadIO m, MonadReader r m, HasPromptBar r, HasOptions r) => String -> String -> m ()
open newDescription defaultText = do
    logVerbose "Opening prompt."
    entry <- asks _promptEntry
    io . (`labelSetText` newDescription) =<< asks _promptDescription
    io $ entrySetText entry defaultText
    io . widgetShow =<< asks _promptBox
    io $ widgetGrabFocus entry
    io $ editableSetPosition entry (-1)

-- | Close prompt, clean its content and callbacks
clean :: (MonadIO m, MonadReader r m, HasHooks r, HasPromptBar r, MonadBaseControl IO m) => m ()
clean = do
     entry <- asks _promptEntry
     io $ widgetRestoreText entry StateNormal
     io $ widgetModifyText entry StateNormal $ Color 32767 32767 32767
     io . widgetHide =<< asks _promptBox

     asks _promptChanged   >>= \ref -> io $ readIORef ref >>= mapM_ signalDisconnect >> writeIORef ref Nothing
     asks _promptValidated >>= \ref -> io $ readIORef ref >>= mapM_ signalDisconnect >> writeIORef ref Nothing

-- | Open prompt bar with given description and default value,
-- and register a callback to trigger at validation.
read :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasPromptBar r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m)
     => String        -- ^ Prompt description
     -> String        -- ^ Initial value
     -> EntryHook     -- ^ Function to trigger when validating prompt value
     -> m ()
read = read' False

-- | Same as 'read', but callback is triggered for each change in prompt's entry.
incrementalRead, iread :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, MonadError HError m) => String -> String -> EntryHook -> m ()
incrementalRead = read' True
-- | Alias for incrementalRead.
iread           = incrementalRead

read' :: (MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, MonadError HError m) => Bool -> String -> String -> EntryHook -> m ()
read' incremental description startValue f = do
    clean
    open description startValue
    (PromptBar { _entry = entry }) <- asks _promptBar

    when incremental $ onEntryChanged entry f   >>= \i -> asks _promptChanged   >>= io . (`writeIORef` Just i)
    onEntryValidated entry (f >=> const clean)  >>= \i -> asks _promptValidated >>= io . (`writeIORef` Just i)
    return ()


-- | Same as 'read' for URI values
readURI :: (MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, MonadBaseControl IO m, MonadIO m, MonadError HError m)
        => String -> String -> EntryURIHook -> m ()
readURI description startValue callback = do
    clean
    open description startValue
    checkURI startValue

    (PromptBar { _entry = entry }) <- asks _promptBar
    id1 <- onEntryChanged   entry $ checkURI
    id2 <- onEntryValidated entry $ parseURIReference >=> callback >=> const clean
    asks _promptChanged   >>= io . (`writeIORef` Just id1)
    asks _promptValidated >>= io . (`writeIORef` Just id2)
    return ()
  where
    checkURI :: EntryHook
    checkURI value = do
        (PromptBar { _entry = entry }) <- asks _promptBar
        io $ widgetModifyText entry StateNormal color
      where
        color = (isURIReference value) ? green ?? red
        green = Color     0 65535 0
        red   = Color 65535     0 0
