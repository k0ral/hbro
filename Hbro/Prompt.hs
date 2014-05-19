{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- | Designed to be imported as @qualified@.
module Hbro.Prompt(
    PromptBar(..),
    onChanged,
    onValidated,
    entry,
    description,
    box,
    PromptReader(..),
    init,
    open,
    hide,
    clean,
    read,
    incrementalRead,
    iread,
    readURI,
    getEntryValue)
where

-- {{{ Imports
import Hbro.Error
-- import Hbro.Gui
import Hbro.Network
import Hbro.Notification
import Hbro.Util

import Control.Conditional hiding(when)
import Control.Lens hiding((??))
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Base
import Control.Monad.Error hiding(forM_, mapM_, when)
import Control.Monad.Writer
import Control.Monad.Trans.Control

-- import Data.Foldable
-- import Data.Functor
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Layout.HBox

import Network.URI hiding(parseURIReference)

import Prelude hiding(init, mapM_, read)

import System.Glib.Signals
-- }}}

-- {{{ Types
data PromptBar m = PromptBar {
    _box           :: HBox,
    _description   :: Label,
    _entry         :: Entry,
    _onChanged     :: IORef (String -> m ()),
    _onValidated   :: IORef (String -> m ())}

makeLenses ''PromptBar

class (Monad m, Monad n) => PromptReader n m | m -> n where
    readPrompt :: Simple Lens (PromptBar n) a -> m a
-- }}}

-- Validate/cancel prompt
onEntryValidated :: (MonadBase IO m, MonadBaseControl IO m, NotificationReader m, Error e, Show e, MonadError e m, EntryClass t) => t -> (String -> m ()) -> m (ConnectId t)
onEntryValidated entry' f = liftBaseWith $ \runInIO -> on entry' keyPressEvent $ do
    key <- eventKeyName
    io $ when (key == "Return") $ do
        void . runInIO $ (io (entryGetText entry') >>= f) `catchError` \e -> io (print e) >> notify 5000 (show e)
    return False

-- Incremental behavior
onEntryChanged :: (MonadBaseControl IO m, NotificationReader m, Error e, Show e, MonadError e m, EditableClass t, EntryClass t) => t -> (String -> m ()) -> m (ConnectId t)
onEntryChanged entry' f = liftBaseWith $ \runInIO -> on entry' editableChanged $ do
    void . runInIO $ (io (entryGetText entry') >>= f) `catchError` \e -> io (print e) >> notify 5000 (show e)


init :: (MonadBase IO m, MonadBaseControl IO m, NotificationReader m, Error e, Show e, MonadError e m) => PromptBar m -> m ()
init promptBar = do
    io $ labelSetAttributes l [allItalic, allBold]
    io $ labelSetAttributes l [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 32767 32767 32767}]

    io $ widgetModifyBase entry' StateNormal $ Color 0 0 0
    io $ widgetModifyText entry' StateNormal $ Color 32767 32767 32767

    void . onEntryChanged   entry' $ \v -> io (readIORef onChanged')   >>= \f -> f v
    void . onEntryValidated entry' $ \v -> io (readIORef onValidated') >>= \f -> f v
  where
    l            = _description promptBar
    entry'       = _entry promptBar
    onChanged'   = _onChanged promptBar
    onValidated' = _onValidated promptBar


open :: (Functor m, MonadBase IO m, PromptReader n m, MonadWriter String m) => String -> String -> m ()
open newDescription defaultText = do
    tell "Opening prompt."
    e <- readPrompt entry
    io . (`labelSetText` newDescription) =<< readPrompt description
    io $ entrySetText e defaultText
    io . widgetShow =<< readPrompt box
    io $ widgetGrabFocus e
    io $ editableSetPosition e (-1)

hide :: (MonadBase IO m, PromptReader n m) => m ()
hide = io . widgetHide =<< readPrompt box

-- | Close prompt, clean its content and callbacks
clean :: (MonadBase IO m, PromptReader n m) => m ()
clean = do
     e <- readPrompt entry
     io $ (`widgetRestoreText` StateNormal) e
     io . widgetModifyText e StateNormal $ Color 32767 32767 32767
     hide

     readPrompt onChanged   >>= io . (`writeIORef` return (return ()))
     readPrompt onValidated >>= io . (`writeIORef` return (return ()))
     return ()

-- | Open prompt bar with given description and default value,
-- and register a callback to trigger at validation.
read :: (MonadBaseControl IO m, PromptReader m m, Error e, MonadError e m, MonadWriter String m)
     => String        -- ^ Prompt description
     -> String        -- ^ Initial value
     -> (String -> m ())    -- ^ Function to trigger when validating prompt value
     -> m ()
read = read' False

-- | Same as 'read', but callback is triggered for each change in prompt's entry.
incrementalRead :: (MonadBase IO m, MonadBaseControl IO m, PromptReader m m, Error e, MonadError e m, MonadWriter String m) => String -> String -> (String -> m ())  -> m ()
incrementalRead = read' True

-- | Alias for 'incrementalRead'.
iread :: (MonadBaseControl IO m, PromptReader m m, Error e, MonadError e m, MonadWriter String m) => String -> String -> (String -> m ())  -> m ()
iread = incrementalRead

read' :: (MonadBaseControl IO m, PromptReader m m, Error e, MonadError e m, MonadWriter String m) => Bool -> String -> String -> (String -> m ()) -> m ()
read' incremental description' startValue f = do
    clean
    open description' startValue

    when incremental $ readPrompt onChanged >>= io . (`writeIORef` f)
    readPrompt onValidated >>= io . (`writeIORef` (f >=> const clean))
    return ()


-- | Same as 'read' for URI values
readURI :: (MonadBase IO m, PromptReader m m, MonadError HError m, MonadWriter String m) => String -> String -> (URI -> m ()) -> m ()
readURI description' startValue callback = do
    clean
    open description' startValue
    checkURI startValue

    readPrompt onChanged   >>= io . (`writeIORef` checkURI)
    readPrompt onValidated >>= io . (`writeIORef` (parseURIReference >=> callback >=> const clean))
    return ()
  where
    checkURI v = do
        e <- readPrompt entry
        io $ widgetModifyText e StateNormal color
      where
        color = (isURIReference v) ? green ?? red
        green = Color     0 65535 0
        red   = Color 65535     0 0


getEntryValue :: (MonadBase IO m, PromptReader n m) => m String
getEntryValue = io . entryGetText =<< readPrompt entry
