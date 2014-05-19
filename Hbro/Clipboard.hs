-- | Designed to be imported as @qualified@.
module Hbro.Clipboard where

-- {{{ Imports
import Hbro.Error
import Hbro.Util

import Control.Concurrent.MVar.Lifted

import Graphics.UI.Gtk.General.Clipboard
import Graphics.UI.Gtk.General.Selection
-- }}}

data EmptyClipboard = EmptyClipboard SelectionTag deriving(Typeable)
instance Exception EmptyClipboard
instance Show EmptyClipboard where show tag = "Empty clipboard [" ++ show tag ++ "]"


-- | Write given string to the selection-primary clipboard
write :: (MonadBase IO m) => String -> m ()
write = write' selectionPrimary

-- | Write given string to the given clipboard
write' :: (MonadBase IO m) => SelectionTag -> String -> m ()
write' tag text = do
    io . debugM "hbro.clipboard" $ "Writing to clipboard: " ++ text
    gSync $ clipboardGet tag >>= (`clipboardSetText` text)


-- | Read clipboard's content. 'selectionPrimary' is inspected first; if empty, 'selectionClipboard' is returned.
read :: (MonadBase IO m, MonadThrow m, MonadCatch m) => m String
read = esum (EmptyClipboard selectionPrimary) $ map (logErrors . read') [selectionPrimary, selectionClipboard]

-- |
read' :: (MonadBase IO m, MonadThrow m) => SelectionTag -> m String
read' tag = do
    clipboard <- gSync $ clipboardGet tag
    result    <- newEmptyMVar

    gAsync . clipboardRequestText clipboard $ putMVar result

    takeMVar result `failWithM` EmptyClipboard tag
