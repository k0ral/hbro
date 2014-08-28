-- | Designed to be imported as @qualified@.
module Hbro.Clipboard where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Logger
import           Hbro.Prelude

import           Graphics.UI.Gtk.General.Clipboard
import           Graphics.UI.Gtk.General.Selection
-- }}}

-- | Write given 'Text' to the selection-primary clipboard
write :: (BaseIO m) => Text -> m ()
write = write' selectionPrimary

-- | Write given text to the given clipboard
write' :: (BaseIO m) => SelectionTag -> Text -> m ()
write' tag text = do
    debugM "hbro.clipboard" $ "Writing to clipboard: " ++ text
    gSync (clipboardGet tag) >>= gAsync . (`clipboardSetText` text)

-- | Read clipboard's content. Both 'selectionPrimary' and 'selectionClipboard' are inspected (in this order).
read :: (BaseIO m) => ExceptT Text m Text
read = read' selectionPrimary <|> read' selectionClipboard

-- | Return the content from the given clipboard.
read' :: (BaseIO m, MonadError Text m) => SelectionTag -> m Text
read' tag = do
    clipboard <- gSync $ clipboardGet tag
    result    <- newEmptyMVar

    gAsync . clipboardRequestText clipboard $ putMVar result

    takeMVar result <!> ("Empty clipboard [" ++ tshow tag ++ "]")
