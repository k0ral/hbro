{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Designed to be imported as @qualified@.
module Hbro.Clipboard where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Concurrent.MVar.Lifted

import           Graphics.UI.Gtk.General.Clipboard
import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.General.Selection
-- }}}


data ClipboardException = EmptyClipboard SelectionTag deriving(Eq, Show)

instance Exception ClipboardException where
  displayException (EmptyClipboard tag) = "Empty clipboard [" <> show tag <> "]"


-- | Write given 'Text' to the selection-primary clipboard
write :: (BaseIO m, MonadLogger m) => Text -> m ()
write = write' selectionPrimary

-- | Write given text to the given clipboard
write' :: (BaseIO m, MonadLogger m) => SelectionTag -> Text -> m ()
write' tag text = do
    debug $ "Writing to clipboard: " <> text
    gSync (clipboardGet tag) >>= gAsync . (`clipboardSetText` text)

-- | Read clipboard's content. Both 'selectionPrimary' and 'selectionClipboard' are inspected (in this order).
read :: (BaseIO m, Alternative m, MonadThrow m) => m Text
read = read' selectionPrimary <|> read' selectionClipboard

-- | Return the content from the given clipboard.
read' :: (Alternative m, MonadThrow m, BaseIO m) => SelectionTag -> m Text
read' tag = do
    clipboard <- gSync $ clipboardGet tag
    result    <- newEmptyMVar

    gAsync . clipboardRequestText clipboard $ putMVar result

    takeMVar result <!> EmptyClipboard tag
