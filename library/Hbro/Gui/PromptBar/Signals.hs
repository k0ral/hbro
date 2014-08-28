{-# LANGUAGE TemplateHaskell #-}
module Hbro.Gui.PromptBar.Signals (
-- * Utils
    onEntryCancelled,
    onEntryChanged,
    onEntryActivated,
-- * Signals
    Cancelled(..),
    Changed(..),
    Activated(..),
    Signals,
    cancelledL,
    changedL,
    validatedL,
    initSignals,
    attach,
) where

-- {{{ Imports
-- import Hbro.Error
import           Hbro.Gdk.KeyVal
import           Hbro.Logger
import           Hbro.Prelude                    hiding (on)

import           Control.Lens

import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Entry.Editable
import           Graphics.UI.Gtk.Entry.Entry
import           Graphics.UI.Gtk.Gdk.EventM      as Gdk

import           System.Glib.Signals
-- }}}


-- {{{ Utils
onEntryCancelled :: (BaseIO m, EntryClass t) => t -> (Cancelled -> IO ()) -> m (ConnectId t)
onEntryCancelled theEntry f = gSync . on theEntry keyPressEvent $ do
    key <- KeyVal <$> eventKeyVal
    io . when (key == _Escape) $ do
        value <- entryGetText theEntry
        debugM "hbro.prompt" $ "Prompt cancelled with value: " ++ value
        f (Cancelled value)
    return False

onEntryChanged :: (BaseIO m, EditableClass t, EntryClass t) => t -> (Changed -> IO ()) -> m (ConnectId t)
onEntryChanged theEntry f = gSync . on theEntry editableChanged $ do
    value <- entryGetText theEntry
    debugM "hbro.prompt" $ "Prompt value changed to: " ++ value
    f (Changed value)


onEntryActivated :: (BaseIO m, EntryClass t) => t -> (Activated -> IO ()) -> m (ConnectId t)
onEntryActivated theEntry f = gSync . on theEntry entryActivated $ do
    value <- entryGetText theEntry
    debugM "hbro.prompt" $ "Prompt activated with value: " ++ value
    f (Activated value)
-- }}}

-- {{{ Types
data Cancelled = Cancelled Text
instance Describable Cancelled where describe _ = "PromptCancelled"

data Changed   = Changed Text
instance Describable Changed where describe _ = "PromptChanged"

data Activated = Activated Text
instance Describable Activated where describe _ = "PromptActivated"

data Signals = Signals
    { _cancelled :: TMVar Cancelled
    , _changed   :: TMVar Changed
    , _validated :: TMVar Activated
    }

makeLensesWith ?? ''Signals $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
-- }}}

initSignals :: (BaseIO m) => m Signals
initSignals = io (Signals <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO)


attach :: (BaseIO m, EditableClass t, EntryClass t) => t -> Signals -> m ()
attach entry signals = void $ sequence
    [ onEntryCancelled entry $ void . atomically . tryPutTMVar (signals^.cancelledL)
    , onEntryChanged   entry $ void . atomically . tryPutTMVar (signals^.changedL)
    , onEntryActivated entry $ void . atomically . tryPutTMVar (signals^.validatedL)
    ]
