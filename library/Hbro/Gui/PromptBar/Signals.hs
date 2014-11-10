{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Logger
import           Hbro.Prelude                    hiding (on)

import           Control.Lens

import           Graphics.UI.Gtk.Abstract.Widget
import           Graphics.UI.Gtk.Entry.Editable
import           Graphics.UI.Gtk.Entry.Entry
import           Graphics.UI.Gtk.Gdk.EventM      as Gdk

import           System.Glib.Signals             hiding(Signal)
-- }}}


-- {{{ Utils
onEntryCancelled :: (BaseIO m, EntryClass t) => t -> (Text -> IO ()) -> m (ConnectId t)
onEntryCancelled theEntry f = gSync . on theEntry keyPressEvent $ do
    key <- KeyVal <$> eventKeyVal
    io . when (key == _Escape) $ do
        value <- entryGetText theEntry
        debugM "hbro.prompt" $ "Prompt cancelled with value: " ++ value
        f value
    return False

onEntryChanged :: (BaseIO m, EditableClass t, EntryClass t) => t -> (Text -> IO ()) -> m (ConnectId t)
onEntryChanged theEntry f = gSync . on theEntry editableChanged $ do
    value <- entryGetText theEntry
    debugM "hbro.prompt" $ "Prompt value changed to: " ++ value
    f value


onEntryActivated :: (BaseIO m, EntryClass t) => t -> (Text -> IO ()) -> m (ConnectId t)
onEntryActivated theEntry f = gSync . on theEntry entryActivated $ do
    value <- entryGetText theEntry
    debugM "hbro.prompt" $ "Prompt activated with value: " ++ value
    f value
-- }}}

-- {{{ Types
data Cancelled = Cancelled deriving(Show)
instance Event Cancelled where
  type Input Cancelled = Text

data Changed   = Changed deriving(Show)
instance Event Changed where
  type Input Changed = Text

data Activated = Activated deriving(Show)
instance Event Activated where
  type Input Activated = Text

declareLenses [d|
  data Signals = Signals
    { cancelledL :: Signal Cancelled
    , changedL   :: Signal Changed
    , validatedL :: Signal Activated
    }
  |]
-- }}}

initSignals :: (BaseIO m) => m Signals
initSignals = Signals <$> newSignal Cancelled <*> newSignal Changed <*> newSignal Activated


attach :: (BaseIO m, EditableClass t, EntryClass t) => t -> Signals -> m ()
attach entry signals = void $ sequence
    [ onEntryCancelled entry . emit $ signals^.cancelledL
    , onEntryChanged   entry . emit $ signals^.changedL
    , onEntryActivated entry . emit $ signals^.validatedL
    ]
