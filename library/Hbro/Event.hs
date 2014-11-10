{-# LANGUAGE TypeFamilies #-}
module Hbro.Event (
-- * Types
      Event(..)
    , Signal
    , Hook(..)
-- * Utils
    , nullHook
    , newSignal
    , waitFor
    , emit
    ) where

import           Hbro.Prelude

-- | An event type is defined by its input type
class (Show e) => Event e where
  type Input e :: *
  type Input e = ()

-- | A signal notifies the occurrence of an event.
data (Event e) => Signal e = Signal e (TQueue (Input e))

instance (Event e) => Describable (Signal e) where
  describe (Signal e _) = tshow e

-- | 'Signal' exports no constructor, use this function instead.
newSignal :: (BaseIO m, Event e) => e -> m (Signal e)
newSignal e = Signal e <$> io newTQueueIO

-- | A hook is executed when an event occurs
data (Event e) => Hook m e = Hook (Input e -> m ())

-- | A hook that does nothing
nullHook :: (Event e, Monad m) => Hook m e
nullHook = Hook . const $ return ()

waitFor :: (Event e, BaseIO m) => Signal e -> m (Input e)
waitFor (Signal _ s) = atomically $ readTQueue s

emit :: (Event e, BaseIO m) => Signal e -> Input e -> m ()
emit (Signal _ s) input = atomically $ writeTQueue s input
