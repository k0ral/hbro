{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DatatypeContexts    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Hbro.Event (
-- * Types
      Event(..)
    , Signal
    , Handler
-- * Signal manipulation
    , newSignal
    , emit
    , emit'
    , closeSignal
    , closeSignal'
    , listenTo
-- * Handlers
    , addHandler
    , addRecursiveHandler
    , deleteHandlers
    ) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.MonadIO
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Logger.Extended
import           Control.Monad.Trans.Resource

import           Data.Function                   (fix)
-- }}}

-- | An event type is defined by its input type.
class (Show e) => Event e where
  type Input e :: *
  type Input e = ()

  describeInput :: e -> Input e -> Maybe Text

-- | A signal notifies the occurrence of an event.
data (Event e) => Signal e = Signal e (TMChan (Input e)) (TVar [ReleaseKey])

-- | Event handler.
type Handler m a = Input a -> m ()

instance (Event e) => Describable (Signal e) where
  describe (Signal e _ _) = show e

-- | 'Signal' exports no constructor, use this function instead.
newSignal :: (BaseIO m, Event e) => e -> m (Signal e)
newSignal e = Signal e <$> io newBroadcastTMChanIO <*> newTVar []

-- | Blocks until signal is received.
waitFor :: (MonadIO m) => TMChan a -> m (Maybe a)
waitFor = atomically . readTMChan

-- | Trigger an event.
emit :: (Event e, MonadIO m, MonadLogger m) => Signal e -> Input e -> m ()
emit signal@(Signal e _ h) input = do
  forM_ (describeInput e input) $ debug . ("Event triggered: " <>)
  handlers <- readTVar h
  when (null handlers) . forM_ (describeInput e input) $ debug . (<>) "No handler for event: "
  emit' signal input

-- | Like 'emit', but doesn't log anything.
emit' :: (Event e, MonadIO m) => Signal e -> Input e -> m ()
emit' (Signal _ s _) input = atomically $ writeTMChan s input

-- | Close a signal and all its attached hooks.
closeSignal :: (Event e, MonadIO m, MonadLogger m, MonadResource m) => Signal e -> m ()
closeSignal signal = debug "Closing signal." >> closeSignal' signal

-- | Like 'close', but doesn't log anything.
closeSignal' :: (Event e, MonadIO m, MonadResource m) => Signal e -> m ()
closeSignal' signal@(Signal _ s _) = atomically (closeTMChan s) >> deleteHandlers signal

-- | Asynchronously wait for the next event.
listenTo :: (Event a, MonadIO m) => Signal a -> m (Async (Maybe (Input a)))
listenTo (Signal _ signal _) = io . async . waitFor =<< atomically (dupTMChan signal)

-- | Execute a function each time an event occurs.
addHandler :: (Event a, ControlIO m, MonadResource m) => Signal a -> Handler m a -> m ReleaseKey
addHandler signal f = addRecursiveHandler signal () $ const f

-- | Generalized version of 'addHandler' where the callback function may recurse.
addRecursiveHandler :: (Event a, ControlIO m, MonadResource m) => Signal a -> b -> (b -> Input a -> m b) -> m ReleaseKey
addRecursiveHandler (Signal _ s handlers) init f = do
  signal <- atomically $ dupTMChan s

  result <- liftBaseWith $ \runInIO ->
    runInIO . flip allocate cancel . async . void . runInIO . flip fix init $ \recurse acc ->
      waitFor signal >>= mapM_ (f acc >=> recurse)
  (releaseKey, _ :: Async ()) <- restoreM result
  modifyTVar handlers (releaseKey:)

  return releaseKey

-- | Stop all handlers associated to the given signal.
deleteHandlers :: (Event e, MonadIO m, MonadResource m) => Signal e -> m ()
deleteHandlers (Signal _ _ handlers) = mapM_ release =<< readTVar handlers
