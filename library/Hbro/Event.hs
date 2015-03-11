{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Hbro.Event (
-- * Types
      Event(..)
    , Signal
-- * Signal manipulation
    , newSignal
    , emit
    , emit'
    , closeSignal
    , closeSignal'
    , listenTo
-- * Hooks
    , setDefaultHook
    , addHook
    , addRecursiveHook
    ) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Logger.Extended

import           Data.Function                   (fix)
-- }}}

-- | An event type is defined by its input type.
class (Show e) => Event e where
  type Input e :: *
  type Input e = ()

  describeInput :: e -> Input e -> Maybe Text

-- | A signal notifies the occurrence of an event.
data (Event e) => Signal e = Signal e (TMChan (Input e)) (MVar (Async ()))

instance (Event e) => Describable (Signal e) where
  describe (Signal e _ _) = tshow e

-- | 'Signal' exports no constructor, use this function instead.
newSignal :: (BaseIO m, Event e) => e -> m (Signal e)
newSignal e = Signal e <$> io newBroadcastTMChanIO <*> newEmptyMVar

-- | Blocks until signal is received.
waitFor :: (MonadIO m) => TMChan a -> m (Maybe a)
waitFor = atomically . readTMChan

-- | Trigger an event.
emit :: (Event e, MonadIO m, MonadLogger m) => Signal e -> Input e -> m ()
emit signal@(Signal e _ _) input = do
  forM_ (describeInput e input) $ debug . ("Event triggered: " ++)
  emit' signal input

-- | Like 'emit', but doesn't log anything.
emit' :: (Event e, MonadIO m) => Signal e -> Input e -> m ()
emit' (Signal _ s _) input = atomically $ writeTMChan s input

-- | Close a signal and all its attached hooks.
closeSignal :: (Event e, MonadIO m, MonadLogger m) => Signal e -> m ()
closeSignal signal = do
  debug "Closing signal."
  closeSignal' signal

-- | Like 'close', but doesn't log anything.
closeSignal' :: (Event e, MonadIO m) => Signal e -> m ()
closeSignal' (Signal _ s _) = atomically $ closeTMChan s

-- | Asynchronously wait for the next event.
listenTo :: (Event a, MonadIO m) => Signal a -> m (Async (Maybe (Input a)))
listenTo (Signal _ signal _) = io . async . waitFor =<< atomically (dupTMChan signal)

-- | A default hook is run as long as no other hook is added.
setDefaultHook :: (Event a, ControlIO m) => Signal a -> (Input a -> m ()) -> m ()
setDefaultHook (Signal _ s defThread) f = do
  signal <- atomically $ dupTMChan s
  mapM cancel =<< tryTakeMVar defThread

  hookThread <- async . fix $ \recurse -> do
    mapM_ (\x -> f x >> recurse) =<< waitFor signal

  putMVar defThread $ map (const ()) hookThread

-- | Execute a function each time an event occurs.
addHook :: (Event a, ControlIO m) => Signal a -> (Input a -> m ()) -> m (Async ())
addHook (Signal _ s defThread) f = do
  signal <- atomically $ dupTMChan s
  thread  <- async . fix $ \recurse -> do
    mapM_ (\x -> f x >> recurse) =<< waitFor signal

  mapM cancel =<< tryTakeMVar defThread
  return $ map (const ()) thread

-- | Generalized version of 'addHook' where the callback function may recurse
addRecursiveHook :: (Event a, ControlIO m) => Signal a -> b -> (b -> Input a -> m b) -> m (Async ())
addRecursiveHook (Signal _ signal defThread) init f = do
  signal' <- atomically $ dupTMChan signal
  thread  <- async . flip fix init $ \recurse acc -> do
    mapM_ (f acc >=> recurse) =<< waitFor signal'

  mapM cancel =<< tryTakeMVar defThread
  return $ map (const ()) thread
