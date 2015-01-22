{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Hbro.Event (
-- * Types
      Event(..)
    , Signal
-- * Utils
    , newSignal
    , emit
    , setDefaultHook
    , addHook
    , addRecursiveHook
    , listenTo
    ) where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Concurrent.Async.Lifted

import           Data.Function                   (fix)
-- }}}

-- | An event type is defined by its input type.
class (Show e) => Event e where
  type Input e :: *
  type Input e = ()

-- | A signal notifies the occurrence of an event.
data (Event e) => Signal e = Signal e (TChan (Input e)) (MVar (Async ()))

instance (Event e) => Describable (Signal e) where
  describe (Signal e _ _) = tshow e

-- | 'Signal' exports no constructor, use this function instead.
newSignal :: (BaseIO m, Event e) => e -> m (Signal e)
newSignal e = Signal e <$> io newBroadcastTChanIO <*> newEmptyMVar

-- | Blocks until signal is received.
waitFor :: (MonadIO m) => TChan a -> m a
waitFor = atomically . readTChan

-- | Trigger an event.
emit :: (Event e, MonadIO m) => Signal e -> Input e -> m ()
emit (Signal e s _) input = do
  debugM $ "Event triggered: " ++ tshow e
  atomically $ writeTChan s input

-- | A default hook is run as long as no other hook is added.
setDefaultHook :: (Event a, ControlIO m) => Signal a -> (Input a -> m ()) -> m ()
setDefaultHook (Signal _ signal defThread) f = do
  signal' <- atomically $ dupTChan signal
  mapM cancel =<< tryTakeMVar defThread
  putMVar defThread . map (const ()) =<< (async . forever $ waitFor signal' >>= f)

-- | Execute a function each time an event occurs.
addHook :: (Event a, ControlIO m) => Signal a -> (Input a -> m ()) -> m (Async ())
addHook (Signal _ signal defThread) f = do
  signal' <- atomically $ dupTChan signal
  thread  <- async . forever $ waitFor signal' >>= f
  mapM cancel =<< tryTakeMVar defThread
  return $ map (const ()) thread

-- | Generalized version of 'addHook' where the callback function may recurse
addRecursiveHook :: (Event a, ControlIO m) => Signal a -> b -> (b -> Input a -> m b) -> m (Async ())
addRecursiveHook (Signal _ signal defThread) init f = do
  signal' <- atomically $ dupTChan signal
  thread  <- async $ (fix $ \recurse acc -> waitFor signal' >>= f acc >>= recurse) init
  mapM cancel =<< tryTakeMVar defThread
  return $ map (const ()) thread

listenTo :: (Event a, MonadIO m) => Signal a -> m (Async (Input a))
listenTo (Signal _ signal _) = io . async . waitFor =<< atomically (dupTChan signal)
