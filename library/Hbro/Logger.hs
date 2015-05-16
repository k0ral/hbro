{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Hbro.Logger
    ( module X
    , LogMessage(..)
    , MonadThreadedLogger(..)
    , ThreadedLoggingT
    , runThreadedLoggingT
    , logErrors
    , logErrors_
    ) where

-- {{{ Imports
import           Hbro.Event
import           Hbro.Prelude                  hiding (runReaderT)

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger.Extended as X
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource

import           Data.Text                     (justifyLeft)
import           Data.Text.Encoding
import           Data.Text.Encoding.Error

import           System.Log.FastLogger         as X
-- }}}

-- | Log event
data LogMessage = LogMessage deriving(Show)
instance Event LogMessage where
  type Input LogMessage = (Loc, LogSource, LogLevel, Text)
  describeInput _ _ = Nothing

class (MonadLogger m) => MonadThreadedLogger m where
  addLogHandler :: (Input LogMessage -> IO ()) -> m ()

instance (Monad m, MonadThreadedLogger m) => MonadThreadedLogger (ResourceT m) where
  addLogHandler = lift . addLogHandler

newtype ThreadedLoggingT m a = ThreadedLoggingT { unThreadedLoggingT :: ReaderT (Signal LogMessage, LogLevel) m a }
deriving instance (Alternative m) => Alternative (ThreadedLoggingT m)
deriving instance (Applicative m) => Applicative (ThreadedLoggingT m)
deriving instance (Functor m) => Functor (ThreadedLoggingT m)
deriving instance (Monad m) => Monad (ThreadedLoggingT m)
deriving instance (MonadIO m) => MonadIO (ThreadedLoggingT m)
deriving instance (MonadResource m) => MonadResource (ThreadedLoggingT m)
deriving instance (MonadThrow m) => MonadThrow (ThreadedLoggingT m)
deriving instance (MonadCatch m) => MonadCatch (ThreadedLoggingT m)
deriving instance MonadTrans ThreadedLoggingT

instance MonadBase b m => MonadBase b (ThreadedLoggingT m) where
  liftBase = liftBaseDefault

instance MonadTransControl ThreadedLoggingT where
  type StT ThreadedLoggingT a = StT (ReaderT (Signal LogMessage, LogLevel)) a
  liftWith = defaultLiftWith ThreadedLoggingT unThreadedLoggingT
  restoreT = defaultRestoreT ThreadedLoggingT

instance MonadBaseControl b m => MonadBaseControl b (ThreadedLoggingT m) where
  type StM (ThreadedLoggingT m) a = ComposeSt ThreadedLoggingT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance (MonadIO m, Functor m) => MonadLogger (ThreadedLoggingT m) where
  monadLoggerLog loc source level message = ThreadedLoggingT . void . runMaybeT $ do
    (loggerSignal, levelRef) <- Control.Monad.Reader.ask
    guard $ level >= levelRef
    emit' loggerSignal (loc, source, level, decodeUtf8With lenientDecode . fromLogStr $ toLogStr message)

instance (ControlIO m, MonadResource m) => MonadThreadedLogger (ThreadedLoggingT m) where
  addLogHandler f = ThreadedLoggingT $ do
    (loggerSignal, _) <- Control.Monad.Reader.ask
    void $ addHandler loggerSignal (io . f)

runThreadedLoggingT :: (ControlIO m, MonadResource m) => LogLevel -> ThreadedLoggingT m b -> m b
runThreadedLoggingT logLevel f = do
    loggerSignal <- newSignal LogMessage
    addHandler loggerSignal $ \(_loc, _source, level, message) -> io . putStrLn $ formatLevel level ++ " " ++ message

    result <- flip runReaderT (loggerSignal, logLevel) $ unThreadedLoggingT f
    closeSignal' loggerSignal
    return result

formatLevel :: LogLevel -> Text
formatLevel LevelDebug     = "DEBUG"
formatLevel LevelInfo      = "INFO "
formatLevel LevelWarn      = "WARN "
formatLevel LevelError     = "ERROR"
formatLevel (LevelOther a) = justifyLeft 5 ' ' . take 5 $ tshow a

-- | Like 'catchError', except that the error is automatically logged, then discarded.
logErrors :: (ControlIO m, MonadLogger m, MonadCatch m) => m a -> m (Maybe a)
logErrors f = catchAll (Just <$> f) $ \e -> error (tshow e) >> return Nothing

-- | Like 'logErrors', but discards the result.
logErrors_ :: (MonadLogger m, ControlIO m, MonadCatch m) => m a -> m ()
logErrors_ = void . logErrors
