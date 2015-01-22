{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Monad.Reader.Tagged
  ( ReaderT
  , runReaderT
  , MonadReader(..)
  ) where

-- {{{ Imports
import           ClassyPrelude               hiding (MonadReader (..),
                                              ReaderT (..))

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Error
import           Control.Monad.Trans.Control
-- }}}


-- | Tagged version of the Reader monad transformer.
newtype ReaderT tag r m a = ReaderT { unReaderT :: tag -> r -> m a }

-- | 'ReaderT' exports no constructor. Use this function instead.
runReaderT :: tag -> r -> ReaderT tag r m a -> m a
runReaderT tag r f = unReaderT f tag r

-- | Transform the computation inside a 'ReaderT'.
mapReaderT :: (m a -> n b) -> ReaderT t r m a -> ReaderT t r n b
mapReaderT f m = ReaderT $ \t r -> f $ unReaderT m t r

liftReaderT :: m a -> ReaderT t r m a
liftReaderT = ReaderT . const . const

liftCatch ::
    (m a -> (e -> m a) -> m a)          -- ^ @catch@ on the argument monad.
    -> ReaderT t r m a                    -- ^ Computation to attempt.
    -> (e -> ReaderT t r m a)             -- ^ Exception handler.
    -> ReaderT t r m a
liftCatch f m h =
    ReaderT $ \t r -> f (runReaderT t r m) (runReaderT t r . h)

instance MonadTrans (ReaderT t r) where
    lift = liftReaderT

instance MonadTransControl (ReaderT t r) where
    type StT (ReaderT t r) a = a
    liftWith f = ReaderT $ \tag r -> f $ \t -> runReaderT tag r t
    restoreT = ReaderT . const . const

instance (MonadBaseControl b m) => MonadBaseControl b (ReaderT t r m) where
    type StM (ReaderT t r m) a = ComposeSt (ReaderT t r) m a;
    liftBaseWith = defaultLiftBaseWith;
    restoreM     = defaultRestoreM;

instance (Functor m) => Functor (ReaderT t r m) where
    fmap f  = mapReaderT (fmap f)

instance (Applicative m) => Applicative (ReaderT t r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \t r -> unReaderT f t r <*> unReaderT v t r

instance (Applicative b, Applicative m, Monad b, Monad m, MonadBase b m) => MonadBase b (ReaderT t r m) where
    liftBase = lift . liftBase

instance (Monad m) => Monad (ReaderT t r m) where
    return a = ReaderT $ \_ _ -> return a
    m >>= k = ReaderT $ \t r -> do
      a <- unReaderT m t r
      unReaderT (k a) t r

instance (MonadIO m) => MonadIO (ReaderT t r m) where
    liftIO = lift . liftIO

instance MonadError e m => MonadError e (ReaderT t r m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

instance Alternative m => Alternative (ReaderT t r m) where
    empty   = liftReaderT empty
    m <|> n = ReaderT $ \t r -> runReaderT t r m <|> runReaderT t r n

instance (MonadFix m) => MonadFix (ReaderT t r m) where
    mfix f = ReaderT $ \t r -> mfix $ \a -> runReaderT t r (f a)

instance (MonadPlus m) => MonadPlus (ReaderT t r m) where
    mzero       = lift mzero
    m `mplus` n = ReaderT $ \t r -> runReaderT t r m `mplus` runReaderT t r n

class (Monad m) => MonadReader tag r m | m tag -> r where
    read :: tag -> m r

    readL :: tag -> Lens' r a -> m a
    readL tag l = return . view l =<< read tag


data HTrue
data HFalse

type family EmbeddedReader t r acc m where
    EmbeddedReader t r acc (ReaderT t r m) = acc
    EmbeddedReader t r acc (ReaderT t' r' m) = EmbeddedReader t r HTrue m
    EmbeddedReader t r acc m = HFalse


class MonadReader' embedded tag r m | m tag -> r where
    read' :: embedded -> tag -> m r

instance (Monad m) => MonadReader' HFalse tag r (ReaderT tag r m) where
    read' _ _ = ReaderT (\_ r -> return r)

instance (Monad m, MonadReader tag r m) => MonadReader' HTrue tag r (ReaderT tag' r' m) where
    read' _ = lift . read

instance (Monad m, MonadReader' (EmbeddedReader tag r HFalse m) tag r m) => MonadReader tag r m where
    read = read' (undefined :: EmbeddedReader tag r HFalse m)
