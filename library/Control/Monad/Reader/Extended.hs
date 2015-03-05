{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module provides a generic way to combine multiple `MonadReader` contexts into a single one.
--
-- == Example
-- > {-# LANGUAGE TupleSections #-}
-- >
-- > f :: (MonadReader r m, Has A r, Has B r) => m ()
-- > f = do
-- >   (a :: A) <- ask
-- >   (b :: B) <- ask
-- >   doSomething a b
-- >
-- > main = let a = makeA :: A
-- >            b = makeB :: B
-- >        in flip runReaderT (a, ()) . withReaderT (b,) $ f
--
module Control.Monad.Reader.Extended
    ( module X
    , Has(..)
    , ask
    , asks
    ) where

-- {{{ Imports
import           Control.Monad.Reader as X (MonadReader, runReaderT,
                                            withReaderT)
import qualified Control.Monad.Reader as Reader
-- }}}

data HTrue
data HFalse

type family Embedded r t acc where
    Embedded r ()     acc = HFalse
    Embedded r (r, a) acc = acc
    Embedded r (a, b) acc = Embedded r b HTrue


class Has' embedded a t where get' :: embedded -> t -> a

instance Has' HFalse a (a, t) where
  get' _ (a, _) = a

instance (Has a t') => Has' HTrue a (t, t') where
  get' _ (_, t') = get t'

class Has a t where get :: t -> a

-- | This instance means @t = (t1, (t2, (... , (tn, ()) ...) )@ such that @a = ti@ for some i
instance Has' (Embedded a t HFalse) a t => Has a t where
  get = get' (undefined :: Embedded a t HFalse)

-- | Replacement for 'Control.Monad.Reader.ask'.
ask :: (MonadReader r m, Has a r) => m a
ask = Reader.asks get

-- | Replacement for 'Control.Monad.Reader.asks'.
asks :: (MonadReader r m, Has a r) => (a -> b) -> m b
asks f = Reader.asks (f . get)
