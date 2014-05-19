module Hbro.Error ( module X, module Hbro.Error) where

-- {{{ Imports
import Hbro.Util

import Control.Error.Util as X (note, hush)
import Control.Monad.Catch as X
import Control.Monad.Error as X hiding(forM_, guard, join, mapM_, when)
import Control.Monad.Trans.Maybe as X
-- }}}

-- {{{ Exception types
data UnavailableURI = UnavailableURI deriving(Typeable)
instance Exception UnavailableURI
instance Show UnavailableURI where show _ = "No available URI."
-- }}}

-- | Convert a 'Maybe' value into a 'MonadThrow' monad.
failWith, (<!>) :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
failWith x e = either throwM return $ note e x
(<!>) = failWith

-- | Same as 'failWith' with a monadic 'Maybe' value
failWithM :: (MonadThrow m, Exception e) => m (Maybe a) -> e -> m a
failWithM f e = f >>= (`failWith` e)

-- | Similar to 'msum'. Execute sequentially a list of error-prone functions until one succeeds. In case all fail, an 'Error' is thrown.
esum :: (MonadThrow m, Exception e) => e -> [m (Maybe a)] -> m a
esum e f = (runMaybeT . msum . map MaybeT) f `failWithM` e

-- | Like 'runErrorT', except that the error is automatically logged, then discarded.
logErrors :: (MonadBase IO m, MonadThrow m, MonadCatch m) => m a -> m (Maybe a)
logErrors f = (Just <$> f) `catchAll` \e -> io (errorM "hbro.error" $ show e) >> return Nothing

-- | Like 'logErrors', but discards the result.
logErrors_ :: (MonadBase IO m, MonadThrow m, MonadCatch m) => m a -> m ()
logErrors_ = void . logErrors
