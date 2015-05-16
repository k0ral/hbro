{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Error ( module X, module Hbro.Error) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Error.Util  as X (hush, note)
import           Control.Monad.Catch ()
-- }}}

failWith :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
failWith x e = maybe (throwM e) return x

-- | Monadic and infix version of 'failWith'
(<!>) :: (MonadThrow m, Exception e) => m (Maybe a) -> e -> m a
(<!>) f e = f >>= (`failWith` e)
