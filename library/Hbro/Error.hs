{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Error ( module X, module Hbro.Error) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Error.Safe
import           Control.Error.Util   as X (hush, note)
import           Control.Monad.Except as X (ExceptT, MonadError (..),
                                            runExceptT)
-- }}}

-- {{{ Fail
-- | 'MonadError' with trivial error.
type MonadFail m = MonadError () m

-- | 'throwError' for 'MonadFail'.
die :: MonadFail m => m a
die = throwError ()

-- | 'runErrorT' with trivial error.
runFailT :: (Monad m) => ExceptT () m a -> m (Maybe a)
runFailT = return . rightMay <=< runExceptT

-- | Lift a 'Maybe' value into a 'MonadFail' computation
liftMaybe :: (MonadFail m) => Maybe a -> m a
liftMaybe = maybe (throwError ()) return
-- }}}

-- | Lift an 'Either' value into a 'MonadError' computation
liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return

failWith :: (MonadError e m) => Maybe a -> e -> m a
failWith x e = either throwError return $ note e x

-- | Monadic and infix version of 'failWith'
(<!>) :: (MonadError e m) => m (Maybe a) -> e -> m a
(<!>) f e = f >>= (`failWith` e)
