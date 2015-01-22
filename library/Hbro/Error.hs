{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Error ( module X, module Hbro.Error) where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Error.Safe
import           Control.Error.Util  as X (hush, note)
import           Control.Monad.Error as X (Error (..), ErrorT, MonadError (..),
                                           runErrorT)
-- }}}


instance Error Text where
  noMsg = ""
  strMsg = pack

instance Error () where
  noMsg = ()
  strMsg = const ()

-- {{{ Fail
-- | 'MonadError' with trivial error.
type MonadFail m = MonadError () m

-- | 'throwError' for 'MonadFail'.
die :: MonadFail m => m a
die = throwError ()

-- | 'runErrorT' with trivial error.
runFailT :: (Monad m) => ErrorT () m a -> m (Maybe a)
runFailT = return . rightMay <=< runErrorT

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

-- | Like 'catchError', except that the error is automatically logged, then discarded.
logErrors :: (MonadIO m, Functor m, MonadError Text m) => m a -> m (Maybe a)
logErrors f = catchError (Just <$> f) $ \e -> errorM e >> return Nothing

-- | Like 'logErrors', but discards the result.
logErrors_ :: (MonadIO m, Functor m, MonadError Text m) => m a -> m ()
logErrors_ = void . logErrors
