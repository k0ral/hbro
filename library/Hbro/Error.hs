module Hbro.Error ( module X, module Hbro.Error) where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Error.Util        as X (hush, note)
import           Control.Monad.Except      as X (ExceptT, MonadError (..),
                                                 runExceptT, withExceptT)
import           Control.Monad.Trans.Maybe as X
-- }}}

-- | Convert a 'Maybe' value into a 'MonadError' monad.
failWith :: (MonadError e m) => Maybe a -> e -> m a
failWith x e = either throwError return $ note e x

-- | Monadic and infix version of 'failWith'
(<!>) :: (MonadError e m) => m (Maybe a) -> e -> m a
(<!>) f e = f >>= (`failWith` e)

-- | Like 'runExceptT', except that the error is automatically logged, then discarded.
logErrors :: (BaseIO m) => ExceptT Text m a -> m (Maybe a)
logErrors = runExceptT >=> either (\e -> errorM "hbro.error" e >> return Nothing) (return . Just)

logErrors' :: (BaseIO m) => ExceptT e m a -> m (Maybe a)
logErrors' = return . either (const Nothing) Just <=< runExceptT

-- | Like 'logErrors', but discards the result.
logErrors_ :: (BaseIO m) => ExceptT Text m a -> m ()
logErrors_ = void . logErrors
