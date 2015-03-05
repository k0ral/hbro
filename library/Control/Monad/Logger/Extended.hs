module Control.Monad.Logger.Extended (module X, module Control.Monad.Logger.Extended) where

-- {{{ Imports
import           Control.Monad.Logger as X

import           Data.Text
-- }}}

debug, info, warning, error :: (MonadLogger m) => Text -> m ()
debug = logDebugN
info = logInfoN
warning = logWarnN
error = logErrorN
