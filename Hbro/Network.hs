-- | Functions from 'Network.URI' rewritten with 'MonadError' instead of 'Maybe'.
module Hbro.Network where

-- {{{ Imports
import Hbro.Error

import Control.Monad.Error

import Network.URI as N
-- }}}


parseURIReference :: (MonadError HError m) => String -> m URI
parseURIReference uri = maybe (throwError $ InvalidURI uri) return $ N.parseURIReference uri


parseURI :: (MonadError HError m) => String -> m URI
parseURI uri = maybe (throwError $ InvalidURI uri) return $ N.parseURI uri
