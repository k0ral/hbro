{-# LANGUAGE DeriveDataTypeable #-}
-- | Functions from 'Network.URI' rewritten with 'MonadError' instead of 'Maybe'.
module Network.URI.Monadic
    ( module X
    , parseURIReference
    , parseURI
    ) where

-- {{{ Imports
import Hbro.Prelude

import Control.Monad.Except (MonadError(..), throwError)

import Network.URI as X hiding(parseURIReference, parseURI)
import qualified Network.URI as N
-- }}}

-- | Error message
invalidURI :: Text -> Text
invalidURI uri = "Invalid URI: " ++ uri

parseURIReference :: (MonadError Text m) => Text -> m URI
parseURIReference uri = maybe (throwError $ invalidURI uri) return . N.parseURIReference $ unpack uri

parseURI :: (MonadError Text m) => Text -> m URI
parseURI uri = maybe (throwError $ invalidURI uri) return . N.parseURI $ unpack uri
