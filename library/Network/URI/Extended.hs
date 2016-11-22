{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Generalized version of 'Network.URI'.
module Network.URI.Extended
    ( module X
    , parseURIReference
    , parseURI
    , UriException(..)
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Network.URI  as X hiding (parseURI, parseURIReference)
import qualified Network.URI  as N
-- }}}

-- | Generalized version of 'N.parseURIReference'.
parseURIReference :: (MonadThrow m) => Text -> m URI
parseURIReference uri = N.parseURIReference (unpack uri) `failWith` InvalidUri uri

-- | Generalized version of 'N.parseURI'.
parseURI :: (MonadThrow m) => Text -> m URI
parseURI uri = N.parseURI (unpack uri) `failWith` InvalidUri uri


data UriException = InvalidUri Text deriving(Eq, Show)
instance Exception UriException where
  displayException (InvalidUri t) = "Invalid URI: " ++ unpack t
