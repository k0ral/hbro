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

import           Data.Aeson

import           Network.URI  as X hiding (parseURI, parseURIReference)
import qualified Network.URI  as N
-- }}}

instance FromJSON URI where
    parseJSON (String t) = maybe mzero return $ parseURIReference t
    parseJSON _ = mzero

instance ToJSON URI where
    toJSON = String . tshow

-- | Generalized version of 'N.parseURIReference'.
parseURIReference :: (MonadThrow m) => Text -> m URI
parseURIReference uri = N.parseURIReference (unpack uri) `failWith` InvalidUri uri

-- | Generalized version of 'N.parseURI'.
parseURI :: (MonadThrow m) => Text -> m URI
parseURI uri = N.parseURI (unpack uri) `failWith` InvalidUri uri


data UriException = InvalidUri Text deriving(Eq)
instance Exception UriException

instance Show UriException where
  show (InvalidUri t) = "Invalid URI: " ++ unpack t
