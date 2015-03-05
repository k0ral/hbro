{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Monadic version of 'Network.URI'.
module Network.URI.Extended
    ( module X
    , parseURIReferenceM
    , parseURIM
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Data.Aeson

import           Network.URI  as X
-- }}}

instance FromJSON URI where
    parseJSON (String t) = maybe mzero return . parseURIReference $ unpack t
    parseJSON _ = mzero

instance ToJSON URI where
    toJSON = String . tshow

-- | Error message
invalidURI :: Text -> Text
invalidURI uri = "Invalid URI: " ++ uri

-- | Monadic version of 'parseURIReference'
parseURIReferenceM :: (MonadError Text m) => Text -> m URI
parseURIReferenceM uri = parseURIReference (unpack uri) `failWith` invalidURI uri

-- | Monadic version of 'parseURI'
parseURIM :: (MonadError Text m) => Text -> m URI
parseURIM uri = parseURI (unpack uri) `failWith` invalidURI uri
