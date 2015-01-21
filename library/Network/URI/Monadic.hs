{-# LANGUAGE FlexibleContexts #-}
-- | Monadic version of 'Network.URI'.
module Network.URI.Monadic
    ( module X
    , parseURIReference
    , parseURI
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Network.URI  as X hiding (parseURI, parseURIReference)
import qualified Network.URI  as N
-- }}}

-- | Error message
invalidURI :: Text -> Text
invalidURI uri = "Invalid URI: " ++ uri

parseURIReference :: (MonadError Text m) => Text -> m URI
parseURIReference uri = (N.parseURIReference $ unpack uri) `failWith` invalidURI uri

parseURI :: (MonadError Text m) => Text -> m URI
parseURI uri = (N.parseURI $ unpack uri) `failWith` invalidURI uri
