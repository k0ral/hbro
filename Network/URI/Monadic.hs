{-# LANGUAGE DeriveDataTypeable #-}
-- | Functions from 'Network.URI' rewritten with 'MonadThrow' instead of 'Maybe'.
module Network.URI.Monadic
    ( module X
    , InvalidURI(..)
    , parseURIReference
    , parseURI
) where

-- {{{ Imports
import Control.Monad.Catch

import Data.Typeable

import Network.URI as X hiding(parseURIReference, parseURI)
import qualified Network.URI as N
-- }}}

data InvalidURI = InvalidURI String deriving(Typeable)
instance Exception InvalidURI
instance Show InvalidURI where show (InvalidURI uri) = "Invalid URI: " ++ uri

parseURIReference :: (MonadThrow m) => String -> m URI
parseURIReference uri = maybe (throwM $ InvalidURI uri) return $ N.parseURIReference uri


parseURI :: (MonadThrow m) => String -> m URI
parseURI uri = maybe (throwM $ InvalidURI uri) return $ N.parseURI uri
