{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | General configuration parameters.
-- The recommended way to import this module is:
-- @
-- import Hbro.Config hiding(get, set)
-- import qualified Hbro.Config as Config
-- @
module Hbro.Config (
-- * Types
      Config
    , homePageL
-- * Getter/setter
    , get
    , set
) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Lens hiding (set)
import qualified Control.Lens as L (set)

import           Network.URI  (URI)
import qualified Network.URI  as N
-- }}}

-- | Custom settings provided by the user
declareLenses [d|
  data Config = Config
    { homePageL :: URI
    }
  |]

instance Describable Config where
    describe c = "Home page = " ++ tshow (c^.homePageL)

instance Default Config where
    def = Config $ fromJust . N.parseURI $ "https://duckduckgo.com/"

get :: (MonadIO m, MonadReader r m, Has (TVar Config) r) => Lens' Config a -> m a
get l = return . view l =<< atomically . readTVar =<< ask

set :: (MonadIO m, MonadReader r m, Has (TVar Config) r) => Lens' Config a -> a -> m ()
set l v = atomically . (`modifyTVar` L.set l v) =<< ask
