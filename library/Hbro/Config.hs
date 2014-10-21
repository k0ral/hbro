{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
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
    , HasConfig(..)
-- * Getter/setter
    , get
    , set
) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Lens         hiding (set)
import qualified Control.Lens         as L (set)

import           Network.URI          (URI)
import qualified Network.URI          as N
-- }}}

-- | Custom settings provided by the user
declareLenses [d|
  data Config = Config
    { homePageL :: URI
    }
  |]

instance Describable Config where
    describe c = "Home page = " ++ (tshow $ c^.homePageL)

instance Default Config where
    def = Config $ fromJust . N.parseURI $ "https://duckduckgo.com/"

class HasConfig t where _config :: Lens' t (TVar Config)

instance HasConfig (TVar Config) where _config = id

get :: (MonadReader r m, BaseIO m, HasConfig r) => Lens' Config a -> m a
get l = return . view l =<< atomically . readTVar =<< askL _config

set :: (MonadReader r m, BaseIO m, HasConfig r) => Lens' Config a -> a -> m ()
set l v = atomically . (`modifyTVar` L.set l v) =<< askL _config
