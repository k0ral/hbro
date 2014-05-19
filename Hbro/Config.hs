{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
-- | General configuration parameters.
-- The recommended way to import this module is:
-- @
-- import Hbro.Config hiding(get, set)
-- import qualified Hbro.Config as Config (get, set)
-- @
module Hbro.Config
    (
-- * Types
      Config
    , homePageL
    , HasConfig(..)
-- * Getter/setter
    , get
    , set
) where

-- {{{ Imports
import Hbro.Util

import Control.Concurrent.STM
import Control.Lens hiding(set)
import qualified Control.Lens as L (set)
import Control.Monad.Reader

import Network.URI (URI)
import qualified Network.URI as N

import Prelude hiding(mapM_, read)
-- }}}

-- | Custom settings provided by the user
data Config = Config
    { _homePage :: URI
    }

makeLensesWith ?? ''Config $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))


instance Show Config where
    show c = "Home page = " ++ (show $ c^.homePageL)

instance Default Config where
    def = Config
        { _homePage = fromJust . N.parseURI $ "https://duckduckgo.com/"
        }

class HasConfig t where _config :: Lens' t (TVar Config)

instance HasConfig (TVar Config) where _config = id

get :: (MonadReader r m, MonadBase IO m, HasConfig r) => Lens' Config a -> m a
get l = return . view l =<< io . atomically . readTVar =<< askl _config

set :: (MonadReader r m, MonadBase IO m, HasConfig r) => Lens' Config a -> a -> m ()
set l v = io . atomically . (`modifyTVar` L.set l v) =<< askl _config
