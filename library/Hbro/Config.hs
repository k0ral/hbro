{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | General configuration parameters.
-- The recommended way to import this module is:
-- @
-- import Hbro.Config hiding(get, set)
-- import qualified Hbro.Config as Config
-- @
module Hbro.Config (
-- * Types
      Config
    , homePage_
-- * Getter/setter
    , Hbro.Config.get
    , Hbro.Config.set
) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Concurrent.STM.MonadIO

import           Lens.Micro.Platform as Lens

import           Network.URI  (URI)
import qualified Network.URI  as N
-- }}}

-- | Custom settings provided by the user
data Config = Config URI

homePage_ :: Lens' Config URI
homePage_ f (Config a) = (\b -> Config b) <$> f a

instance Describable Config where
  describe c = "Home page = " <> show (c^.homePage_)

instance Default Config where
  def = Config $ fromJust . N.parseURI $ "https://duckduckgo.com/"

get :: (MonadIO m, MonadReader r m, Has (TVar Config) r) => Lens' Config a -> m a
get l = fmap (view l) (readTVar =<< ask)

set :: (MonadIO m, MonadReader r m, Has (TVar Config) r) => Lens' Config a -> a -> m ()
set l v = void . (`modifyTVar` Lens.set l v) =<< ask
