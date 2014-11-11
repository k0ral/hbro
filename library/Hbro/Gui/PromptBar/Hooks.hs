{-# LANGUAGE TemplateHaskell #-}
module Hbro.Gui.PromptBar.Hooks
    ( PromptHooks
    , onCancelledL
    , onChangedL
    , onValidatedL
    , initHooks
    , HasPromptHooks(..)
    , clean
    , set
    ) where

-- {{{ Imports
-- import Hbro.Error
import           Hbro.Event
import           Hbro.Gui.PromptBar.Signals
import           Hbro.Prelude

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.TH
-- }}}

-- | No exported constructor, please use 'initHooks'
declareLenses [d|
  data PromptHooks m = PromptHooks
    { onCancelledL :: TMVar (Hook m Cancelled)
    , onChangedL   :: TMVar (Hook m Changed)
    , onValidatedL :: TMVar (Hook m Activated)
    }
  |]

class HasPromptHooks n a | a -> n where _promptHooks :: Lens' a (PromptHooks n)
instance HasPromptHooks n (PromptHooks n) where _promptHooks = id

initHooks :: (BaseIO m) => m (PromptHooks n)
initHooks = io (PromptHooks <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO)

clean :: (BaseIO m, MonadReader t m, HasPromptHooks n t) => m ()
clean = do
    hooks <- askL _promptHooks
    void . atomically $ do
        tryTakeTMVar $ hooks^.onCancelledL
        tryTakeTMVar $ hooks^.onChangedL
        tryTakeTMVar $ hooks^.onValidatedL


set :: (BaseIO m, MonadReader t m, HasPromptHooks n t) => Lens' (PromptHooks n) (TMVar (Hook n x)) -> Hook n x -> m ()
set l f = do
    hook' <- askL $ _promptHooks.l
    atomically $ writeTMVar hook' f
