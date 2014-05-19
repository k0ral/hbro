{-# LANGUAGE TemplateHaskell #-}
module Hbro.Gui.PromptBar.Hooks (
    PromptHooks,
    onCancelledL,
    onChangedL,
    onValidatedL,
    initHooks,
    HasPromptHooks(..),
    clean,
    set,
) where

-- {{{ Imports
-- import Hbro.Error
import Hbro.Gui.PromptBar.Signals
import Hbro.Util

import Control.Concurrent.STM
import Control.Lens.Lens
import Control.Lens.Setter hiding(set)
import Control.Lens.TH
import Control.Monad hiding(when)
import Control.Monad.Reader hiding(when)

import Prelude
-- }}}

-- | No exported constructor, please use 'initHooks'
data PromptHooks m = PromptHooks
    { _onCancelled :: TMVar (Cancelled -> m ())
    , _onChanged   :: TMVar (Changed   -> m ())
    , _onValidated :: TMVar (Activated -> m ())
    }

makeLensesWith ?? ''PromptHooks $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

class HasPromptHooks n a | a -> n where _promptHooks :: Lens' a (PromptHooks n)
instance HasPromptHooks n (PromptHooks n) where _promptHooks = id

initHooks :: (MonadBase IO m) => m (PromptHooks n)
initHooks = io (PromptHooks <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO)

clean :: (MonadBase IO m, MonadReader t m, HasPromptHooks n t) => m ()
clean = do
    io . void . atomically . tryTakeTMVar =<< askl (_promptHooks.onCancelledL)
    io . void . atomically . tryTakeTMVar =<< askl (_promptHooks.onChangedL)
    io . void . atomically . tryTakeTMVar =<< askl (_promptHooks.onValidatedL)


set :: (MonadBase IO m, MonadReader t m, HasPromptHooks n t) => Lens' (PromptHooks n) (TMVar (x -> n ())) -> (x -> n ()) -> m ()
set l f = do
    hook' <- askl $ _promptHooks.l
    io . atomically $ tryTakeTMVar hook' >> putTMVar hook' f
