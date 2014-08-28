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
import           Hbro.Gui.PromptBar.Signals
import           Hbro.Prelude

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter        hiding (set)
import           Control.Lens.TH
import           Control.Monad.Reader       hiding (when)
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

initHooks :: (BaseIO m) => m (PromptHooks n)
initHooks = io (PromptHooks <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO)

clean :: (BaseIO m, MonadReader t m, HasPromptHooks n t) => m ()
clean = do
    hooks <- askL _promptHooks
    void . atomically $ do
        tryTakeTMVar $ hooks^.onCancelledL
        tryTakeTMVar $ hooks^.onChangedL
        tryTakeTMVar $ hooks^.onValidatedL


set :: (BaseIO m, MonadReader t m, HasPromptHooks n t) => Lens' (PromptHooks n) (TMVar (x -> n ())) -> (x -> n ()) -> m ()
set l f = do
    hook' <- askL $ _promptHooks.l
    atomically $ writeTMVar hook' f
