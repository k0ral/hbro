module Hbro.IPC.Hooks where

-- {{{ Imports
import           Hbro.Error
import           Hbro.IPC.Signals
import           Hbro.Prelude

import           Control.Lens         hiding (Action)

import           Data.Map             as M
-- }}}

-- {{{ Types
type Action m   = [Argument] -> m Response         -- ^ Actions may depend on arguments
newtype Hooks m = Hooks (Map Command (Action m))   -- ^ Map commands to actions
-- }}}

-- | Bind a new command to an action
bind :: Command -> Action m -> Hooks m -> Hooks m
bind command f (Hooks hooks) = Hooks $ M.insert command f hooks

-- |
dequeue :: (BaseIO m) => a -> Signals -> TVar (Hooks (ExceptT Text (ReaderT a m))) -> m ()
dequeue globalContext signals bindings = forever $ do
    (Command c, arguments) <- atomically . takeTMVar $ nextCommand
    (Hooks theBindings)    <- atomically $ readTVar bindings

    -- result <- (`runReaderT` globalContext) . handleAll (return . Left . show) $ do
    result <- (`runReaderT` globalContext) . runExceptT $ do
        f <- return (M.lookup (Command c) theBindings) <!> ("Unknown command: " ++ c)
        f arguments
    atomically . putTMVar response $ join result
  where
    nextCommand = signals^.nextCommandL
    response    = signals^.responseL
