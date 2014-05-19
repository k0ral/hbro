{-# LANGUAGE TemplateHaskell #-}
module Hbro.IPC.Hooks where

-- {{{ Imports
import Hbro.Error
import Hbro.IPC.Signals
import Hbro.Util

import Control.Concurrent.STM
import Control.Lens hiding(Action)
import Control.Monad.Reader

import Data.Map as M
-- }}}

-- {{{ Types
type Action m   = [Argument] -> (m Response)       -- ^ Actions may depend on arguments
newtype Hooks m = Hooks (Map Command (Action m))   -- ^ Map commands to actions

data UnknownCommand = UnknownCommand Command deriving(Typeable)
instance Exception UnknownCommand
instance Show UnknownCommand where show (UnknownCommand (Command x)) = "Unknown command: " ++ x
-- }}}

-- | Bind a new command to an action
bind :: Command -> Action m -> Hooks m -> Hooks m
bind command f (Hooks hooks) = Hooks $ M.insert command f hooks


-- |
dequeue :: (MonadBase IO m, MonadCatch m, MonadThrow m) => a -> Signals -> TVar (Hooks (ReaderT a m)) -> m ()
dequeue globalContext signals bindings = forever $ do
    (command, arguments) <- io . atomically . takeTMVar $ nextCommand
    (Hooks theBindings)  <- io . atomically $ readTVar bindings

    result <- handleAll (return . Left . show) $ do
        f <- M.lookup command theBindings <!> UnknownCommand command
        runReaderT (f arguments) globalContext
    io . atomically . putTMVar response $ result
  where
    nextCommand = signals^.nextCommandL
    response    = signals^.responseL
