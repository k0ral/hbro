{-# LANGUAGE TemplateHaskell, TupleSections #-}
-- | Designed to be imported as @qualified@.
module Hbro.IPC.Signals
    ( Argument
    , Response
    , Command(..)
-- * Signals
    , Signals
    , nextCommandL
    , responseL
    , initSignals
) where

-- {{{ Imports
-- import Hbro.Error
import Hbro.Util

import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO)
import Control.Lens hiding(Action, Context)

import Prelude hiding(init, log, mapM_, read)
-- }}}


-- {{{ Types
-- | Commands are mere 'String's
newtype Command = Command String deriving(Eq, Ord)

-- | Arguments are 'String's too
type Argument = String

-- | Responses may be OK or KO
type Response = Either String String
-- type Queue    = [(Command, [Argument])]

data Signals = Signals
    { _nextCommand :: TMVar (Command, [Argument])
    , _response    :: TMVar Response
    }

makeLensesWith ?? ''Signals $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))
-- }}}

initSignals :: (MonadBase IO m) => m Signals
initSignals = io (Signals <$> newEmptyTMVarIO <*> newEmptyTMVarIO)
