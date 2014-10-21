{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
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
import           Hbro.Prelude

import           Control.Lens hiding (Action, Context)
-- }}}


-- {{{ Types
-- | Commands are mere 'Text's
newtype Command = Command Text deriving(Eq, Ord)

-- | Arguments are 'Text's too
type Argument = Text

-- | Responses may be OK or KO
type Response = Either Text Text
-- type Queue    = [(Command, [Argument])]

declareLenses [d|
  data Signals = Signals
    { nextCommandL :: TMVar (Command, [Argument])
    , responseL    :: TMVar Response
    }
  |]
-- }}}

initSignals :: (BaseIO m) => m Signals
initSignals = io (Signals <$> newEmptyTMVarIO <*> newEmptyTMVarIO)
