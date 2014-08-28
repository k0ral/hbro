module Hbro.Signals.NotificationBar where

import           Hbro.Prelude


data Signals = Signals {
    __cleanTimer :: TMVar HandlerId }
