module Hbro.Signals.NotificationBar where

import Hbro.Util


data Signals = Signals {
    __cleanTimer :: TMVar HandlerId }
