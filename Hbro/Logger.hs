module Hbro.Logger
    ( module X
    , initialize
) where

-- {{{ Imports
import Hbro.Util

import System.Log as X (Priority(..))
import System.Log.Formatter
-- import System.Log.Handler hiding(setLevel)
import System.Log.Handler.Simple
-- import System.Log.Logger
-- }}}

initialize :: (MonadBase IO m) => Priority -> m ()
initialize = io . updateGlobalLogger rootLoggerName . setup

setup :: Priority -> Logger -> Logger
setup level = setLevel level . setHandlers [logHandler]

logHandler :: GenericHandler ()
logHandler = GenericHandler
    { priority  = DEBUG
    , formatter = simpleLogFormatter "$time | $prio | $tid | $msg"
    , privData  = ()
    , writeFunc = \_ t -> putStrLn t
    , closeFunc = \_ -> return ()
    }
