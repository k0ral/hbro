{-# LANGUAGE OverloadedStrings #-}
module Hbro.Logger
    ( module X
    , initialize
-- * Redefinitions
    , debugM
    , errorM
    , infoM
) where

-- {{{ Imports
import Hbro.Prelude

import Control.Concurrent (myThreadId)

import Data.Text (justifyLeft, replace)

import System.Log as X (Priority(..))
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger (Logger, rootLoggerName, setLevel, setHandlers, updateGlobalLogger)
import qualified System.Log.Logger as L
-- }}}

initialize :: (BaseIO m) => Priority -> m ()
initialize = io . updateGlobalLogger rootLoggerName . setup

setup :: Priority -> Logger -> Logger
setup level = setLevel level . setHandlers [logHandler]

logHandler :: GenericHandler ()
logHandler = GenericHandler
    { priority  = DEBUG
    , formatter = logFormatter "$my_time $my_prio $my_tid $msg"
    , privData  = ()
    , writeFunc = \_ t -> putStrLn $ pack t
    , closeFunc = \_ -> return ()
    }

logFormatter :: String -> LogFormatter a
logFormatter string handler (prio, message) loggername = varFormatter
    [ ("my_prio", return $ formatPriority prio)
    , ("my_time", formatTime defaultTimeLocale "%F %T" <$> getCurrentTime)
    , ("my_tid",  unpack . justifyLeft 5 ' ' . replace "ThreadId " "#" . tshow <$> myThreadId)
    ]
    string handler (prio, message) loggername

formatPriority :: Priority -> String
formatPriority WARNING   = "WARN "
formatPriority CRITICAL  = "CRIT "
formatPriority INFO      = "INFO "
formatPriority p         = unpack . justifyLeft 5 ' ' . take 5 $ tshow p

-- | Better version of 'debugM'
debugM :: (BaseIO m) => Text -> Text -> m ()
debugM a b = io $ L.debugM (unpack a) (unpack b)

-- | Better version of 'errorM'
errorM :: (BaseIO m) => Text -> Text -> m ()
errorM a b = io $ L.errorM (unpack a) (unpack b)

-- | Better version of 'infoM'
infoM :: (BaseIO m) => Text -> Text -> m ()
infoM a b = io $ L.infoM (unpack a) (unpack b)
