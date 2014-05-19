{-# LANGUAGE OverloadedStrings #-}
module Hbro.Logger
    ( module X
    , initialize
) where

-- {{{ Imports
import Hbro.Util

import Control.Concurrent (myThreadId)

import Data.Text as Text (justifyLeft, pack, replace, take, unpack)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)

import System.Locale (defaultTimeLocale)
import System.Log as X (Priority(..))
import System.Log.Formatter
import System.Log.Handler.Simple
-- }}}

initialize :: (MonadBase IO m) => Priority -> m ()
initialize = io . updateGlobalLogger rootLoggerName . setup

setup :: Priority -> Logger -> Logger
setup level = setLevel level . setHandlers [logHandler]

logHandler :: GenericHandler ()
logHandler = GenericHandler
    { priority  = DEBUG
    , formatter = logFormatter "$my_time $my_prio $my_tid $msg"
    , privData  = ()
    , writeFunc = \_ t -> putStrLn t
    , closeFunc = \_ -> return ()
    }

logFormatter :: String -> LogFormatter a
logFormatter string handler (prio, message) loggername = varFormatter
    [ ("my_prio", return $ formatPriority prio)
    , ("my_time", formatTime defaultTimeLocale "%F %T" <$> getCurrentTime)
    , ("my_tid",  Text.unpack . justifyLeft 5 ' ' . replace "ThreadId " "#" . Text.pack . show <$> myThreadId)
    ]
    string handler (prio, message) loggername

formatPriority :: Priority -> String
formatPriority WARNING   = "WARN "
formatPriority CRITICAL  = "CRIT "
formatPriority INFO      = "INFO "
formatPriority p         = Text.unpack . justifyLeft 5 ' ' . Text.take 5 . Text.pack $ show p
