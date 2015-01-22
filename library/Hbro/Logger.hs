{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbro.Logger
    ( module X
    , initialize
-- * Redefinitions
    , debugM
    , errorM
    , infoM
    , warningM
) where

-- {{{ Imports
import           Hbro.Prelude

import           Control.Concurrent        (myThreadId)

import           Data.Text                 (justifyLeft, replace)

import           System.Log                as X (Priority (..))
import           System.Log.Formatter
import           System.Log.Handler.Simple
import           System.Log.Logger         (Logger, rootLoggerName, setHandlers,
                                            setLevel, updateGlobalLogger)
import qualified System.Log.Logger         as L
-- }}}

initialize :: (MonadIO m) => Priority -> m ()
initialize = io . updateGlobalLogger rootLoggerName . setup

setup :: Priority -> Logger -> Logger
setup level = setLevel level . setHandlers [logHandler]

logHandler :: GenericHandler ()
logHandler = GenericHandler
    { priority  = DEBUG
    , formatter = logFormatter "$my_time $my_prio $my_tid $msg"
    , privData  = ()
    , writeFunc = \_ t -> putStrLn $ pack t
    , closeFunc = const $ return ()
    }

logFormatter :: String -> LogFormatter a
logFormatter string handler (prio, message) = varFormatter
    [ ("my_prio", return $ formatPriority prio)
    , ("my_time", formatTime defaultTimeLocale "%F %T" <$> getCurrentTime)
    , ("my_tid",  unpack . justifyLeft 5 ' ' . replace "ThreadId " "#" . tshow <$> myThreadId)
    ]
    string handler (prio, message)

formatPriority :: Priority -> String
formatPriority WARNING   = "WARN "
formatPriority CRITICAL  = "CRIT "
formatPriority INFO      = "INFO "
formatPriority p         = unpack . justifyLeft 5 ' ' . take 5 $ tshow p

-- | Lifted 'debugM'
debugM :: (MonadIO m) => Text -> m ()
debugM a = io $ L.debugM "hbro" (unpack a)

-- | Lifted 'errorM'
errorM :: (MonadIO m) => Text -> m ()
errorM a = io $ L.errorM "hbro" (unpack a)

-- | Lifted 'infoM'
infoM :: (MonadIO m) => Text -> m ()
infoM = io . L.infoM "hbro" . unpack

-- | Lifted 'warningM'
warningM :: (MonadIO m) => Text -> m ()
warningM = io . L.warningM "hbro" . unpack
