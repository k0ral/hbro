{-# LANGUAGE TemplateHaskell, TupleSections #-}
-- | Designed to be imported as @qualified@.
module Hbro.IPC (
      routine
    , sendMessage
) where

-- {{{ Imports
import Hbro.Error
import Hbro.IPC.Signals
import Hbro.Util

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Lens hiding(Action, Context)

import Data.ByteString.UTF8 (fromString, toString)

import Prelude hiding(init, log, mapM_, read)

import System.ZMQ4.Monadic (ZMQ, Rep(..), Req(..), runZMQ)
import qualified System.ZMQ4.Monadic as ZMQ
-- }}}

-- | Designed to be run in a distinct thread
routine :: String -> Signals -> ZMQ z ()
routine uri signals = do
    socket <- ZMQ.socket Rep
    liftIO . infoM "hbro.ipc" $ "Opening IPC socket at: " ++ uri
    ZMQ.bind socket uri

    forever $ do
      message <- receive socket
      liftIO . debugM "hbro.ipc" $ "Received command: " ++ message

      case words message of
          []                -> send socket "ERROR Empty command"
          command:arguments -> do
              liftIO . atomically $ putTMVar nextCommand (Command command, arguments)
              response' <- liftIO . atomically $ takeTMVar response
              liftIO . debugM "hbro.ipc" $ "Sending response: " ++ show response'
              send socket $ either ("ERROR " ++) id response'
    where
      nextCommand = signals^.nextCommandL
      response    = signals^.responseL

-- {{{ Utils
-- | Send message to given socket
send :: (ZMQ.Sender a) => ZMQ.Socket z a -> String -> ZMQ z ()
send socket payload = ZMQ.send socket [] (fromString payload)

-- | Send a single command to the given socket (which must be 'Rep'), and return the answer
sendMessage :: (MonadBase IO m, MonadIO m)
            => String   -- ^ Target socket URI
            -> String   -- ^ Message
            -> m String
sendMessage socketURI message = runZMQ $ do
    socket <- ZMQ.socket Req
    ZMQ.bind socket socketURI
    liftIO . debugM "hbro.ipc" $ "Sending message to IPC socket at: " ++ socketURI
    send socket message
    receive socket

-- | Wait for a message to be received from given socket
receive :: (ZMQ.Receiver a) => ZMQ.Socket z a -> ZMQ z String
receive socket = toString <$> ZMQ.receive socket

-- | Same as 'sendMessage', but for all running instances of the browser.
-- sendMessageToAll :: (MonadBase IO m, MonadReader t m, HasConfig t) => String -> m [String]
-- sendMessageToAll message = do
--     dir  <- readConfig socketDir
--     getAllProcessIDs >>= mapM ((`sendMessage` message) . (`socketPath` dir))
-- }}}
