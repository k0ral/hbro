{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
-- | Designed to be imported as @qualified@.
module Hbro.IPC
    ( routine
    , sendMessage
    ) where

-- {{{ Imports
import           Hbro.IPC.Signals
import           Hbro.Logger
import           Hbro.Prelude

import           Control.Lens        hiding (Action, Context)

import           System.ZMQ4.Monadic (Rep (..), Req (..), ZMQ, runZMQ)
import qualified System.ZMQ4.Monadic as ZMQ
-- }}}

-- | Designed to be run in a distinct thread
routine :: Text -> Signals -> ZMQ z ()
routine uri signals = do
    socket <- ZMQ.socket Rep
    io . infoM "hbro.ipc" $ "Opening IPC socket at: " ++ uri
    ZMQ.bind socket (unpack uri)

    forever $ do
      message <- receive socket
      io . debugM "hbro.ipc" $ "Received command: " ++ message

      case words message of
          []                -> send socket "ERROR Empty command"
          command:arguments -> do
              atomically $ putTMVar nextCommand (Command command, arguments)
              response' <- atomically $ takeTMVar response
              io . debugM "hbro.ipc" $ "Sending response: " ++ tshow response'
              send socket $ either ("ERROR " ++) id response'
    where
      nextCommand = signals^.nextCommandL
      response    = signals^.responseL

-- {{{ Utils
-- | Send message to given socket
send :: (ZMQ.Sender a) => ZMQ.Socket z a -> Text -> ZMQ z ()
send socket payload = ZMQ.send socket [] (encodeUtf8 payload)

-- | Send a single command to the given socket (which must be 'Rep'), and return the answer
sendMessage :: (BaseIO m)
            => Text   -- ^ Target socket URI
            -> Text   -- ^ Message
            -> m Text
sendMessage socketURI message = runZMQ $ do
    socket <- ZMQ.socket Req
    ZMQ.bind socket (unpack socketURI)
    io . debugM "hbro.ipc" $ "Sending message to IPC socket at: " ++ socketURI
    send socket message
    receive socket

-- | Wait for a message to be received from given socket
receive :: (ZMQ.Receiver a) => ZMQ.Socket z a -> ZMQ z Text
receive socket = decodeUtf8 <$> ZMQ.receive socket

-- | Same as 'sendMessage', but for all running instances of the browser.
-- sendMessageToAll :: (MonadBase IO m, MonadReader t m, HasConfig t) => String -> m [String]
-- sendMessageToAll message = do
--     dir  <- readConfig socketDir
--     getAllProcessIDs >>= mapM ((`sendMessage` message) . (`socketPath` dir))
-- }}}
