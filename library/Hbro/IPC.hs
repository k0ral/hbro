{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Designed to be imported as @qualified@.
module Hbro.IPC
    ( CommandMap
    , bindCommands
    -- , sendMessage
    ) where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Data.Function (fix)
import qualified Data.Map      as Map

import           System.ZMQ4   (Receiver, Rep (..), Sender, Socket)
import qualified System.ZMQ4   as ZMQ
-- }}}

-- | Commands are mere 'Text's
type Command = Text

-- | Arguments are 'Text's too
type Argument = Text

-- | Responses may be OK or KO
type Response = Either Text Text

type CommandMap m = Map Command ([Argument] -> m Response)

withContext :: ControlIO m => (ZMQ.Context -> m a) -> m (StM m a)
withContext f = liftBaseWith $ \runInBase -> ZMQ.withContext (runInBase . f)

withSocket :: (ZMQ.SocketType s, ControlIO m) => ZMQ.Context -> s -> (Socket s -> m a) -> m (StM m a)
withSocket c s f = liftBaseWith $ \runInBase -> ZMQ.withSocket c s (runInBase . f)

bindCommands :: (ControlIO m, MonadLogger m) => Text -> CommandMap m -> m ()
bindCommands uri commandMap = void . withContext $ \c -> withSocket c Rep $ \socket -> do
  info $ "Opening IPC socket at: " ++ uri
  io $ ZMQ.bind socket (unpack uri)

  fix $ \recurse -> do
    message <- receive socket
    debug $ "Received command: " ++ message

    case words message of
      [] -> send socket "ERROR Empty command"
      command:arguments -> do
        response <- case Map.lookup command commandMap of
                      Just f -> either ("ERROR " ++) id <$> f arguments
                      _ -> return "ERROR Unknown command"
        debug $ "Sending response: " ++ tshow response
        send socket response
    recurse


-- {{{ Utils
-- | Send message to given socket
send :: (BaseIO m, Sender a) => Socket a -> Text -> m ()
send socket = io . ZMQ.send socket [] . encodeUtf8

-- | Send a single command to the given socket (which must be 'Rep'), and return the answer
-- sendMessage :: (BaseIO m)
--             => Context
--             -> Text   -- ^ Target socket URI
--             -> Text   -- ^ Message
--             -> m Text
-- sendMessage context socketURI message = do
--     socket <- io $ ZMQ.socket context Req
--     io . ZMQ.bind socket $ unpack socketURI
--     debug $ "Sending message to IPC socket at: " ++ socketURI
--     io . ZMQ.send socket [] $ encodeUtf8 message
--     decodeUtf8 <$> io (ZMQ.receive socket)

-- | Wait for a message to be received from given socket
receive :: (BaseIO m, Receiver a) => Socket a -> m Text
receive socket = decodeUtf8 <$> io (ZMQ.receive socket)

-- | Same as 'sendMessage', but for all running instances of the browser.
-- sendMessageToAll :: (MonadBase IO m, MonadReader t m, HasConfig t) => String -> m [String]
-- sendMessageToAll message = do
--     dir  <- readConfig socketDir
--     getAllProcessIDs >>= mapM ((`sendMessage` message) . (`socketPath` dir))
-- }}}
