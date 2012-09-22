{-# LANGUAGE FlexibleContexts #-}
-- | Designed to be imported as @qualified@.
module Hbro.Socket where

-- {{{ Imports
-- import Hbro.Core
import Hbro.Util
import Hbro.Types

import Control.Monad hiding(mapM_)
import Control.Monad.Error hiding(mapM_)
-- import Control.Monad.IO.Class
import Control.Monad.Reader hiding(mapM_)
import Control.Monad.Trans.Control

import Data.ByteString.Char8 (pack, unpack)
--import Data.Foldable
import Data.Functor
import qualified Data.Map as M

import Graphics.UI.Gtk.General.General

import Prelude hiding(log, mapM_, read)

import System.FilePath
import System.Posix.Process
import System.Posix.Types
import qualified System.ZMQ as ZMQ
-- }}}

-- | Open a response-socket at configured location, named hbro.<pid>, and start listening for commands.
open :: (MonadBaseControl IO m, MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => m ()
open = do
    pid       <- io getProcessID
    socketDir <- asks _socketDir
    path      <- socketPath pid <$> io socketDir
    socket    <- io . (`ZMQ.socket` ZMQ.Rep) =<< asks _ZMQContext

    logNormal $ "Opening socket at " ++ path
    io $ ZMQ.bind socket path
    readCommands socket
    io $ ZMQ.close socket
    return ()

-- | Close the response socket by sending it the command "QUIT".
-- Typically called when exiting application.
close :: (Functor m, MonadIO m, MonadReader r m, HasConfig r, HasZMQContext r) => m ()
close = do
    uri <- getPath
    logVerbose $ "Closing socket " ++ show uri ++ "..."
    void $ sendCommand uri "QUIT"

-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
readCommands :: (Functor m, MonadIO m, MonadBaseControl IO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasZMQContext r, HasHooks r, MonadError HError m) => ZMQ.Socket ZMQ.Rep -> m ()
readCommands socket = do
    message <- read socket
    logVerbose $ "Received command: " ++ message

    case words message of
    -- Empty command
        [] -> send socket "ERROR Unknown command"
    -- Exit command
        ["QUIT"] -> send socket "OK"
    -- Valid command
        command:arguments -> do
            (CommandsList commands) <- asks _commands
            case M.lookup command (M.fromList commands) of
                Just callback -> (postGUISync' (callback arguments) >>= send socket) `catchError` (\_ -> send socket "ERROR")
                _             -> send socket "ERROR Unknown command"

            readCommands socket

postGUISync' :: (MonadBaseControl IO m) => m a -> m a
postGUISync' f = control $ \runInIO -> postGUISync (runInIO f)

-- | Return socket URI used for the current process.
getPath :: (Functor m, MonadIO m, MonadReader r m, HasConfig r) => m String
getPath = do
    dir <- asks _socketDir
    pid <- io getProcessID
    socketPath pid <$> io dir

-- | Return the socket path to use for the given browser's process ID.
socketPath :: ProcessID -> FilePath -> String
socketPath pid socketDir = "ipc://" ++ socketDir </> "hbro." ++ show pid

-- |
send :: (MonadIO m) => ZMQ.Socket a -> String -> m ()
send socket payload = io $ ZMQ.send socket (pack payload) []

read :: (MonadIO m) => ZMQ.Socket a -> m String
read socket = io $ unpack <$> ZMQ.receive socket []

-- | Send a single command (through a Request socket) to the given Response socket, and return the answer.
sendCommand :: (MonadIO m, MonadReader r m, HasZMQContext r) => String -> String -> m String
sendCommand socketURI command = do
    context <- asks _ZMQContext
    io $ ZMQ.withSocket context ZMQ.Req $ \socket -> do
      ZMQ.connect socket socketURI
      send socket command
      read socket

-- | Same as 'sendCommand', but for all running instances of the browser.
sendCommandToAll :: (MonadIO m, MonadReader r m, HasConfig r, HasZMQContext r) => String -> m [String]
sendCommandToAll command = do
    dir  <- asks _socketDir
    dir' <- io dir
    (io getAllProcessIDs) >>= mapM ((`sendCommand` command) . (`socketPath` dir'))
