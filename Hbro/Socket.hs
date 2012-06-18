module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Core hiding(getURI)
import Hbro.Util
import Hbro.Types

import Control.Concurrent
import Control.Monad hiding(mapM_)

import Data.ByteString.Char8 (pack, unpack)
--import Data.Foldable
import qualified Data.Map as M

import Prelude hiding(log, mapM_)

import System.FilePath
import System.Posix.Process
import System.Posix.Types
import System.ZMQ 
-- }}}
    
-- | Open a response-socket at configured location, named hbro.<pid>, and start listening for commands    
open :: K ()    
open = do
-- Resolve socket URI    
    pid       <- io getProcessID
    socketURI <- with (mSocketDir . mConfig) $ resolve >=> (return . (socketFile pid))
-- Open socket and listen to commands
    mapK (void . forkIO) $ withK mContext $ \context -> do
        logNormal $ "Opening socket at " ++ socketURI
        mapK2 (withSocket context Rep) $ \sock -> do
            io $ bind sock socketURI
            readCommands sock


-- | Close the response socket by sending it the command "QUIT".
-- Typically called when exiting application.            
close :: K ()
close = getURI >>= \uri -> do 
    logVerbose . ("Closing socket " ++) . (++ " ...") $ uri
    void . (`sendCommand` "QUIT") $ uri


-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
readCommands :: Socket Rep -> K ()
readCommands sock = do
    message <- io $ unpack `fmap` receive sock []

    case words message of
    -- Empty command
        [] -> io $ send sock (pack "ERROR Unknown command") []
    -- Exit command
        ["QUIT"] -> io $ do
            logVerbose "Receiving QUIT command"
            send sock (pack "OK") []
    -- Valid command
        command:arguments -> withK (M.fromList . mCommands . mConfig) $ \commands -> do
            logVerbose . ("Receiving command: " ++) $ message
            case M.lookup command commands of
                Just callback -> callback arguments >>= io . (send'' sock) . pack
                _             -> io $ send sock (pack "ERROR Unknown command") []

            readCommands sock
        
getURI :: K String
getURI = with (mSocketDir . mConfig) $ \dir -> do
    dir' <- resolve dir
    (`socketFile` dir') `fmap` getProcessID
        
-- | Return the socket path to use for the given browser's process ID.
socketFile :: ProcessID -> String -> String
socketFile pid socketDir = "ipc://" ++ socketDir </> "hbro." ++ show pid
  
-- | Send a single command (through a Request socket) to the given Response socket,
-- and return the answer.
sendCommand :: String -> String -> K String
sendCommand socketURI command = with mContext $ \context -> withSocket context Req $ \sock -> do
    connect sock socketURI
    send sock (pack command) []
    unpack `fmap` receive sock []
        
-- | Same as 'sendCommand', but for all running instances of the browser.
sendCommandToAll :: String -> K [String]
sendCommandToAll command = withK (mSocketDir . mConfig) $ \dir -> do
    dir' <- io $ resolve dir
    (io getAllProcessIDs) >>= mapM ((`sendCommand` command) . (`socketFile` dir'))
