module Hbro.Socket where
    
-- {{{ Imports
import Hbro.Util
import Hbro.Types

import Control.Monad hiding(mapM_)

import Data.ByteString.Char8 (pack, unpack)
--import Data.Foldable
import qualified Data.Map as Map

import Prelude hiding(mapM_)

import System.Console.CmdArgs (whenNormal, whenLoud)
import System.FilePath
import System.ZMQ 
-- }}}
    
-- | Create a response socket to listen for commands.
-- Loops on listenToSocket until "Quit" command is received.
openRepSocket :: Context -> String -> (Socket Rep -> IO ()) -> IO ()
openRepSocket context socketURI listen = do    
    whenNormal $ putStrLn ("Opening socket at " ++ socketURI)
    withSocket context Rep $ \repSocket -> do
        bind repSocket socketURI
        listen repSocket

-- | Listen for incoming requests from response socket.
-- Parse received commands and feed the corresponding callback, if any.
listenToCommands :: Environment -> CommandsMap -> Socket Rep -> IO ()
listenToCommands environment commands repSocket = do
    message      <- receive repSocket []
    let message' =  unpack message

    case words message' of
    -- Empty command
        [] -> send repSocket (pack "ERROR Unknown command") []
    -- Exit command
        ["QUIT"] -> do
            whenLoud $ putStrLn "Receiving QUIT command"
            send repSocket (pack "OK") []
    -- Valid command
        command:arguments -> do
            whenLoud $ putStrLn ("Receiving command: " ++ message')
            case Map.lookup command commands of
                Just callback -> callback arguments repSocket environment
                _             -> send repSocket (pack "ERROR Unknown command") []

            listenToCommands environment commands repSocket
        
-- | Close the response socket by sending it the command "QUIT".
-- Typically called when exiting application.            
closeSocket :: Context -> String -> IO ()
closeSocket context socketURI = do
    whenLoud $ putStrLn ("Closing socket " ++ socketURI ++ " ...")
    void $ sendCommand context socketURI "QUIT"
        
-- | Return the socket path to use for the given browser's process ID.
socketFile :: String -> String -> String
socketFile pid socketDir = "ipc://" ++ socketDir </> "hbro." ++ pid
  
-- | Send a single command (through a Request socket) to the given Response socket,
-- and return the answer.
sendCommand :: Context -> String -> String -> IO String
sendCommand context socketURI command = withSocket context Req $ \reqSocket -> do
    connect reqSocket socketURI
    send reqSocket (pack command) []
    receive reqSocket [] >>= return . unpack
        
-- | Same as 'sendCommand', but for all running instances of the browser.
sendCommandToAll :: Context -> FilePath -> String -> IO [String]
sendCommandToAll context socketDir command = getAllProcessIDs >>= mapM (\pid -> sendCommand context (socketFile pid socketDir) command)
