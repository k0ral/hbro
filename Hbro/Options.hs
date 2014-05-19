{-# LANGUAGE TemplateHaskell #-}
-- | Commandline options tools.
module Hbro.Options (
-- * Types
      CliOptions()
    , startURIL
    , socketPathL
    , dyreModeL
    , uiFileL
    , dyreDebugL
-- * Util
    , parseOptions
    , usage
    , getSocketURI
) where

-- {{{ Imports
import qualified Hbro.Dyre as Dyre
import Hbro.Logger as Logger
import Hbro.Util

import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.TH

import Data.Version

import Network.URI as N

import Paths_hbro
import Prelude hiding(mapM_)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process
import qualified System.ZMQ4 as ZMQ (version)

import Text.Read hiding(get, lift)
-- }}}

-- {{{ Types
-- | Available commandline options (cf @hbro -h@).
data CliOptions = CliOptions
    { _startURI   :: Maybe URI
    , _socketPath :: Maybe FilePath
    , _uiFile     :: Maybe FilePath
    , _dyreMode   :: Dyre.Mode
    , _dyreDebug  :: Bool
    } deriving(Eq)

makeLensesWith ?? ''CliOptions $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

instance Show CliOptions where
    show opts = unwords $ catMaybes
        [ return . ("URI=" ++) . show       =<< (opts^.startURIL)
        , return . ("SOCKET=" ++)           =<< (opts^.socketPathL)
        , return . ("UI_FILE=" ++)          =<< (opts^.uiFileL)
        , return . ("DYRE_MODE=" ++) . show $ opts^.dyreModeL
        , Just "DYRE_DEBUG" <| opts^.dyreDebugL |> Nothing
        ]

instance Default CliOptions where
    def = CliOptions {
        _startURI   = Nothing,
        _socketPath = Nothing,
        _uiFile     = Nothing,
        _dyreMode   = def,
        _dyreDebug  = False}
-- }}}

-- {{{ Util
flag :: (Monad m) => Lens' CliOptions a -> a -> ArgDescr (CliOptions -> m CliOptions)
flag attribute value = NoArg $ return . set attribute value

optional :: (Monad m) => Lens' CliOptions (Maybe String) -> String -> ArgDescr (CliOptions -> m CliOptions)
optional attribute name = ReqArg (\x -> return . set attribute (Just x)) name
-- }}}

description :: (MonadBase IO m, MonadPlus m) => [OptDescr (CliOptions -> m CliOptions)]
description =
-- Action
    [ Option ['h']     ["help"]               help
        "Print this help."
    , Option ['r']     ["recompile"]          recompile
        "Only recompile dyreMode"
    , Option ['V']     ["version"]            printVersion
        "Print version."
-- Log level
    , Option ['v']     ["verbose"]            (setLogLevel DEBUG)
        "Equivalent to -l DEBUG."
    , Option ['q']     ["quiet"]              (setLogLevel ERROR)
        "Equivalent to -l ERROR."
    , Option ['l']     ["log-level"]          (ReqArg setCustomLogLevel "VALUE")
        "Set log level. VALUE may be one of DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY."
-- Paths
    , Option ['s']     ["socket"]             (optional socketPathL "PATH")
        "Where to open IPC socket"
    , Option ['u']     ["ui"]                 (optional uiFileL "PATH")
        "Path to UI descriptor (XML file)"
-- Dynamic redyreMode
    , Option ['1']     ["vanilla"]            (flag dyreModeL Dyre.Vanilla)
        "Do not read custom dyreMode file"
    , Option []        ["force-reconf"]       (flag dyreModeL Dyre.ForceReconfiguration)
        "Recompile dyreMode before starting the program"
    , Option []        ["deny-reconf"]        (flag dyreModeL Dyre.IgnoreReconfiguration)
        "Do not recompile dyreMode even if it has changed"
    , Option []        ["dyre-debug"]         (NoArg return)
        "Use './cache/' as the cache directory and ./ as the dyreMode directory. Useful to debug the program."
    ]


help, printVersion, recompile :: (MonadBase IO m, MonadPlus m) => ArgDescr (a -> m b)
help         = NoArg . const $ io (putStrLn usage) >> abort
recompile    = NoArg . const $ Dyre.recompile >>= mapM_ (io . putStrLn) >> abort
printVersion = NoArg . const $ do
    (a, b, c) <- io ZMQ.version
    io . putStrLn $ "hbro: v" ++ showVersion version
    io . putStrLn $ "0MQ library: v" ++ intercalate "." (map show [a, b, c])
    abort

setLogLevel :: (MonadBase IO m, MonadPlus m) => Priority -> ArgDescr (a -> m a)
setLogLevel priority = NoArg $ \options -> do
    Logger.initialize priority
    return options

setCustomLogLevel :: (MonadBase IO m) => String -> (a -> m a)
setCustomLogLevel level options = do
    maybe (io $ errorM "hbro.options" message) Logger.initialize $ readMaybe level
    return options
  where message = "Invalid log level '" ++ level ++ "'."

-- | Usage text (cf @hbro -h@)
usage :: String
usage = usageInfo "Usage: hbro [OPTIONS] URI" (description :: [OptDescr (CliOptions -> IO CliOptions)])

-- | Get and parse commandline options
parseOptions :: (MonadBase IO m, MonadPlus m) => m CliOptions
parseOptions = do
    Logger.initialize INFO
    (opts, input, unknown, errors) <- io $ getOpt' RequireOrder description <$> getArgs

    unless (null errors)  $ io (errorM "hbro.options" (unlines errors)) >> abort
    unless (null unknown) . io . infoM "hbro.options" $ "Unrecognized options: " ++ unwords unknown
    unless (null $ tailSafe input) . io . infoM "hbro.options" $ "Ignored input: " ++ unlines (tailSafe input)

    options <- foldl (>>=) (return def) opts >>= parseInput (listToMaybe input)
    io . debugM "hbro.options" $ "Used options: " ++ show options
    return options

-- | Get URI passed in commandline, check whether it is a file path or an internet URI
-- and return the corresponding normalized URI (that is: prefixed with "file://" or "http://")
parseInput :: (MonadBase IO m) => Maybe String -> CliOptions -> m CliOptions
parseInput (Just uri) options = do
    fileURI    <- io $ doesFileExist uri
    currentDir <- io getCurrentDirectory
    let parsedURI = case fileURI of
                      True -> N.parseURIReference $ "file://" ++ (currentDir </> uri)
                      _    -> N.parseURIReference uri
    unless (isJust parsedURI) . io . errorM "hbro.options" $ "Invalid URI: " ++ uri
    return $ set startURIL parsedURI options
parseInput Nothing options = return options


-- | Return socket URI used by this instance
getSocketURI :: (MonadBase IO m) => CliOptions -> m String
getSocketURI options = maybe getDefaultSocketURI (return . ("ipc://" ++)) $ options^.socketPathL
  where
    getDefaultSocketURI = do
      dir <- io getTemporaryDirectory
      pid <- io getProcessID
      return $ "ipc://" ++ dir </> "hbro." ++ show pid
