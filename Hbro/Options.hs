{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- | Commandline options tools. Designed to be imported as @qualified@.
module Hbro.Options (
    CliOptions(),
    OptionsReader(..),
    startURI,
    socketPath,
    help,
    quiet,
    verbose,
    version,
    vanilla,
    recompile,
    denyReconf,
    forceReconf,
    dyreDebug,
    usage,
    get,
    getStartURI,
    getSocketURI)
where

-- {{{ Imports
import Hbro.Util

import Control.Conditional
import Control.Lens as L  hiding((??))
import Control.Monad.Base
import Control.Monad.Reader

import Data.Default
import Data.Functor
import Data.List
import Data.Maybe

import Network.URI as N

import Prelude hiding(log)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process
-- }}}

-- {{{ Types
-- | Available commandline options (cf @hbro -h@).
data CliOptions = CliOptions {
    _startURI      :: Maybe String,
    _socketPath    :: Maybe FilePath,
    _help          :: Bool,
    _quiet         :: Bool,
    _verbose       :: Bool,
    _version       :: Bool,
    _vanilla       :: Bool,
    _recompile     :: Bool,
    _denyReconf    :: Bool,
    _forceReconf   :: Bool,
    _dyreDebug     :: Bool}
    deriving(Eq)

makeLenses ''CliOptions

instance Show CliOptions where
    show opts = intercalate " " $ catMaybes [
        return . ("URI=" ++)    =<< view startURI opts,
        return . ("SOCKET=" ++) =<< view socketPath opts,
        view help        opts ? Just "HELP" ?? Nothing,
        view quiet       opts ? Just "QUIET" ?? Nothing,
        view verbose     opts ? Just "VERBOSE" ?? Nothing,
        view version     opts ? Just "VERSION" ?? Nothing,
        view vanilla     opts ? Just "VANILLA" ?? Nothing,
        view recompile   opts ? Just "RECOMPILE" ?? Nothing,
        view denyReconf  opts ? Just "DENY_RECONFIGURATION" ?? Nothing,
        view forceReconf opts ? Just "FORCE_RECONFIGURATION" ?? Nothing,
        view dyreDebug   opts ? Just "DYRE_DEBUG" ?? Nothing]

instance Default CliOptions where
    def = CliOptions {
        _startURI     = Nothing,
        _socketPath   = Nothing,
        _help         = False,
        _quiet        = False,
        _verbose      = False,
        _version      = False,
        _vanilla      = False,
        _recompile    = False,
        _denyReconf   = False,
        _forceReconf  = False,
        _dyreDebug    = False}

-- | 'MonadReader' for 'CliOptions'
class OptionsReader m where
    readOptions :: Simple Lens CliOptions a -> m a

instance (Monad m) => OptionsReader (ReaderT CliOptions m) where
    readOptions l = return . view l =<< ask

instance OptionsReader ((->) CliOptions) where
    readOptions l = view l
-- }}}


description :: [OptDescr (CliOptions -> CliOptions)]
description = [
    Option ['h']     ["help"]               (NoArg (set help True))                         "Print this help.",
    Option ['q']     ["quiet"]              (NoArg (set quiet True))                        "Do not print any log.",
    Option ['v']     ["verbose"]            (NoArg (set verbose True))                      "Print detailed logs.",
    Option ['V']     ["version"]            (NoArg (set version True))                      "Print version.",
    Option ['1']     ["vanilla"]            (NoArg (set vanilla True))                      "Do not read custom configuration file.",
    Option ['r']     ["recompile"]          (NoArg (set recompile True))                    "Only recompile configuration.",
    Option ['s']     ["socket"]             (ReqArg (\v -> set socketPath (Just v)) "PATH") "Where to open IPC socket",
    Option []        ["force-reconf"]       (NoArg id)                                      "Recompile configuration before starting the program.",
    Option []        ["deny-reconf"]        (NoArg id)                                      "Do not recompile configuration even if it has changed.",
    Option []        ["dyre-debug"]         (NoArg id)                                      "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."]

-- | Usage text (cf @hbro -h@)
usage :: String
usage = usageInfo "Usage: hbro [OPTIONS] [URI]" description

-- | Get and parse commandline options
get :: (MonadBase IO m) => m CliOptions
get = io $ do
    options <- getOpt' Permute description <$> getArgs
    case options of
        (opts, input, _, []) -> return $ set startURI ((null $ concat input) ? Nothing ?? Just (concat input)) (foldl (flip id) def opts)
        (_, _, _, _)         -> return def

-- | Get URI passed in commandline, check whether it is a file path or an internet URI
-- and return the corresponding normalized URI (that is: prefixed with "file://" or "http://")
getStartURI :: (MonadBase IO m, OptionsReader m) => m (Maybe URI)
getStartURI = do
    theURI <- readOptions startURI
    case theURI of
      Just uri -> do
          fileURI <- io $ doesFileExist uri
          case fileURI of
              True -> io getCurrentDirectory >>= return . N.parseURIReference . ("file://" ++) . (</> uri)
              _    -> return $ N.parseURIReference uri
      _ -> return Nothing


-- | Return socket URI used by this instance
getSocketURI :: (MonadBase IO m, OptionsReader m) => m String
getSocketURI = maybe getDefaultSocketURI (return . ("ipc://" ++)) =<< readOptions socketPath
  where
    getDefaultSocketURI = do
      dir <- io getTemporaryDirectory
      pid <- io getProcessID
      return $ "ipc://" ++ dir </> "hbro." ++ show pid
