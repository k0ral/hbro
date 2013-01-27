{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- | Commandline options tools. Designed to be imported as @qualified@.
module Hbro.Options (
    CliOptions(),
    OptionsReader(..),
    startURI,
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
    getStartURI)
where

-- {{{ Imports
import Hbro.Util

import Control.Conditional
import Control.Lens as L  hiding((??))
import Control.Monad.Base
import Control.Monad.Reader

import Data.Default
import Data.Functor

import Network.URI as N

import Prelude hiding(log)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
-- }}}

-- {{{ Types
-- | Available commandline options (cf @hbro -h@).
data CliOptions = CliOptions {
    _startURI      :: Maybe String,
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
    show opts =
        (maybe "" ("URI=" ++) $ view startURI opts)
        ++ (view help        opts ? " HELP" ?? "")
        ++ (view quiet       opts ? " QUIET" ?? "")
        ++ (view verbose     opts ? " VERBOSE" ?? "")
        ++ (view version     opts ? " VERSION" ?? "")
        ++ (view vanilla     opts ? " VANILLA" ?? "")
        ++ (view recompile   opts ? " RECOMPILE" ?? "")
        ++ (view denyReconf  opts ? " DENY_RECONFIGURATION" ?? "")
        ++ (view forceReconf opts ? " FORCE_RECONFIGURATION" ?? "")
        ++ (view dyreDebug   opts ? " DYRE_DEBUG" ?? "")

instance Default CliOptions where
    def = CliOptions {
        _startURI     = Nothing,
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
    Option ['h']     ["help"]               (NoArg (\o -> o { _help      = True }))     "Print this help.",
    Option ['q']     ["quiet"]              (NoArg (\o -> o { _quiet     = True }))     "Do not print any log.",
    Option ['v']     ["verbose"]            (NoArg (\o -> o { _verbose   = True }))     "Print detailed logs.",
    Option ['V']     ["version"]            (NoArg (\o -> o { _version   = True }))     "Print version.",
    Option ['1']     ["vanilla"]            (NoArg (\o -> o { _vanilla   = True }))     "Do not read custom configuration file.",
    Option ['r']     ["recompile"]          (NoArg (\o -> o { _recompile = True }))     "Only recompile configuration.",
    Option []        ["deny-reconf"]        (NoArg id)                                  "Do not recompile configuration even if it has changed.",
    Option []        ["force-reconf"]       (NoArg id)                                  "Recompile configuration before starting the program.",
    Option []        ["dyre-debug"]         (NoArg id)                                  "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."]

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
