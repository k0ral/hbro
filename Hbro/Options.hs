-- | Designed to be imported as @qualified@.
module Hbro.Options where

-- {{{ Imports
import Hbro.Default()
import Hbro.Types
import Hbro.Util

import Control.Conditional
import Control.Monad.IO.Class

import Data.Default
import Data.Functor

import Network.URI as N

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
-- }}}

description :: [OptDescr (CliOptions -> CliOptions)]
description = [
    Option ['h']     ["help"]               (NoArg (\o -> o { __help      = True }))     "Print this help.",
    Option ['q']     ["quiet"]              (NoArg (\o -> o { __quiet     = True }))     "Do not print any log.",
    Option ['v']     ["verbose"]            (NoArg (\o -> o { __verbose   = True }))     "Print detailed logs.",
    Option ['1']     ["vanilla"]            (NoArg (\o -> o { __vanilla   = True }))     "Do not read custom configuration file.",
    Option ['r']     ["recompile"]          (NoArg (\o -> o { __recompile = True }))     "Only recompile configuration.",
    Option []        ["deny-reconf"]        (NoArg id)                                   "Do not recompile configuration even if it has changed.",
    Option []        ["force-reconf"]       (NoArg id)                                   "Recompile configuration before starting the program.",
    Option []        ["dyre-debug"]         (NoArg id)                                   "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."]

help :: String
help = usageInfo "Usage: hbro [OPTIONS] [URI]" description

get :: (MonadIO m) => m CliOptions
get = io $ do
    options <- getOpt' Permute description <$> getArgs
    case options of
        (opts, input, _, []) -> return $ (foldl (flip id) def opts) { __startURI = (null $ concat input) ? Nothing ?? Just (concat input) }
        (_, _, _, _)         -> return def

getStartURI :: (MonadIO m, HasOptions a) => a -> m (Maybe URI)
getStartURI options = io $ case (_startURI options) of
    Just uri -> do
        fileURI <- doesFileExist uri
        case fileURI of
            True -> getCurrentDirectory >>= return . N.parseURIReference . ("file://" ++) . (</> uri)
            _    -> return $ N.parseURIReference uri
    _ -> return Nothing
