{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
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
import qualified Hbro.Dyre             as Dyre
import           Hbro.Error
import           Hbro.Logger           as Logger
import           Hbro.Prelude

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter
import           Control.Lens.TH

import           Data.Version

import           Filesystem

import           Network.URI.Monadic

import           Paths_hbro

import           System.Console.GetOpt
import           System.Posix.Process
import qualified System.ZMQ4           as ZMQ (version)

import           Text.Read             hiding (get, lift)
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

instance Describable CliOptions where
    describe opts = unwords $ catMaybes
        [ ("URI=" ++) . tshow <$> (opts^.startURIL)
        , ("SOCKET=" ++) . fpToText  <$> (opts^.socketPathL)
        , ("UI_FILE=" ++) . fpToText <$> (opts^.uiFileL)
        , Just . ("DYRE_MODE=" ++) . tshow $ opts^.dyreModeL
        , Just "DYRE_DEBUG" <| opts^.dyreDebugL |> Nothing
        ]

instance Default CliOptions where
    def = CliOptions
        { _startURI   = Nothing
        , _socketPath = Nothing
        , _uiFile     = Nothing
        , _dyreMode   = def
        , _dyreDebug  = False
        }
-- }}}

-- {{{ Util
action :: (BaseIO m, MonadPlus m) => m () -> ArgDescr (a -> m b)
action f = NoArg . const $ f >> abort

flag :: (Monad m) => Lens' CliOptions a -> a -> ArgDescr (CliOptions -> m CliOptions)
flag attribute value = NoArg $ return . set attribute value

optional :: (Monad m, IsString s) => Lens' CliOptions (Maybe s) -> String -> ArgDescr (CliOptions -> m CliOptions)
optional attribute name = ReqArg (\x -> return . set attribute (Just $ fromString x)) name
-- }}}

description :: (BaseIO m, MonadPlus m) => [OptDescr (CliOptions -> m CliOptions)]
description =
-- Action
    [ Option ['h']     ["help"]               (action help)
        "Print this help."
    , Option ['r']     ["recompile"]          (action recompile)
        "Recompile configuration file and exit."
    , Option ['V']     ["version"]            (action printVersion)
        "Print version."
-- Log level
    , Option ['v']     ["verbose"]            (setLogLevel DEBUG)
        "Equivalent to -l DEBUG."
    , Option ['q']     ["quiet"]              (setLogLevel ERROR)
        "Equivalent to -l ERROR."
    , Option ['l']     ["log-level"]          (ReqArg setCustomLogLevel "VALUE")
        "Set log level. VALUE may be one of DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY."
-- Paths
    , Option ['u']     ["uri"]                (ReqArg setStartURI "URI")
        "URI to load at start-up."
    , Option ['S']     ["socket"]             (optional socketPathL "PATH")
        "Where to open IPC socket"
    , Option ['U']     ["ui"]                 (optional uiFileL "PATH")
        "Path to UI descriptor (XML file)"
-- Dynamic reconfiguration
    , Option ['1']     ["vanilla"]            (flag dyreModeL Dyre.Vanilla)
        "Do not read custom configuration file"
    , Option []        ["force-reconf"]       (flag dyreModeL Dyre.ForceReconfiguration)
        "Recompile configuration file before starting the program"
    , Option []        ["dyre-master-binary"] (ReqArg (const return) "PATH")
        "Internal flag used for dynamic reconfiguration."
    , Option []        ["deny-reconf"]        (flag dyreModeL Dyre.IgnoreReconfiguration)
        "Do not recompile configuration file even if it has changed"
    , Option []        ["dyre-debug"]         (NoArg return)
        "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."
    ]


help, printVersion, recompile :: (BaseIO m, MonadPlus m) => m ()
help         = putStrLn usage
recompile    = Dyre.recompile >>= mapM_ putStrLn
printVersion = do
    (a, b, c) <- io ZMQ.version
    putStrLn $ "hbro: v" ++ pack (showVersion version)
    putStrLn $ "0MQ library: v" ++ intercalate "." (map tshow [a, b, c])

setLogLevel :: (BaseIO m, MonadPlus m) => Priority -> ArgDescr (a -> m a)
setLogLevel priority = NoArg $ \options -> do
    Logger.initialize priority
    return options

setCustomLogLevel :: (BaseIO m) => String -> (a -> m a)
setCustomLogLevel level options = do
    maybe (errorM "hbro.options" message) Logger.initialize $ readMaybe level
    return options
  where message = "Invalid log level '" ++ pack level ++ "'."

-- | Parse URI passed in commandline, check whether it is a file path or an internet URI
-- and return the corresponding normalized URI (that is: prefixed with "file://" or "http://")
setStartURI :: (BaseIO m, MonadPlus m) => String -> (CliOptions -> m CliOptions)
setStartURI (pack -> uri) options = do
    fileURI    <- io . isFile $ fpFromText uri
    workingDir <- io getWorkingDirectory

    let fileURI' = ("file://" ++ fpToText workingDir ++ "/" ++ uri) <| fileURI |> uri
    parsedURI <- maybe abort return =<< logErrors (parseURIReference fileURI')

    return $ set startURIL (Just parsedURI) options

-- | Usage text (cf @hbro -h@)
usage :: Text
usage = pack $ usageInfo "Usage: hbro [OPTIONS]" (description :: [OptDescr (CliOptions -> IO CliOptions)])

-- | Get and parse commandline options
parseOptions :: (BaseIO m, MonadPlus m) => m CliOptions
parseOptions = do
    Logger.initialize INFO

    (opts, input', unknown', errors') <- io $ getOpt' Permute description . map unpack <$> getArgs
    let (input, unknown, errors) = (tailSafe $ (pack <$> input'), pack <$> unknown', pack <$> errors')

    unless (null errors)  $ errorM "hbro.options" (unlines errors) >> abort
    unless (null unknown) . infoM "hbro.options" $ "Unrecognized options: " ++ unwords unknown
    unless (null input) . infoM "hbro.options" $ "Ignored input: " ++ unlines input

    options <- foldl' (>>=) (return def) opts
    debugM "hbro.options" $ "Used options: " ++ describe options
    return options

-- | Return socket URI used by this instance
getSocketURI :: (BaseIO m) => CliOptions -> m Text
getSocketURI options = maybe getDefaultSocketURI (return . normalize) $ options^.socketPathL
  where
    normalize = ("ipc://" ++) . fpToText
    getDefaultSocketURI = do
      dir <- fpToText <$> (io $ getAppCacheDirectory "hbro")
      pid <- io getProcessID
      return $ "ipc://" ++ dir ++ "/hbro." ++ tshow pid
