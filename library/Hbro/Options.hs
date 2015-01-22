{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Commandline options tools.
module Hbro.Options (
      Command(..)
    , CliOptions()
    , startURIL
    , socketPathL
    , uiFileL
    , dyreModeL
    , dyreDebugL
    , logLevelL
    , parseOptions
    ) where

-- {{{ Imports
import qualified Hbro.Dyre                   as Dyre
import           Hbro.Error
import           Hbro.Logger                 as Logger
import           Hbro.Prelude                hiding ((<>))

import           Control.Lens.Getter
import           Control.Lens.TH

import           Network.URI

import           Options.Applicative.Builder
import           Options.Applicative.Extra
import           Options.Applicative.Types
-- }}}

-- * Types
-- | Available commands
data Command = Rebuild | Version

-- | Available options
declareLenses [d|
  data CliOptions = CliOptions
    { startURIL   :: Maybe URI
    , socketPathL :: Maybe FilePath
    , uiFileL     :: Maybe FilePath
    , dyreModeL   :: Dyre.Mode
    , dyreDebugL  :: Bool
    , logLevelL   :: Priority
    } deriving(Eq)
  |]

instance Describable CliOptions where
    describe opts = unwords $ catMaybes
        [ ("URI=" ++) . tshow <$> (opts^.startURIL)
        , ("SOCKET=" ++) . fpToText  <$> (opts^.socketPathL)
        , ("UI=" ++) . fpToText <$> (opts^.uiFileL)
        , Just . ("DYRE_MODE=" ++) . tshow $ opts^.dyreModeL
        , Just "DYRE_DEBUG" <| opts^.dyreDebugL |> Nothing
        , Just . ("LOG-LEVEL=" ++) . tshow $ opts^.logLevelL
        ]

instance Default CliOptions where
    def = CliOptions
            {- startURI   -} Nothing
            {- socketPath -} Nothing
            {- uiFile     -} Nothing
            {- dyreMode   -} def
            {- dyreDebug  -} False
            {- logLevel   -} INFO

-- * High level
parseOptions :: (MonadIO m) => m (Either Command CliOptions)
parseOptions = io $ customExecParser (prefs noBacktrack) (info parser $ progDesc "Minimal KISS-compliant browser")

-- * Low level
-- ** URI options
startURI :: Parser URI
startURI = option (eitherReader $ note "Invalid URI" . parseURIReference) $ long "uri" <> short 'u' <> metavar "START-URI" <> help "URI to load at start-up."

socketURI, uiURI :: Parser FilePath
socketURI = option (eitherReader $ Right . fromString) $ long "socket" <> short 'S' <> metavar "SOCKET-URI" <> help "URI to open IPC's listening socket."
uiURI     = option (eitherReader $ Right . fromString) $ long "ui" <> short 'U' <> metavar "UI-FILE" <> help "Path to UI descriptor (XML file)"

-- ** Dyre options
vanillaFlag, forceReconfFlag, denyReconfFlag :: Parser Dyre.Mode
vanillaFlag      = flag def Dyre.Vanilla $ long "vanilla" <> short '1' <> help "Do not read custom configuration file"
forceReconfFlag  = flag def Dyre.ForceReconfiguration $ long "force-reconf" <> help "Recompile configuration file before starting the program"
denyReconfFlag   = flag def Dyre.IgnoreReconfiguration $ long "deny-reconf" <> help "Do not recompile configuration file even if it has changed"

dyreDebug :: Parser Bool
dyreDebug = switch $ long "dyre-debug" <> help "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."

dyreMasterBinary :: Parser String
dyreMasterBinary = strOption $ long "dyre-master-binary" <> metavar "PATH" <> hidden <> internal <> help "Internal flag used for dynamic reconfiguration."

-- ** Log level options
verboseFlag, quietFlag, logLevel :: Parser Priority
verboseFlag = flag INFO DEBUG $ long "verbose" <> short 'v' <> help "Set log level to DEBUG."
quietFlag   = flag INFO ERROR $ long "quiet" <> short 'q' <> help "Set log level to ERROR."
logLevel    = option auto $ long "log-level" <> short 'l' <> metavar "LOG-LEVEL" <> value INFO <> completeWith ["DEBUG", "INFO", "NOTICE", "WARNING", "ERROR", "CRITICAL", "ALERT", "EMERGENCY"] <> help "Set log level. Available values: DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY."

-- ** Commands
rebuildOptions, versionOptions :: Parser Command
rebuildOptions = pure Rebuild
versionOptions = pure Version

rebuildCommand, versionCommand :: Mod CommandFields Command
rebuildCommand = command "rebuild" $ info rebuildOptions $ progDesc "Rebuild configuration file."
versionCommand = command "version" $ info versionOptions $ progDesc "Print version."

commands :: Parser Command
commands = subparser $ rebuildCommand <> versionCommand

-- ** Aggregated parsers
cliOptions :: Parser CliOptions
cliOptions = CliOptions
    <$> optional startURI
    <*> optional socketURI
    <*> optional uiURI
    <*> (vanillaFlag <|> forceReconfFlag <|> denyReconfFlag)
    <*> dyreDebug
    <*> (verboseFlag <|> quietFlag <|> logLevel)

parser :: Parser (Either Command CliOptions)
parser = helper <*> optional dyreMasterBinary *> ((Left <$> commands) <|> (Right <$> cliOptions))
