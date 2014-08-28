-- | Dynamic reconfiguration. Designed to be imported as @qualified@.
module Hbro.Dyre
    ( Mode(..)
    , wrap
    , recompile
    ) where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Config.Dyre
import           Config.Dyre.Compile
import           Config.Dyre.Paths
-- }}}

-- | How dynamic reconfiguration process should behave.
data Mode = Normal | Vanilla | ForceReconfiguration | IgnoreReconfiguration
    deriving(Eq, Show)

-- | Default mode is 'Normal', that is: use custom configuration file and recompile if change detected.
instance Default Mode where def = Normal

-- | Describe various paths used for dynamic reconfiguration
describePaths :: BaseIO m => m Text
describePaths = io $ do
    (a, b, c, d, e) <- getPaths baseParameters
    return . unlines $ map pack
        [ "Current binary:  " ++ a
        , "Custom binary:   " ++ b
        , "Config file:     " ++ c
        , "Cache directory: " ++ d
        , "Lib directory:   " ++ e
        ]

-- Dynamic reconfiguration settings
parameters :: Mode -> (a -> IO b) -> Params (Either Text a)
parameters mode main = baseParameters
    { configCheck = mode /= Vanilla
    , realMain    = main'
    }
  where
    main' (Left e)  = errorM "hbro.dyre" e
    main' (Right x) = do
      debugM "hbro.dyre" . ("Dynamic reconfiguration paths:\n" ++) =<< describePaths
      void $ main x

baseParameters :: Params (Either Text a)
baseParameters = defaultParams
    { projectName             = "hbro"
    , showError               = const (Left . pack)
    , ghcOpts                 = ["-threaded"]
    , statusOut               = hPutStrLn stderr
    , includeCurrentDirectory = False
    }

wrap :: (BaseIO m) => Mode -> (a -> IO b) -> a -> m ()
wrap mode main args = io . wrapMain (parameters mode main) $ Right args


-- | Launch a recompilation of the configuration file
recompile :: (BaseIO m) => m (Maybe Text)
recompile = io $ do
    customCompile baseParameters
    map pack <$> getErrorString baseParameters
