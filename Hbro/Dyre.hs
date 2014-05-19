-- | Dynamic reconfiguration. Designed to be imported as @qualified@.
module Hbro.Dyre (
    Mode(..),
    wrap,
    recompile
) where

-- {{{ Imports
import Hbro.Util

import Config.Dyre
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Monad hiding(mapM_)

import Prelude hiding(mapM_)

import System.IO
-- }}}

-- | How dynamic reconfiguration process should behave.
-- Default is 'Normal', that is: use custom configuration file and recompile if change detected.
data Mode = Normal | Vanilla | ForceReconfiguration | IgnoreReconfiguration
    deriving(Eq, Show)

instance Default Mode where def = Normal

-- | Describe various paths used for dynamic reconfiguration
describePaths :: MonadBase IO m => m String
describePaths = io $ do
    (a, b, c, d, e) <- getPaths baseParameters
    return $ unlines
        [ "Current binary:  " ++ a
        , "Custom binary:   " ++ b
        , "Config file:     " ++ c
        , "Cache directory: " ++ d
        , "Lib directory:   " ++ e
        ]

-- Dynamic reconfiguration settings
parameters :: Mode -> (a -> IO b) -> Params (Either String a)
parameters mode main = baseParameters
    { configCheck = mode /= Vanilla
    , realMain    = main'
    }
  where
    main' (Left e)  = errorM "hbro.dyre" e
    main' (Right x) = do
      debugM "hbro.dyre" . ("Dynamic reconfiguration paths:\n" ++) =<< describePaths
      void $ main x

baseParameters :: Params (Either String a)
baseParameters = defaultParams
    { projectName             = "hbro"
    , showError               = const Left
    , ghcOpts                 = ["-threaded"]
    , statusOut               = hPutStrLn stderr
    , includeCurrentDirectory = False
    }

wrap :: (MonadBase IO m) => Mode -> (a -> IO b) -> a -> m ()
wrap mode main args = io . wrapMain (parameters mode main) $ Right args


-- | Launch a recompilation of the configuration file
recompile :: (MonadBase IO m) => m (Maybe String)
recompile = io $ do
    customCompile  baseParameters
    getErrorString baseParameters
