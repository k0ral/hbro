{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Dynamic reconfiguration. Designed to be imported as @qualified@.
module Hbro.Dyre
    ( Mode(..)
    , getHbroExecutable
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
describePaths :: MonadIO m => m Text
describePaths = io $ do
    (a, b, c, d, e) <- getPaths baseParameters
    return . unlines $ map pack
        [ "Current binary:  " ++ a
        , "Custom binary:   " ++ b
        , "Config file:     " ++ c
        , "Cache directory: " ++ d
        , "Lib directory:   " ++ e
        ]

getHbroExecutable :: (MonadIO m) => m FilePath
getHbroExecutable = io $ do
  (a, _, _, _, _) <- getPaths baseParameters
  return . fpFromText $ pack a

-- Dynamic reconfiguration settings
parameters :: (Functor m, MonadIO m, MonadLogger m, StM m () ~ ()) => (RunInBase m IO) -> Mode -> (a -> m b) -> Params (Either Text a)
parameters runInIO mode main = baseParameters
    { configCheck = mode /= Vanilla
    , realMain    = runInIO . main'
    }
    where main' (Left e) = error e
          main' (Right x) = do
            debug . ("Dynamic reconfiguration paths:\n" ++) =<< describePaths
            void $ main x

baseParameters :: Params (Either Text a)
baseParameters = defaultParams
    { projectName             = "hbro"
    , showError               = const (Left . pack)
    , ghcOpts                 = ["-threaded"]
    , statusOut               = hPutStrLn stderr
    , includeCurrentDirectory = False
    }

wrap :: (ControlIO m, MonadLogger m, StM m () ~ ()) => Mode -> (a -> m b) -> a -> m ()
wrap mode result args = liftBaseWith $ \runInIO -> wrapMain (parameters runInIO mode result) (Right args)


-- | Launch a recompilation of the configuration file
recompile :: (MonadIO m) => m (Maybe Text)
recompile = io $ do
    customCompile baseParameters
    map pack <$> getErrorString baseParameters
