module Hbro.Dyre where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Config.Dyre
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Monad.IO.Class

import System.IO
-- }}}


-- | Print various paths used for dynamic reconfiguration
printPaths :: MonadIO m => m ()
printPaths = io $ do
    (a, b, c, d, e) <- getPaths (parameters $ const $ return ())
    putStrLn $ unlines [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e, []]

-- | Dynamic reconfiguration settings
parameters :: (Either String a -> IO ()) -> Params (Either String a)
parameters main = defaultParams {
    projectName             = "hbro",
    showError               = const Left,
    realMain                = main,
    ghcOpts                 = ["-threaded"],
    statusOut               = hPutStrLn stderr,
    includeCurrentDirectory = False}

wrap :: (Either String a -> IO ()) -> CliOptions -> (Either String a -> IO ())
wrap main opts = wrapMain $ (parameters main) { configCheck = not $ _vanilla opts }


-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile  (parameters $ const $ return ())
    getErrorString (parameters $ const $ return ())
