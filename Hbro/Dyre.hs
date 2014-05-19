-- | Dynamic reconfiguration. Designed to be imported as @qualified@.
module Hbro.Dyre (
    wrap,
    recompile
) where

-- {{{ Imports
import Hbro.Options hiding(recompile)
import Hbro.Util

import Config.Dyre
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader

import System.IO
-- }}}


nullMain :: a -> IO ()
nullMain = const $ return ()

-- Print various paths used for dynamic reconfiguration
printPaths :: MonadBase IO m => m ()
printPaths = io $ do
    (a, b, c, d, e) <- getPaths $ parameters nullMain False
    putStrLn $ unlines [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e]

-- Dynamic reconfiguration settings
parameters :: (a -> IO ()) -> Bool -> Params (Either String a)
parameters main verbose' = defaultParams {
    projectName             = "hbro",
    showError               = const Left,
    realMain                = main',
    ghcOpts                 = ["-threaded"],
    statusOut               = hPutStrLn stderr,
    includeCurrentDirectory = False}
  where
    main' (Left e)  = putStrLn e
    main' (Right x) = do
      when verbose' printPaths
      main x

wrap :: (a -> IO ()) -> CliOptions -> a -> IO ()
wrap main opts args = do
    wrapMain ((parameters main (opts^.verbose)) { configCheck = not $ opts^.vanilla }) $ Right args


-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile  $ parameters nullMain False
    getErrorString $ parameters nullMain False
