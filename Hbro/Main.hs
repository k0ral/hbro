module Main where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Paths_hbro

import System.Directory
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = do
    home   <- getHomeDirectory
    tmp    <- getTemporaryDirectory
    uiFile <- getDataFileName "examples/ui.xml"

--     putStrLn "[WARNING] You are running the default configuration which provides hardly no feature."
--     putStrLn "[WARNING] You should copy the example configuration files hbro.hs and ui.xml in ~/.config/hbro/hbro and start hacking them."

    launchHbro $ (defaultConfig home tmp) {
        mUIFile = uiFile
    }
