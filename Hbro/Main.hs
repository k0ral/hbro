module Main where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Paths_hbro

import System.Directory
import System.Environment.XDG.BaseDir
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = do
--     putStrLn "[WARNING] You are running the default configuration which provides hardly no feature."
--     putStrLn "[WARNING] You should copy the example configuration files hbro.hs and ui.xml in ~/.config/hbro/hbro and start hacking them."

    tmpDir    <- getTemporaryDirectory
    configDir <- getUserConfigDir "hbro"
    dataDir   <- getUserDataDir "hbro"
    uiFile    <- getDataFileName "examples/ui.xml"
    
    launchHbro $ (defaultConfig $ CommonDirectories tmpDir configDir dataDir) {
        mUIFile = uiFile
    }
