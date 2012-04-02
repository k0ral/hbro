module Main where

-- {{{ Imports
import Hbro.Config
import Hbro.Hbro
import Hbro.Types

import Paths_hbro
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = do
    uiFile <- getDataFileName "examples/ui.xml"
    
    launchHbro $ defaultConfig {
        mUIFile = const uiFile
    }
