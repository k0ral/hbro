module Main where

-- {{{ Imports
import Hbro.Boot
import Hbro.Config
import Hbro.Types

import Paths_hbro
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = do
    uiFile <- getDataFileName "examples/ui.xml"
    
    hbro $ defaultConfig {
        mUIFile = const uiFile
    }
