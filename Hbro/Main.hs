module Main where

-- {{{ Imports
import Hbro.Hbro
import Hbro.Types

import Paths_hbro
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = do
--     putStrLn "[WARNING] You are running the default configuration which provides hardly no feature."
--     putStrLn "[WARNING] You should copy the example configuration files hbro.hs and ui.xml in ~/.config/hbro/hbro and start hacking them."

    uiFile <- getDataFileName "examples/ui.xml"
    
    launchHbro $ \d -> (defaultConfig d) {
        mUIFile = uiFile
    }
