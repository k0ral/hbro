module Main where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import Paths_hbro
-- }}}

main :: IO ()
main = do
    uiFile <- getDataFileName "examples/ui.xml"

--     putStrLn "[WARNING] You are running the default configuration which provides hardly no feature."
--     putStrLn $ "[WARNING] You should copy the example configuration files hbro.hs and ui.xml in " ++ configHome ++ "/hbro and start hacking them."

    launchHbro defaultConfig {
        mUIFile = uiFile
    }
