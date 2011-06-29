module Main where

-- {{{ Imports
import Hbro.Config
import Hbro.Types

import System.Environment

import Paths_hbro
-- }}}

main :: IO ()
main = do
    uiFile      <- getDataFileName "examples/ui.xml"
    configHome  <- getEnv "XDG_CONFIG_HOME"

    putStrLn "You are running the default configuration which provides hardly no feature."
    putStrLn $ "You should copy the example configuration files hbro.hs and ui.xml in " ++ configHome ++ "/hbro and start hacking them."

    hbro defaultConfiguration {
        mUIFile = uiFile
    }
