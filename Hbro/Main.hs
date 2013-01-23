module Main where

-- {{{ Imports
import Hbro

import Control.Lens

import Paths_hbro
-- }}}

-- | Default main function provided as example
main :: IO ()
main = do
    theUIFile <- getDataFileName "examples/ui.xml"
    hbro (return ()) (set uIFile theUIFile)
