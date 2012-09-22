module Main where

-- {{{ Imports
import Hbro

import Data.Default

import Paths_hbro
-- }}}

-- | Default main function provided as example.
main :: IO ()
main = hbro (def { __UIFile = getDataFileName "examples/ui.xml" }) def
