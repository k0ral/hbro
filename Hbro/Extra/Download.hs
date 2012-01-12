module Hbro.Extra.Download where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label

import Network.URI

import System.FilePath
-- }}}

labelNotify :: Environment -> IO ()
labelNotify env = do
  feedbackLabel <- builderGetObject ((mBuilder . mGUI) env)  castToLabel "feedback"
  labelSetMarkupTemporary feedbackLabel "<span foreground=\"green\">Download started</span>" 5000
  
aria, wget, axel :: URI -> FilePath -> String -> IO ()
aria uri directory filename = spawn "aria2c" [show uri, "-d", directory, "-o", filename]
wget uri directory filename = spawn "wget"   [show uri, "-O", directory </> filename]
axel uri directory filename = spawn "axel"   [show uri, "-o", directory </> filename]
