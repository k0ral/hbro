module Hbro.Extra.Clipboard where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
--import Hbro.Util

import Graphics.UI.Gtk.General.Clipboard
-- }}}


-- | Copy current URI in clipboard.
toClipboard :: String -> IO ()
toClipboard text = do
    primaryClip <- clipboardGet selectionPrimary
    clipboardSetText primaryClip text
    
withClipboard :: (Maybe String -> IO ()) -> IO ()
withClipboard callback = do
    primaryClip <- clipboardGet selectionPrimary
    clipboardRequestText primaryClip callback
