module Hbro.Extra.Clipboard where

-- {{{ Imports
--import Hbro.Core
--import Hbro.Types
--import Hbro.Util

import Graphics.UI.Gtk.General.Clipboard
-- }}}


-- | Copy current URI in clipboard.
toClipboard :: String -> IO ()
toClipboard text = clipboardGet selectionPrimary >>= (`clipboardSetText` text)
    
withClipboard :: (Maybe String -> IO ()) -> IO ()
withClipboard callback = clipboardGet selectionPrimary >>= (`clipboardRequestText` callback)
