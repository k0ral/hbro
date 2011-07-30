module Hbro.Config where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import qualified Config.Dyre as Dyre

import Graphics.UI.Gtk.WebKit.WebSettings
-- }}}


showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }

hbro :: Configuration -> IO ()
hbro = Dyre.wrapMain Dyre.defaultParams {
    Dyre.projectName  = "hbro",
    Dyre.showError    = showError,
    Dyre.realMain     = realMain,
    Dyre.ghcOpts      = ["-threaded"]
}

-- | Default configuration.
-- Does quite nothing.
defaultConfiguration :: Configuration
defaultConfiguration = Configuration {
    mHomePage    = "https://www.google.com",
    mSocketDir   = "/tmp/",
    mUIFile      = "~/.config/hbro/ui.xml",
    mKeys        = [],
    mWebSettings = webSettingsNew,
    mSetup       = \_ -> return () :: IO (),
    mCommands    = [],
    mError       = Nothing
}
