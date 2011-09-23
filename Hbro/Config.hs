module Hbro.Config (
    hbro,
    defaultConfiguration
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Types

import qualified Config.Dyre as D

import Graphics.UI.Gtk.WebKit.WebSettings

import System.IO
-- }}}

-- | Browser's main function.
-- To be called in function "main" with a proper configuration.
hbro :: Configuration -> IO ()
hbro = D.wrapMain D.defaultParams {
    D.projectName  = "hbro",
    D.showError    = showError,
    D.realMain     = realMain,
    D.ghcOpts      = ["-threaded"],
    D.statusOut    = hPutStrLn stderr
}


showError :: Configuration -> String -> Configuration
showError configuration message = configuration { mError = Just message }


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
