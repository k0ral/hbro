{-# LANGUAGE DeriveDataTypeable #-} 
module Hbro.Types where

-- {{{ Imports
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import Prelude

import System.Console.CmdArgs
-- }}}

data Browser = Browser {
    mOptions        :: CliOptions,          -- ^ Commandline options
    mConfiguration  :: Configuration,       -- ^ Custom configuration provided by user
    mGUI            :: GUI                  -- ^ Graphical widgets
}

data CliOptions = CliOptions {
    mURI :: Maybe String                    -- ^ URI to load at start-up
} deriving (Data, Typeable, Show, Eq)


data Configuration = Configuration {
    mHomePage       :: String,              -- ^ Startup page 
    mSocketDir      :: String,              -- ^ Path to socket directory ("/tmp" for example)
    mUIFile         :: String,              -- ^ Path to XML file describing UI (used by GtkBuilder)
    mKeyBindings    :: KeyBindingsList,     -- ^ List of keybindings
    mWebSettings    :: IO WebSettings,      -- ^ Web settings
    mAtStartUp      :: Browser -> IO (),    -- ^ Custom startup instructions
    mError          :: Maybe String         -- ^ Error
}

data GUI = GUI {
    mWindow             :: Window,          -- ^ Main window
    mInspectorWindow    :: Window,          -- ^ WebInspector window
    mScrollWindow       :: ScrolledWindow,  -- ^ ScrolledWindow containing the webview
    mWebView            :: WebView,         -- ^ Browser's webview
    mPromptLabel        :: Label,           -- ^ Description of current prompt
    mPromptEntry        :: Entry,           -- ^ Prompt entry
    mStatusBox          :: HBox,            -- ^ Status bar's horizontal box
    mBuilder            :: Builder          -- ^ Builder object created from XML file
}

type KeyBindingsList = [(([Modifier], String), (Browser -> IO ()))]
