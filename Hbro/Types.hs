{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE ExistentialQuantification #-}
module Hbro.Types where

-- {{{ Imports
import Data.Map

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import System.Console.CmdArgs
import System.Glib.Attributes
import System.ZMQ 
-- }}}


data Environment = Environment {
    mOptions :: CliOptions,          -- ^ Commandline options
    mConfig  :: Config,              -- ^ Configuration parameters (constants) provided by user
    mGUI     :: GUI                  -- ^ Graphical widgets
}

data CliOptions = CliOptions {
    mURI :: Maybe String                    -- ^ URI to load at start-up
} deriving (Data, Typeable, Show, Eq)


data Config = {-forall a.-} Config {
    mHomePage    :: String,                    -- ^ Startup page 
    mSocketDir   :: String,                    -- ^ Directory where 0MQ will be created ("/tmp" for example)
    mUIFile      :: String,                    -- ^ Path to XML file describing UI (used by GtkBuilder)
    mKeys        :: Environment -> KeysList,   -- ^ List of keybindings
    mWebSettings :: [AttrOp WebSettings],      -- ^ WebSettings' attributes to use with webkit (see Webkit.WebSettings documentation)
    mSetup       :: Environment -> IO (),      -- ^ Custom startup instructions
    mCommands    :: CommandsList,              -- ^ Custom commands to use with IPC sockets
    mError       :: Maybe String               -- ^ Error
    --mCustom      :: a
}

data GUI = GUI {
    mWindow             :: Window,          -- ^ Main window
    mInspectorWindow    :: Window,          -- ^ WebInspector window
    mScrollWindow       :: ScrolledWindow,  -- ^ ScrolledWindow containing the webview
    mWebView            :: WebView,         -- ^ Browser's webview
    mPromptBar          :: PromptBar, 
    mStatusBox          :: HBox,            -- ^ Status bar's horizontal box
    mBuilder            :: Builder          -- ^ Builder object created from XML file
}

data PromptBar = PromptBar {
    mBox         :: HBox,
    mDescription :: Label, -- ^ Description of current prompt
    mEntry       :: Entry -- ^ Prompt entry
}


-- | List of bound keys
-- All callbacks are fed with the Browser instance
-- Note 1 : for modifiers, lists are used for convenience purposes,
--          but are transformed into sets in hbro's internal machinery,
--          so that order and repetition don't matter
-- Note 2 : for printable characters accessed via the shift modifier,
--          you do have to include Shift in modifiers list
type KeysList = [(([Modifier], String), IO ())]

type CommandsList = [(String, ([String] -> Socket Rep -> Environment -> IO ()))]
type CommandsMap  = Map String ([String] -> Socket Rep -> Environment -> IO ())
