{-# LANGUAGE DoRec #-}
module Hbro.Gui where

-- {{{ Imports
import Control.Monad.Trans(liftIO)

--import Graphics.UI.Gtk.Abstract.Misc
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import System.Glib.Attributes
import System.Glib.Signals
-- }}}

data GUI = GUI {
    mWindow             :: Window,          -- ^ Main window
    mInspectorWindow    :: Window,          -- ^ WebInspector window
    mScrollWindow       :: ScrolledWindow,  -- ^ ScrolledWindow containing the webview
    mWebView            :: WebView,         -- ^ Browser's webview
    mPromptLabel        :: Label,           -- ^ Description of current prompt
    mPrompt             :: Entry,           -- ^ Prompt entry
    mBuilder            :: Builder          -- ^ Builder object created from XML file
}

-- {{{ Load GUI from XML file
loadGUI :: String -> IO GUI
loadGUI xmlPath = do
    builder <- builderNew
    builderAddFromFile builder xmlPath

    -- Load main window
    window       <- builderGetObject builder castToWindow            "mainWindow"
    scrollWindow <- builderGetObject builder castToScrolledWindow    "webViewParent"
    promptLabel  <- builderGetObject builder castToLabel             "promptDescription"
    promptEntry  <- builderGetObject builder castToEntry             "promptEntry"

    inspectorWindow <- windowNew

    --windowSetDefaultSize window 1024 768
    --windowSetPosition   window WinPosCenter
    --windowSetIconFromFile window "/path/to/icon"
    set window [ windowTitle := "hbro" ]

    webView <- webViewNew
    containerAdd scrollWindow webView 

    set webView [ widgetCanDefault := True ]
    windowSetDefault window (Just webView)
    set scrollWindow [
        scrolledWindowHscrollbarPolicy := PolicyNever,
        scrolledWindowVscrollbarPolicy := PolicyNever ]

    _ <- on webView closeWebView $ do
        mainQuit
        return True

    return $ GUI window inspectorWindow scrollWindow webView promptLabel promptEntry builder
-- }}}

-- {{{ Prompt
-- | Show or hide the prompt bar (label + entry).
showPrompt :: Bool -> GUI -> IO ()
showPrompt toShow gui = case toShow of
    False -> do widgetHide (mPromptLabel gui)
                widgetHide (mPrompt gui)
    _     -> do widgetShow (mPromptLabel gui)
                widgetShow (mPrompt gui)

-- | Show the prompt bar label and default text.
-- As the user validates its entry, the given callback is executed.
prompt :: String -> String -> Bool -> GUI -> (GUI -> IO ()) -> IO ()
prompt label defaultText incremental gui callback = do
    -- Show prompt
    showPrompt True gui

    -- Fill prompt
    labelSetText (mPromptLabel gui) label
    entrySetText (mPrompt gui) defaultText

    widgetGrabFocus (mPrompt gui)

    -- Register callback
    case incremental of
        True -> do 
            id1 <- on (mPrompt gui) editableChanged $  
                liftIO $ callback gui
            rec id2 <- on (mPrompt gui) keyPressEvent $ do
                key <- eventKeyName
                
                case key of
                    "Return" -> do
                        liftIO $ showPrompt False gui
                        liftIO $ signalDisconnect id1
                        liftIO $ signalDisconnect id2
                        liftIO $ widgetGrabFocus (mWebView gui)
                    "Escape" -> do
                        liftIO $ showPrompt False gui
                        liftIO $ signalDisconnect id1
                        liftIO $ signalDisconnect id2
                        liftIO $ widgetGrabFocus (mWebView gui)
                    _ -> return ()
                return False
            return ()

        _ -> do
            rec id <- on (mPrompt gui) keyPressEvent $ do
                key <- eventKeyName

                case key of
                    "Return" -> do
                        liftIO $ showPrompt False gui
                        liftIO $ callback gui
                        liftIO $ signalDisconnect id
                        liftIO $ widgetGrabFocus (mWebView gui)
                    "Escape" -> do
                        liftIO $ showPrompt False gui
                        liftIO $ signalDisconnect id
                        liftIO $ widgetGrabFocus (mWebView gui)
                    _        -> return ()
                return False

            return ()
-- }}}
