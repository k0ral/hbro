{-# LANGUAGE DoRec #-}
module Hbro.Gui where

-- {{{ Imports
import Hbro.Types

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

-- | Load GUI from XML file
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

-- {{{ Prompt
-- | Show or hide the prompt bar (label + entry).
showPrompt :: Bool -> Browser -> IO ()
showPrompt toShow browser = case toShow of
    False -> do widgetHide (mPromptLabel $ mGUI browser)
                widgetHide (mPromptEntry $ mGUI browser)
    _     -> do widgetShow (mPromptLabel $ mGUI browser)
                widgetShow (mPromptEntry $ mGUI browser)

-- | Show the prompt bar label and default text.
-- As the user validates its entry, the given callback is executed.
prompt :: String -> String -> Bool -> Browser -> (Browser -> IO ()) -> IO ()
prompt label defaultText incremental browser callback = let
        promptLabel = (mPromptLabel $ mGUI browser)
        promptEntry = (mPromptEntry $ mGUI browser)
        webView     = (mWebView     $ mGUI browser)
    in do
        -- Show prompt
        showPrompt True browser

        -- Fill prompt
        labelSetText promptLabel label
        entrySetText promptEntry defaultText

        widgetGrabFocus promptEntry

        -- Register callback
        case incremental of
            True -> do 
                id1 <- on promptEntry editableChanged $  
                    liftIO $ callback browser
                rec id2 <- on promptEntry keyPressEvent $ do
                    key <- eventKeyName
                    
                    case key of
                        "Return" -> do
                            liftIO $ showPrompt False browser
                            liftIO $ signalDisconnect id1
                            liftIO $ signalDisconnect id2
                            liftIO $ widgetGrabFocus webView
                        "Escape" -> do
                            liftIO $ showPrompt False browser
                            liftIO $ signalDisconnect id1
                            liftIO $ signalDisconnect id2
                            liftIO $ widgetGrabFocus webView
                        _ -> return ()
                    return False
                return ()

            _ -> do
                rec id <- on promptEntry keyPressEvent $ do
                    key <- eventKeyName

                    case key of
                        "Return" -> do
                            liftIO $ showPrompt False browser
                            liftIO $ callback browser
                            liftIO $ signalDisconnect id
                            liftIO $ widgetGrabFocus webView
                        "Escape" -> do
                            liftIO $ showPrompt False browser
                            liftIO $ signalDisconnect id
                            liftIO $ widgetGrabFocus webView
                        _        -> return ()
                    return False

                return ()
-- }}}

fullscreen, unfullscreen :: Browser -> IO()
fullscreen   browser = windowFullscreen   (mWindow $ mGUI browser)
unfullscreen browser = windowUnfullscreen (mWindow $ mGUI browser)
