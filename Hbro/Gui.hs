{-# LANGUAGE DoRec #-}
module Hbro.Gui where

-- {{{ Imports
import Control.Monad.Trans(liftIO)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
-- }}}

data GUI = GUI {
    mWindow             :: Window,  -- ^ Main window
    mInspectorWindow    :: Window,  -- ^ WebInspector window
    mWebView            :: WebView, -- ^ Browser's webview
    mPromptLabel        :: Label,   -- ^ Description of current prompt
    mPrompt             :: Entry,   -- ^ Prompt entry
    mWindowBox          :: VBox,    -- ^ Window's layout
    mStatusBox          :: HBox,    -- ^ Status bar's layout
    mProgressLabel      :: Label,
    mUrlLabel           :: Label
}

-- {{{ Load glade GUI
-- | Load GUI from a glade file.
loadGUI :: String -> IO GUI
loadGUI gladePath = do
--        -- Note: crashes with a runtime error on console if fails!
--        Just xml <- xmlNew gladePath

--        -- Load main window
--        -- castTo* don't exist :(
--        window       <- xmlGetWidget xml castToWindow    "window"
--        webView      <- xmlGetWidget xml castToWebView   "webView"
--        promptLabel  <- xmlGetWidget xml castToLabel     "promptLabel"
--        prompt       <- xmlGetWidget xml castToEntry     "prompt"

    window          <- windowNew
    inspectorWindow <- windowNew

    windowSetDefaultSize window 1024 768
    windowSetPosition   window WinPosCenter
    --windowSetIconFromFile window "/path/to/icon"
    set window [ windowTitle := "hbro" ]

    webView         <- webViewNew
    winBox          <- vBoxNew False 0
    promptBox       <- hBoxNew False 10
    statusBox       <- hBoxNew False 5
    scrollWin       <- scrolledWindowNew Nothing Nothing
    promptLabel     <- labelNew Nothing
    promptEntry     <- entryNew
    progressLabel   <- labelNew (Just "0%")
    urlLabel        <- labelNew (Just "")

    boxPackStart winBox     scrollWin       PackGrow 0
    boxPackStart winBox     promptBox       PackNatural 0
    boxPackStart winBox     statusBox       PackNatural 0
    boxPackStart promptBox  promptLabel     PackNatural 0
    boxPackStart promptBox  promptEntry     PackGrow 0
    boxPackStart statusBox  progressLabel   PackNatural 0
    boxPackStart statusBox  urlLabel        PackNatural 0

    window `containerAdd` winBox
    scrollWin `containerAdd` webView

    set webView [ widgetCanDefault := True ]
    windowSetDefault window (Just webView)

    _ <- on webView closeWebView $ do
        mainQuit
        return True

    return $ GUI window inspectorWindow webView promptLabel promptEntry winBox statusBox progressLabel urlLabel
-- }}}

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

