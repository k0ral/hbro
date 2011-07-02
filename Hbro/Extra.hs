module Hbro.Extra where

-- {{{ Imports
import Hbro.Core
import Hbro.Gui
import Hbro.Types
import Hbro.Util 

import Control.Monad.Trans(liftIO)

import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.Clipboard
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView

import System.Glib.Signals

import System.Process
-- }}}

-- {{{ Statusbar elements
-- | Display scroll position in status bar.
-- Needs a Label intitled "scroll" from the builder.
statusBarScrollPosition :: Browser -> IO ()
statusBarScrollPosition browser = 
  let
    builder         = mBuilder      (mGUI browser)
    scrollWindow    = mScrollWindow (mGUI browser)
  in do
    scrollLabel     <- builderGetObject builder castToLabel "scroll"

    adjustment <- scrolledWindowGetVAdjustment scrollWindow
    _ <- onValueChanged adjustment $ do
        current <- adjustmentGetValue adjustment
        lower   <- adjustmentGetLower adjustment
        upper   <- adjustmentGetUpper adjustment
        page    <- adjustmentGetPageSize adjustment
        
        case upper-lower-page of
            0 -> labelSetMarkup scrollLabel "ALL"
            x -> labelSetMarkup scrollLabel $ show (round $ current/x*100) ++ "%"
    return ()


-- | Display pressed keys in status bar.
-- Needs a Label intitled "keys" from the builder.
statusBarPressedKeys :: Browser -> IO ()
statusBarPressedKeys browser = 
  let
    builder         = mBuilder      (mGUI browser)
    webView         = mWebView      (mGUI browser)
  in do
    keysLabel       <- builderGetObject builder castToLabel "keys"
    
    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value
        case keyString of 
            Just string -> liftIO $ labelSetMarkup keysLabel $ "<span foreground=\"green\">" ++ show modifiers ++ escapeMarkup string ++ "</span>"
            _           -> return ()

        return False
    return ()


-- | Display load progress in status bar.
-- Needs a Label intitled "progress" from the builder.
statusBarLoadProgress :: Browser -> IO ()
statusBarLoadProgress browser = 
  let
    builder         = mBuilder      (mGUI browser)
    webView         = mWebView      (mGUI browser)
  in do
    progressLabel   <- builderGetObject builder castToLabel "progress"

    _ <- on webView loadStarted $ \_ -> do
        labelSetMarkup progressLabel "<span foreground=\"red\">0%</span>"
    
    _ <- on webView progressChanged $ \progress' ->
        labelSetMarkup progressLabel $ "<span foreground=\"yellow\">" ++ show progress' ++ "%</span>"

    _ <- on webView loadFinished $ \_ -> do
        labelSetMarkup progressLabel "<span foreground=\"green\">100%</span>"

    _ <- on webView loadError $ \_ _ _ -> do
        labelSetMarkup progressLabel "<span foreground=\"red\">ERROR</span>"
        return False
    return ()


-- | Display current URI, or the destination of a hovered link, in the status bar.
-- Needs a Label intitled "uri" from the builder.
statusBarURI :: Browser -> IO ()
statusBarURI browser = 
  let
    builder         = mBuilder      (mGUI browser)
    webView         = mWebView      (mGUI browser)
  in do
    uriLabel        <- builderGetObject builder castToLabel "uri"
    
    _ <- on webView loadCommitted $ \_ -> do
        getUri <- (webViewGetUri webView)
        case getUri of 
            Just uri -> labelSetMarkup uriLabel $ "<span weight=\"bold\" foreground=\"white\">" ++ escapeMarkup uri ++ "</span>"
            _        -> labelSetMarkup uriLabel "<span weight=\"bold\" foreground=\"red\">ERROR</span>"

    _ <- on webView hoveringOverLink $ \title hoveredUri -> do
        getUri <- (webViewGetUri webView)
        case (hoveredUri, getUri) of
            (Just u, _) -> labelSetMarkup uriLabel $ "<span foreground=\"#5555ff\">" ++ escapeMarkup u ++ "</span>"
            (_, Just u) -> labelSetMarkup uriLabel $ "<span foreground=\"white\" weight=\"bold\">" ++ escapeMarkup u ++ "</span>"
            _           -> putStrLn "FIXME"
    return ()
-- }}}

-- {{{ Features prompts
-- | Prompt for key words to search in current webpage.
promptFind :: Bool -> Bool -> Bool -> Browser -> IO ()
promptFind caseSensitive forward wrap browser =
    prompt "Search" "" True browser (\browser' -> do
        keyWord <- entryGetText (mPromptEntry $ mGUI browser')
        found   <- webViewSearchText (mWebView $ mGUI browser) keyWord caseSensitive forward wrap
        return ())

-- | Switch to next found key word.
findNext :: Bool -> Bool -> Bool -> Browser -> IO ()
findNext caseSensitive forward wrap browser = do
    keyWord <- entryGetText (mPromptEntry $ mGUI browser)
    found   <- webViewSearchText (mWebView $ mGUI browser) keyWord caseSensitive forward wrap 
    return ()

-- | Prompt for URI to open in current window.
promptURL :: Bool -> Browser -> IO()        
promptURL False browser = 
    prompt "Open URL" "" False browser (\b -> do 
        uri <- entryGetText (mPromptEntry $ mGUI b)
        loadURL uri b)
promptURL _ browser = do
    uri <- webViewGetUri (mWebView $ mGUI browser)
    case uri of
        Just url -> prompt "Open URL" url False browser (\b -> do
                        u <- entryGetText (mPromptEntry $ mGUI b)
                        loadURL u b)
        _ -> return ()
-- }}}

-- {{{ Copy/paste
copyUri, copyTitle, loadURIFromClipboard :: Browser -> IO ()

-- | Copy current URI in clipboard.
copyUri browser = do
    getUri <- webViewGetUri (mWebView $ mGUI browser)
    primaryClip <- widgetGetClipboard (mWindow $ mGUI browser) selectionPrimary

    case getUri of
        Just u -> clipboardSetText primaryClip u
        _      -> return ()

-- | Copy current page title in clipboard.
copyTitle browser = do
    getTitle    <- webViewGetTitle (mWebView $ mGUI browser)
    primaryClip <- widgetGetClipboard (mWindow $ mGUI browser) selectionPrimary

    case getTitle of
        Just t -> clipboardSetText primaryClip t
        _      -> return ()

-- | Load URI from clipboard. Does not work for now...
loadURIFromClipboard browser = do
    primaryClip <- widgetGetClipboard (mWindow $ mGUI browser) selectionPrimary

    _ <- clipboardRequestText primaryClip $ \x -> case x of
        Just uri -> putStrLn ("Loading URI from clipboard: " ++ uri) >> loadURL uri browser
        _        -> putStrLn "Loading URI from clipboard: empty clipboard."
    return ()
-- }}}

-- {{{ Others
toggleSourceMode :: Browser -> IO ()
toggleSourceMode browser = do
    currentMode <- webViewGetViewSourceMode (mWebView $ mGUI browser)
    webViewSetViewSourceMode (mWebView $ mGUI browser) (not currentMode)
    reload True browser
-- }}}
