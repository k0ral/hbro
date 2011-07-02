module Main where

-- {{{ Imports
import Hbro.Config
import Hbro.Core
import Hbro.Extra
import Hbro.Gui
import Hbro.Types
import Hbro.Util

import Control.Concurrent

--import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
--import Graphics.UI.Gtk.Misc.Adjustment
--import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import System.Environment
import System.Glib.Attributes
import System.Glib.Signals
import System.Posix.Process
import System.Process 
-- }}}

main :: IO ()
main = do
    configHome <- getEnv "XDG_CONFIG_HOME"

    -- See Types::Configuration documentation for fields description
    -- Commented out fields indicated default values
    hbro defaultConfiguration {
        --mSocketDir    = "/tmp/",
        mUIFile         = configHome ++ "/hbro/ui.xml",
        --mHomePage     = "https://www.google.com",
        mKeys           = myKeys,
        mWebSettings    = myWebSettings,
        mSetup          = mySetup
    }


-- {{{ Keys
myKeys :: KeysList
myKeys = generalKeys ++ bookmarksKeys

generalKeys :: KeysList
generalKeys = [
    --  ((modifiers,        key),           callback)
    -- Browse
    (([],               "<"),           goBack),
    (([Shift],          ">"),           goForward),
    (([Control],        "s"),           stopLoading),
    (([],               "<F5>"),        reload True),
    (([Shift],          "<F5>"),        reload False),
    (([Control],        "r"),           reload True),
    (([Control, Shift], "R"),           reload False),
    (([Control],        "^"),           horizontalHome),
    (([Control],        "$"),           horizontalEnd),
    (([Control],        "<Home>"),      verticalHome),
    (([Control],        "<End>"),       verticalEnd),
    (([Alt],            "<Home>"),      goHome),

    -- Display
    (([Control, Shift], "+"),           zoomIn),
    (([Control],        "-"),           zoomOut),
    (([],               "<F11>"),       fullscreen),
    (([],               "<Escape>"),    unfullscreen),
    (([Control],        "b"),           toggleStatusBar),
    (([Control],        "u"),           toggleSourceMode),

    -- Prompt
    (([Control],        "o"),           promptURL False), 
    (([Control, Shift], "O"),           promptURL True),

    -- Search
    (([Shift],          "/"),           promptFind False True True),
    (([Shift],          "?"),           promptFind False False True),
    (([Control],        "n"),           findNext False True True),
    (([Control, Shift], "N"),           findNext False False True),

    -- Copy/paste
    (([Control],        "y"),           copyUri),
    (([Control, Shift], "Y"),           copyTitle),
    --(([],           "p"),           loadURIFromClipboard), -- /!\ UNSTABLE, can't see why...

    -- Others
    (([Control],        "i"),           showWebInspector),
    (([Control],        "p"),           printPage),
    (([Control],        "t"),           newWindow)
    ]
-- }}}

-- {{{ Web settings
-- Commented lines correspond to default values
myWebSettings :: IO WebSettings
myWebSettings = do
    settings <- webSettingsNew
    set settings [
        --SETTING                                      DEFAULT VALUE 
        --webSettingsCursiveFontFamily              := "serif",
        --webSettingsDefaultFontFamily              := "sans-serif",
        --webSettingsFantasyFontFamily              := ,
        --webSettingsMonospaceFontFamily            := "monospace",
        --webSettingsSansFontFamily                 := "sans-serif",
        --webSettingsSerifFontFamily                := "serif",
        --webSettingsDefaultFontSize                := ,
        --webSettingsDefaultMonospaceFontSize       := 10,
        --webSettingsMinimumFontSize                := 5,
        --webSettingsMinimumLogicalFontSize         := 5,
        --webSettingsAutoLoadImages                 := True,
        --webSettingsAutoShrinkImages               := True,
        --webSettingsDefaultEncoding                := "iso-8859-1",
        --webSettingsEditingBehavior                := EditingBehaviorWindows,
        --webSettingsEnableCaretBrowsing            := ,
        webSettingsEnableDeveloperExtras            := True,
        webSettingsEnableHtml5Database              := False,
        webSettingsEnableHtml5LocalStorage          := False,
        webSettingsEnableOfflineWebApplicationCache := True,
        webSettingsEnablePlugins                    := True,
        webSettingsEnablePrivateBrowsing            := False,
        webSettingsEnableScripts                    := True,
        webSettingsEnableSpellChecking              := True,
        webSettingsEnableUniversalAccessFromFileUris := True,
        webSettingsEnableXssAuditor                 := True,
        --webSettingsEnableSiteSpecificQuirks       := False,
        --webSettingsEnableDomPaste                 := False,
        --webSettingsEnableDefaultContextMenu       := True,
        webSettingsEnablePageCache                  := True,
        --webSettingsEnableSpatialNavigation        := False,
        --webSettingsEnforce96Dpi                   := ,
        webSettingsJSCanOpenWindowAuto              := True,
        --webSettingsPrintBackgrounds               := True,
        --webSettingsResizableTextAreas             := True,
        webSettingsSpellCheckingLang                := Just "en_US",
        --webSettingsTabKeyCyclesThroughElements    := True,
        webSettingsUserAgent                        := "Mozilla Firefox"
        --webSettingsUserStylesheetUri              := Nothing,
        --webSettingsZoomStep                       := 0.1
        ]
    return settings
-- }}}

-- {{{ Setup
mySetup :: Browser -> IO ()
mySetup browser = 
    let
        builder         = mBuilder      (mGUI browser)
        webView         = mWebView      (mGUI browser)
        scrollWindow    = mScrollWindow (mGUI browser)
        window          = mWindow       (mGUI browser)
    in do
        -- Default background (for status bar)
        widgetModifyBg window StateNormal (Color 0 0 10000)
        
        -- Status bar
        statusBarScrollPosition browser
        statusBarPressedKeys    browser
        statusBarLoadProgress   browser
        statusBarURI            browser

        _ <- on webView titleChanged $ \_ title ->
            set window [ windowTitle := ("hbro | " ++ title)]


        -- Special requests
        _ <- on webView downloadRequested $ \download -> do
            getUri <- downloadGetUri download
            _ <- case getUri of
                Just uri -> downloadHandler uri 
                _        -> return ()
            return True

        _ <- on webView mimeTypePolicyDecisionRequested $ \_ request mimetype policyDecision -> do
            getUri <- networkRequestGetUri request
            case (getUri, mimetype) of
                --(Just uri, 'a':'p':'p':'l':'i':'c':'a':'t':'i':'o':'n':'/':_) -> downloadHandler uri
                (Just uri, _) -> putStrLn $ mimetype ++ ": " ++ uri
                _             -> putStrLn "FIXME"

            return False


        -- History handler
        _ <- on webView loadFinished $ \_ -> do
            getUri   <- webViewGetUri webView
            getTitle <- webViewGetTitle webView
            case (getUri, getTitle) of
                (Just uri, Just title)  -> historyHandler uri title
                _                       -> return ()


        -- On navigating to a new URI
        -- Return True to forbid navigation, False to allow
        _ <- on webView navigationPolicyDecisionRequested $ \_ request action policyDecision -> do
            getUri      <- networkRequestGetUri request
            reason      <- webNavigationActionGetReason action
            mouseButton <- webNavigationActionGetButton action

            case getUri of
                Just ('m':'a':'i':'l':'t':'o':':':address) -> do
                    putStrLn $ "Mailing to: " ++ address
                    return True
                Just uri -> 
                    case mouseButton of
                        1 -> return False -- Left button 
                        2 -> runExternalCommand ("hbro -u \"" ++ uri ++ "\"") >> return True -- Middle button
                        3 -> return False -- Right button
                        _ -> return False -- No mouse button pressed
                _        -> return False

            
        -- On requesting new window
        _ <- on webView newWindowPolicyDecisionRequested $ \_ request action policyDecision -> do
            getUri <- networkRequestGetUri request
            case getUri of
                Just uri -> runExternalCommand $ "hbro -u \"" ++ uri ++ "\""
                _        -> putStrLn "ERROR: wrong URI given, unable to open window."

            return True

        -- Favicon
        --_ <- on webView iconLoaded $ \uri -> do something

        return ()
-- }}}
    
-- {{{ Handlers
downloadHandler :: String -> IO ()
downloadHandler uri = runExternalCommand $ "wget \"" ++ uri ++ "\""

historyHandler :: String -> String -> IO ()
historyHandler uri title = do
    configHome <- getEnv "XDG_CONFIG_HOME"
    runCommand (configHome ++ "/hbro/scripts/historyHandler.sh \"" ++ uri ++ "\" \"" ++ title ++ "\"") >> return ()
-- }}}

-- {{{ Bookmarks
bookmarksKeys :: KeysList
bookmarksKeys = [
--  ((modifiers,        key),           callback)
    (([Control],        "d"),           addToBookmarks),
    (([Control, Shift], "D"),           addAllInstancesToBookmarks),
    (([Alt],            "d"),           deleteTagFromBookmarks),
    (([Control],        "l"),           loadFromBookmarks),
    (([Control, Shift], "L"),           loadTagFromBookmarks)
    ]

addToBookmarks, addAllInstancesToBookmarks, loadFromBookmarks, loadTagFromBookmarks, deleteTagFromBookmarks :: Browser -> IO ()

addToBookmarks browser = do
    getUri      <- webViewGetUri (mWebView $ mGUI browser)
    configHome  <- getEnv "XDG_CONFIG_HOME"
    case getUri of
        Just uri -> prompt "Bookmark with tag:" "" False browser (\b -> do 
            tags <- entryGetText (mPromptEntry $ mGUI b)
            runExternalCommand $ configHome ++ "/hbro/scripts/bookmarks.sh add \"" ++ uri ++ "\" " ++ tags)
        _        -> return ()

addAllInstancesToBookmarks browser = do
    configHome <- getEnv "XDG_CONFIG_HOME"
    prompt "Bookmark all instances with tag:" "" False browser (\b -> do 
        tags <- entryGetText (mPromptEntry $ mGUI b)
        _    <- forkIO $ (runCommand (configHome ++ "/hbro/scripts/bookmarks.sh add-all " ++ (mSocketDir $ mConfiguration browser) ++ " " ++ tags)) >> return ()
        return())

loadFromBookmarks browser = do 
    pid         <- getProcessID
    configHome  <- getEnv "XDG_CONFIG_HOME"
    runExternalCommand $ configHome ++ "/hbro/scripts/bookmarks.sh load \"" ++ (mSocketDir $ mConfiguration browser) ++ "/hbro." ++ show pid ++ "\""

loadTagFromBookmarks browser = do
    configHome <- getEnv "XDG_CONFIG_HOME"
    runExternalCommand $ configHome ++ "/hbro/scripts/bookmarks.sh load-tag"

deleteTagFromBookmarks browser = do
    configHome <- getEnv "XDG_CONFIG_HOME"
    runExternalCommand $ configHome ++ "/hbro/scripts/bookmarks.sh delete-tag"
-- }}}
