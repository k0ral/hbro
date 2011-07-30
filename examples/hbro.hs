module Main where

-- {{{ Imports
import Hbro.Config
import Hbro.Core
import qualified Hbro.Extra.Bookmarks as Bookmarks
import qualified Hbro.Extra.BookmarksQueue as Queue
import Hbro.Extra.Clipboard
import Hbro.Extra.History
import Hbro.Extra.Misc
import Hbro.Extra.Prompt
import Hbro.Extra.StatusBar
import Hbro.Gui
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebPolicyDecision
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window

import System.Environment
import System.Glib.Attributes
import System.Glib.Signals
-- import System.Posix.Process
import System.Process 
-- }}}


main :: IO ()
main = do
    configHome <- getEnv "XDG_CONFIG_HOME"

    -- See Hbro.Types.Configuration documentation for fields description
    -- Commented out fields indicate default values
    hbro defaultConfiguration {
        --mSocketDir    = "/tmp/",
        mUIFile         = configHome ++ "/hbro/ui.xml",
        --mHomePage     = "https://www.google.com",
        mKeys           = myKeys,
        mWebSettings    = myWebSettings,
        mSetup          = mySetup
    }


-- {{{ Keys
-- Note that this example is suited for an azerty keyboard.
myKeys :: KeysList
myKeys = generalKeys ++ bookmarksKeys ++ historyKeys

generalKeys :: KeysList
generalKeys = [
--  ((modifiers,        key),           callback)
    -- Browse
    (([Control],        "<Left>"),      goBack),
    (([Shift],          "<Right>"),     goForward),
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
    (([Control],        "g"),           promptGoogle),

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
    (([Control],        "f"),           promptFind False True True),
    (([Shift],          "?"),           promptFind False False True),
    (([Control],        "n"),           findNext False True True),
    (([Control, Shift], "N"),           findNext False False True),

    -- Copy/paste
    (([Control],        "y"),           copyUri),
    (([Control, Shift], "Y"),           copyTitle),
    (([Control],        "p"),           loadURIFromClipboard),

    -- Others
    (([Control],        "i"),           showWebInspector),
    (([Alt],            "p"),           printPage),
    (([Control],        "t"),           \_ -> newWindow),
    (([Control],        "w"),           \_ -> mainQuit)
    ]

bookmarksKeys :: KeysList
bookmarksKeys = [
    (([Control],        "d"),           Bookmarks.addWithTags),
    (([Control, Shift], "D"),           Bookmarks.addAllWithTags),
    (([Alt],            "d"),           \_ -> Bookmarks.deleteWithTag),
    (([Control],        "l"),           Bookmarks.load),
    (([Control, Shift], "L"),           \_ -> Bookmarks.loadWithTag),
    (([Control],        "q"),           Queue.append),
    (([Alt],            "q"),           \b -> do
        uri <- Queue.popFront
        loadURI uri b)
    ]

historyKeys :: KeysList
historyKeys = [
    (([Control],        "h"),           loadFromHistory)
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
        --webSettingsEnableCaretBrowsing              := False,
        webSettingsEnableDeveloperExtras            := True,
        --webSettingsEnableHtml5Database              := True,
        --webSettingsEnableHtml5LocalStorage          := True,
        --webSettingsEnableOfflineWebApplicationCache := True,
        webSettingsEnablePlugins                    := True,
        webSettingsEnablePrivateBrowsing            := False, -- Experimental
        webSettingsEnableScripts                    := True,
        --webSettingsEnableSpellChecking              := False,
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
        webSettingsUserAgent                        := "Mozilla/5.0 (X11; Linux x86_64; rv:2.0.1) Gecko/20100101 Firefox/4.0.1"
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


        -- Download requests
        _ <- on webView downloadRequested $ \download -> do
            uri  <- downloadGetUri download
            name <- downloadGetSuggestedFilename download
            size <- downloadGetTotalSize download

            case (uri, name) of
                (Just uri', Just name') -> myDownload uri' name' 
                _ -> return ()
            return False

        -- Per MIME actions
        _ <- on webView mimeTypePolicyDecisionRequested $ \_ request mimetype policyDecision -> do
            show <- webViewCanShowMimeType webView mimetype

            case (show, mimetype) of
                (True, _) -> webPolicyDecisionUse policyDecision >> return True
                _         -> webPolicyDecisionDownload policyDecision >> return True


        -- History handler
        _ <- on webView loadFinished $ \_ -> do
            uri   <- webViewGetUri   webView
            title <- webViewGetTitle webView
            case (uri, title) of
                (Just uri', Just title') -> addToHistory uri' title'
                _ -> return ()


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

    
myDownload :: String -> String -> IO ()
myDownload uri name = do
    home <- getEnv "HOME"
    --runExternalCommand $ "wget \"" ++ uri ++ "\" -O \"" ++ home ++ "/" ++ name ++ "\""
    --runExternalCommand $ "axel \"" ++ uri ++ "\" -o \"" ++ home ++ "/" ++ name ++ "\""
    runExternalCommand $ "aria2c \"" ++ uri ++ "\" -d " ++ home ++ "/ -o \"" ++ name ++ "\""

promptGoogle :: Browser -> IO ()
promptGoogle browser = 
    prompt "Google search" "" False browser (\browser' -> do
        keyWords <- entryGetText (mPromptEntry $ mGUI browser')
        loadURI ("https://www.google.com/search?q=" ++ keyWords) browser'
        return ())

