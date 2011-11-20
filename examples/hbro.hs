module Main where

-- {{{ Imports
import Hbro.Core
import qualified Hbro.Extra.Bookmarks as Bookmarks
import qualified Hbro.Extra.BookmarksQueue as Queue
import Hbro.Extra.Clipboard
import qualified Hbro.Extra.History as History
import Hbro.Extra.Misc
import Hbro.Extra.Prompt
import Hbro.Extra.Session
import Hbro.Extra.StatusBar
import Hbro.Gui
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
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

import System.Directory
import System.Environment
import System.Glib.Attributes
import System.Glib.Signals
-- import System.Posix.Process
import System.Process 
-- }}}

-- | Main function, basically launches hbro.
main :: IO ()
main = do
    home <- getHomeDirectory                  
    launchHbro $ myConfig home


-- | Application parameters.
-- See Hbro.Types.Parameters documentation for fields description.
-- Commented out fields indicate default values.
myConfig :: String -> Config
myConfig home = defaultConfig {
    --mSocketDir = "/tmp/",
    mUIFile      = home ++ "/.config/hbro/ui.xml",
    mHomePage    = "https://duckduckgo.com",
    mKeys        = myKeys home,
    mWebSettings = myWebSettings,
    mSetup       = mySetup
}


myHistoryFile :: FilePath -> FilePath
myHistoryFile home = home ++ "/.config/hbro/history"

myBookmarksFile :: FilePath -> FilePath
myBookmarksFile home = home ++ "/.config/hbro/bookmarks"


-- {{{ Keys
-- Note that this example is suited for an azerty keyboard.
myKeys :: FilePath -> Environment -> KeysList
myKeys home environment@Environment {mGUI = gui, mConfig = config} = let
    window         = mWindow       gui
    webView        = mWebView      gui
    scrolledWindow = mScrollWindow gui
    statusBox      = mStatusBox    gui
    promptBar      = mPromptBar    gui
    promptEntry    = mEntry promptBar
    bookmarksFile  = myBookmarksFile home
    historyFile    = myHistoryFile   home
  in  
    [
--  ((modifiers,        key),           callback)
-- Browse
    (([Control],        "<Left>"),      webViewGoBack    webView),
    (([Control],        "<Right>"),     webViewGoForward webView),
    (([Alt],            "<Left>"),      (goBackList    webView ["-l", "10"]) >>= maybe (return ()) (loadURI webView)),
    (([Alt],            "<Right>"),     (goForwardList webView ["-l", "10"]) >>= maybe (return ()) (loadURI webView)),
    (([Control],        "s"),           webViewStopLoading       webView),
    (([],               "<F5>"),        webViewReload            webView),
    (([Control],        "<F5>"),        webViewReloadBypassCache webView),
    (([Control],        "^"),           goLeft   scrolledWindow),
    (([Control],        "$"),           goRight  scrolledWindow),
    (([Control],        "<Home>"),      goTop    scrolledWindow),
    (([Control],        "<End>"),       goBottom scrolledWindow),
    (([Alt],            "<Home>"),      goHome webView config),
    (([Control],        "g"),           prompt "Google search" "" (\words -> loadURI webView ("https://www.google.com/search?q=" ++ words)) promptBar),

-- Display
    (([Control, Shift], "+"),           webViewZoomIn    webView),
    (([Control],        "-"),           webViewZoomOut   webView),
    (([],               "<F11>"),       windowFullscreen   window),
    (([],               "<Escape>"),    windowUnfullscreen window),
    (([Control],        "b"),           toggleVisibility statusBox),
    (([Control],        "u"),           toggleSourceMode webView),

-- Prompt
    (([Control],        "o"),           prompt "Open URL " "" (loadURI webView) promptBar),
    (([Control, Shift], "O"),           webViewGetUri webView >>= maybe (return ()) (\uri -> prompt "Open URL " uri (loadURI webView) promptBar)),

-- Search
    (([Shift],          "/"),           promptIncremental "Search " "" (\word -> webViewSearchText webView word False True True >> return ()) promptBar),
    (([Control],        "f"),           promptIncremental "Search " "" (\word -> webViewSearchText webView word False True True >> return ()) promptBar),
    (([Shift],          "?"),           promptIncremental "Search " "" (\word -> webViewSearchText webView word False False True >> return ()) promptBar),
    (([Control],        "n"),           entryGetText promptEntry >>= \word -> webViewSearchText webView word False True True >> return ()),
    (([Control, Shift], "N"),           entryGetText promptEntry >>= \word -> webViewSearchText webView word False False True >> return ()),

-- Copy/paste
    (([Control],        "y"),           webViewGetUri   webView >>= maybe (return ()) toClipboard),
    (([Control, Shift], "Y"),           webViewGetTitle webView >>= maybe (return ()) toClipboard),
    (([Control],        "p"),           withClipboard $ maybe (return ()) (loadURI webView)),
    (([Control, Shift], "P"),           withClipboard $ maybe (return ()) (\uri -> spawn "hbro" ["-u", uri])),

-- Others
    (([Control],        "i"),           showWebInspector webView),
    (([Alt],            "p"),           printPage        webView),
    (([Control],        "t"),           newInstance),
    (([Control],        "w"),           mainQuit),

-- Bookmarks
    (([Control],        "d"),           webViewGetUri webView >>= maybe (return ()) (\uri -> prompt "Bookmark with tags:" "" (\tags -> Bookmarks.add bookmarksFile uri (words tags)) promptBar)),
--    (([Control, Shift], "D"),           prompt "Bookmark all instances with tag:" "" (\tags -> mapM (sendCommandToAll "GET_URI") >>= map (flip Bookmarks.add tags)) . mGUI),
    (([Alt],            "d"),           Bookmarks.deleteWithTag bookmarksFile ["-l", "10"]),
    (([Control],        "l"),           Bookmarks.select        bookmarksFile ["-l", "10"] >>= maybe (return ()) (loadURI webView)),
    (([Control, Shift], "L"),           Bookmarks.selectTag     bookmarksFile ["-l", "10"] >>= maybe (return ()) (\uris -> mapM (\uri -> spawn "hbro" ["-u", uri]) uris >> return ())),
--    (([Control],        "q"),           webViewGetUri webView >>= maybe (return ()) (Queue.append),
--    (([Alt],            "q"),           \b -> do
--        uri <- Queue.popFront
--        loadURI uri b),

-- History
    (([Control],        "h"),           History.select historyFile ["-l", "10"] >>= maybe (return ()) (loadURI webView)),
    
-- Session
    --(([Alt],            "l"),           loadFromSession ["-l", "10"])
    ]
-- }}}

-- {{{ Web settings
-- Commented lines correspond to default values
myWebSettings :: [AttrOp WebSettings]
myWebSettings = [
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
    webSettingsEnableScripts                    := False,
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
-- }}}

-- {{{ Setup
mySetup :: Environment -> IO ()
mySetup environment@Environment {mGUI = gui} = 
    let
        builder         = mBuilder      gui 
        webView         = mWebView      gui
        scrolledWindow  = mScrollWindow gui
        window          = mWindow       gui
    in do        
    -- Scroll position in status bar
        scrollLabel <- builderGetObject builder castToLabel "scroll"
        setupScrollWidget scrollLabel scrolledWindow
    
    -- Zoom level in status bar
        zoomLabel <- builderGetObject builder castToLabel "zoom"
        statusBarZoomLevel zoomLabel webView
        
    -- Pressed keys in status bar
        keysLabel <- builderGetObject builder castToLabel "keys"
        statusBarPressedKeys keysLabel webView
        
    -- Load progress in status bar
        progressLabel <- builderGetObject builder castToLabel "progress"
        statusBarLoadProgress progressLabel webView
        
    -- Current URI in status bar
        uriLabel <- builderGetObject builder castToLabel "uri"
        statusBarURI uriLabel webView

    -- Session manager
        --setupSession browser

        _ <- on webView titleChanged $ \_ title ->
            set window [ windowTitle := ("hbro | " ++ title)]

    -- Download requests
        _ <- on webView downloadRequested $ \download -> do
            uri  <- downloadGetUri download
            name <- downloadGetSuggestedFilename download
            size <- downloadGetTotalSize download
            feedbackLabel <- builderGetObject builder castToLabel "feedback"

            case (uri, name) of
                (Just uri', Just name') -> do
                    myDownload uri' name' 
                    labelSetMarkupTemporary feedbackLabel "<span foreground=\"green\">Download started</span>" 5000
                _ -> labelSetMarkupTemporary feedbackLabel "<span foreground=\"red\">Unable to download</span>" 5000
            return False

    -- Per MIME actions
        _ <- on webView mimeTypePolicyDecisionRequested $ \_ request mimetype policyDecision -> do
            show <- webViewCanShowMimeType webView mimetype

            case (show, mimetype) of
                (True, _) -> webPolicyDecisionUse policyDecision >> return True
                _         -> webPolicyDecisionDownload policyDecision >> return True

    -- History handler
        home <- getHomeDirectory
        _ <- on webView loadFinished $ \_ -> do
            uri   <- webViewGetUri   webView
            title <- webViewGetTitle webView
            case (uri, title) of
                (Just uri', Just title') -> History.add (myHistoryFile home) uri' title'
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
                        2 -> spawn "hbro" ["-u", uri] >> putStrLn uri >> return True -- Middle button
                        3 -> return False -- Right button
                        _ -> return False -- No mouse button pressed
                _        -> return False
            
    -- On requesting new window
        _ <- on webView newWindowPolicyDecisionRequested $ \_ request action policyDecision -> do
            getUri <- networkRequestGetUri request
            case getUri of
                Just uri -> (spawn "hbro" ["-u", uri]) >> putStrLn uri
                _        -> putStrLn "ERROR: wrong URI given, unable to open window."

            return True

    -- Favicon
        --_ <- on webView iconLoaded $ \uri -> do something

        return ()
-- }}}

    
myDownload :: String -> String -> IO ()
myDownload uri name = do
    home <- getHomeDirectory
    --spawn "wget [uri, "-O", home ++ "/" ++ name]
    --spawn "axel [uri, "-o", home ++ "/" ++ name]
    spawn "aria2c" [uri, "-d", home ++ "/", "-o", name]
