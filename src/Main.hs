module Main where

-- {{{ Imports
import Browser
import Gui

import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import System.Cmd
-- }}}

main :: IO ()
main = browser Configuration {
    mError       = Nothing,
    mHomePage    = "https://www.google.com",

    mKeyBindings = [
--      ((Mod,          Key),       Callback)
        -- Browsing
        (([],           "<"),       back),
        (([Shift],      ">"),       forward),
        (([],           "s"),       stop),
        (([],           "<F5>"),    reload True),
        (([Shift],      "<F5>"),    reload False),

        -- Zooming
        (([Shift],      "+"),       zoomIn),
        (([],           "-"),       zoomOut),

        -- Prompt
        (([],           "o"),       promptURL False), 
        (([Shift],      "O"),       promptURL True),

        -- Others
        (([Control],    "i"),       showWebInspector),
        (([Control],    "u"),       toggleSourceMode),
        (([],           "t"),       toggleStatusBar),
        (([Control],    "p"),       print)
    ],

    mWebSettings = (do
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
            webSettingsUserAgent                        := "Mozilla/5.0 (Windows; U; Windows NT 6.1; ru; rv:1.9.2.3) Gecko/20100401 Firefox/4.0 (.NET CLR 3.5.30729)"
            --webSettingsUserStylesheetUri              := Nothing,
            --webSettingsZoomStep                       := 0.1 
            ]
        return settings),

    -- Custom callbacks
    mCustomizations = \gui -> (let
            webView         = mWebView gui
            progressLabel   = mProgressLabel gui
            urlLabel        = mUrlLabel gui
            window          = mWindow gui
        in do
            _ <- on webView loadStarted $ \_ -> do 
                labelSetMarkup progressLabel "<span foreground=\"red\">0%</span>"

            _ <- on webView loadCommitted $ \_ -> do
                getUrl <- (webViewGetUri webView)
                case getUrl of 
                    Just url -> labelSetMarkup urlLabel url 
                    _        -> labelSetMarkup urlLabel ""

            _ <- on webView progressChanged $ \progress ->
                labelSetMarkup progressLabel $ "<span foreground=\"yellow\">" ++ show progress ++ "%</span>"

            _ <- on webView loadFinished $ \_ -> 
                labelSetMarkup progressLabel "<span foreground=\"green\">100%</span>"

            _ <- on webView loadError $ \_ _ _ -> do
                labelSetMarkup progressLabel "<span foreground=\"red\">ERROR</span>"
                return False

            _ <- on webView titleChanged $ \_ title -> do
                set window [ windowTitle := title]

            _ <- on webView downloadRequested $ \download -> do
                getUrl <- downloadGetUri download
                _ <- case getUrl of
                        Just url -> forkOS $ do rawSystem "wget" [url]; return ()
                        _        -> forkOS $ do rawSystem "pwd"  []; return ()
                return True

            _ <- on webView mimeTypePolicyDecisionRequested $ \_ request mimetype policyDecision -> do
                putStrLn mimetype
                getUrl <- networkRequestGetUri request
                case getUrl of
                    Just url -> putStrLn url
                    _        -> putStrLn "ERROR"

                return False

            _ <- on webView newWindowPolicyDecisionRequested $ \_ request action policyDecision -> do
                getUrl <- networkRequestGetUri request
                case getUrl of
                    Just url -> putStrLn ("New Window: " ++ url)
                    _        -> putStrLn "ERROR"

                return True

            return ()
    )}


-- Definitions
    where
        back :: GUI -> IO ()
        back gui = webViewGoBack (mWebView gui)

        forward :: GUI -> IO ()
        forward gui = webViewGoForward (mWebView gui)

        stop :: GUI -> IO ()
        stop gui = webViewStopLoading (mWebView gui)

        reload :: Bool -> GUI -> IO ()
        reload True gui = webViewReload (mWebView gui)
        reload _    gui = webViewReloadBypassCache (mWebView gui)

        zoomIn :: GUI -> IO ()
        zoomIn gui = webViewZoomIn (mWebView gui)

        zoomOut :: GUI -> IO ()
        zoomOut gui = webViewZoomOut (mWebView gui)

        toggleSourceMode :: GUI -> IO ()
        toggleSourceMode gui = do
            currentMode <- webViewGetViewSourceMode (mWebView gui)
            webViewSetViewSourceMode (mWebView gui) (not currentMode)

        -- TODO
        toggleStatusBar :: GUI -> IO ()
        toggleStatusBar gui = return()


        promptURL :: Bool -> GUI -> IO ()        
        promptURL False gui = 
            prompt "Open URL" "" gui (\g -> do 
                uri <- entryGetText (mPrompt g)
                loadURL uri g)
        promptURL _ gui = do
            uri <- webViewGetUri (mWebView gui)
            case uri of
                Just url -> prompt "Open URL" url gui (\g -> do
                                u <- entryGetText (mPrompt g)
                                loadURL u g)
                _ -> return ()

        print :: GUI -> IO ()
        print gui = do
            frame <- webViewGetMainFrame (mWebView gui)
            webFramePrint frame

