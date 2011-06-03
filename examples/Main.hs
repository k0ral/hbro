module Main where

-- {{{ Imports
import Hbro.Core
import Hbro.Gui
import Hbro.Util

import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import System.Cmd
-- }}}

main :: IO ()
main = browser Configuration {
    mError       = Nothing,
    mSocketDir   = "/tmp",
    mHomePage    = "https://www.google.com",

    mKeyBindings = [
--      ((Modifiers,    Key),       Callback)
        -- Browsing
        (([],           "<"),       goBack),
        (([Shift],      ">"),       goForward),
        (([],           "s"),       stop),
        (([],           "<F5>"),    reload True),
        (([Shift],      "<F5>"),    reload False),

        -- Zooming
        (([Shift],      "+"),       zoomIn),
        (([],           "-"),       zoomOut),

        -- Prompt
        (([],           "o"),       promptURL False), 
        (([Shift],      "O"),       promptURL True),

        -- Search
        (([Shift],      "/"),       promptFind False True),
        (([Shift],      "?"),       promptFind False False),
        (([],           "n"),       findNext False True),
        (([Shift],      "N"),       findNext False False),

        -- Others
        (([Control],    "i"),       showWebInspector),
        (([Control],    "u"),       toggleSourceMode),
        (([],           "t"),       toggleStatusBar),
        (([Control],    "p"),       printPage),
        (([],           "<F11>"),   fullscreen),
        (([],           "<Escape>"),   unfullscreen)
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

            _ <- on webView progressChanged $ \progress' ->
                labelSetMarkup progressLabel $ "<span foreground=\"yellow\">" ++ show progress' ++ "%</span>"

            _ <- on webView loadFinished $ \_ -> do
                labelSetMarkup progressLabel "<span foreground=\"green\">100%</span>"


            _ <- on webView loadError $ \_ _ _ -> do
                labelSetMarkup progressLabel "<span foreground=\"red\">ERROR</span>"
                return False

            _ <- on webView titleChanged $ \_ title -> do
                set window [ windowTitle := title]

            _ <- on webView downloadRequested $ \download -> do
                getUrl <- downloadGetUri download
                _ <- case getUrl of
                        Just url -> forkOS $ (rawSystem "wget" [url]) >> return ()
                        _        -> forkOS $ return ()
                return True

            _ <- on webView mimeTypePolicyDecisionRequested $ \_ request mimetype policyDecision -> do
                getUrl <- networkRequestGetUri request
                case getUrl of
                    Just url -> putStrLn $ mimetype ++ ": " ++ url
                    _        -> putStrLn "ERROR"

                return False

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
                            2 -> (runExternalCommand $ "hbro " ++ uri) >> return True -- Middle button
                            3 -> return False -- Right button
                            _ -> return False -- No mouse button pressed
                    _        -> return False


                
            -- On requesting new window
            _ <- on webView newWindowPolicyDecisionRequested $ \_ request action policyDecision -> do
                getUri <- networkRequestGetUri request
                case getUri of
                    Just uri -> runExternalCommand $ "hbro " ++ uri
                    _        -> putStrLn "ERROR: wrong URI given, unable to open window."

                return True

            return ()
    )}


-- Definitions
    where
        goBack :: GUI -> IO ()
        goBack gui = webViewGoBack (mWebView gui)

        goForward :: GUI -> IO ()
        goForward gui = webViewGoForward (mWebView gui)

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
            prompt "Open URL" "" False gui (\g -> do 
                uri <- entryGetText (mPrompt g)
                loadURL uri g)
        promptURL _ gui = do
            uri <- webViewGetUri (mWebView gui)
            case uri of
                Just url -> prompt "Open URL" url False gui (\g -> do
                                u <- entryGetText (mPrompt g)
                                loadURL u g)
                _ -> return ()

        promptFind :: Bool -> Bool -> GUI -> IO ()
        promptFind caseSensitive forward gui =
            prompt "Search" "" True gui (\gui' -> do
                keyWord <- entryGetText (mPrompt gui')
                webViewSearchText (mWebView gui) keyWord caseSensitive forward True
                return ())

        findNext :: Bool -> Bool -> GUI -> IO()
        findNext caseSensitive forward gui = do
            keyWord <- entryGetText (mPrompt gui)
            webViewSearchText (mWebView gui) keyWord caseSensitive forward True
            return ()

        printPage :: GUI -> IO ()
        printPage gui = do
            frame <- webViewGetMainFrame (mWebView gui)
            webFramePrint frame

        fullscreen :: GUI -> IO ()
        fullscreen gui = windowFullscreen (mWindow gui)

        unfullscreen :: GUI -> IO ()
        unfullscreen gui = windowUnfullscreen (mWindow gui)
