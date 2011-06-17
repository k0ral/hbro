module Main where

-- {{{ Imports
import Hbro.Core 
import Hbro.Gui 
import Hbro.Util 

import Control.Monad.Trans(liftIO)

import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.Windows.Window

-- import Paths_hbro -- Doesn't work for now

import System.Environment
import System.Glib.Attributes
import System.Glib.Signals
import System.Process 
import System.Posix.Process
-- }}}

main :: IO ()
main = do
  --uiFile <- getDataFileName "examples/ui.xml" -- Doesn't work for now
  configHome <- getEnv "XDG_CONFIG_HOME"
    
  hbro Configuration {
    -- Do not change this
    mError = Nothing,

    -- Directory where 0MQ sockets will be created
    mSocketDir = socketDir,

    -- XML file defining UI (used by GtkBuilder) 
    --mUIFile = uiFile, -- Doesn't work for now
    -- Use this line if you want to use your own UI file in ~/.config/hbro/ui.xml
    mUIFile = configHome ++ "/hbro/ui.xml",

    -- URI loaded at startup
    mHomePage = "https://www.google.com",


    -- Custom keys
    -- All callbacks are fed with the GUI instance
    -- Note 1 : for modifiers, lists are used for convenience purposes,
    --          but are transformed into sets in hbro's internal machinery,
    --          so that order and repetition don't matter
    -- Note 3 : for printable characters accessed via the shift modifier,
    --          you do have to include Shift in modifiers list
    mKeyBindings = [
--      ((modifiers,    key),           callback)
        -- Browse
        (([],           "<"),           goBack),
        (([Shift],      ">"),           goForward),
        (([],           "s"),           stop),
        (([],           "<F5>"),        reload True),
        (([Shift],      "<F5>"),        reload False),
        (([],           "^"),           horizontalHome),
        (([],           "$"),           horizontalEnd),
        (([],           "<Home>"),      verticalHome),
        (([],           "<End>"),       verticalEnd),

        -- Display
        (([Shift],      "+"),           zoomIn),
        (([],           "-"),           zoomOut),
        (([],           "<F11>"),       fullscreen),
        (([],           "<Escape>"),    unfullscreen),
        (([],           "t"),           toggleStatusBar),
        (([Control],    "u"),           toggleSourceMode),

        -- Prompt
        (([],           "o"),           promptURL False), 
        (([Shift],      "O"),           promptURL True),

        -- Search
        (([Shift],      "/"),           promptFind False True True),
        (([Shift],      "?"),           promptFind False False True),
        (([],           "n"),           findNext False True True),
        (([Shift],      "N"),           findNext False False True),

        -- Copy/paste
        (([],           "y"),           copyUri),
        (([Control],    "y"),           copyTitle),
        --(([],           "p"),           pasteUri), -- /!\ UNSTABLE, can't see why...

        -- Bookmarks
        (([Control],   "d"),            addToBookmarks),
        (([Control],   "l"),            loadFromBookmarks),

        -- Others
        (([Control],    "i"),           showWebInspector),
        (([Control],    "p"),           printPage)
    ],


    -- Various web settings
    -- Commented lines correspond to default values
    -- For more details, please refer to WebSettings documentation
    mWebSettings = do
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
        return settings,


    -- Custom callbacks
    mAtStartUp = \gui -> (let
            builder         = mBuilder gui
            webView         = mWebView gui
            scrollWindow    = mScrollWindow gui
            window          = mWindow gui
        in do
            progressLabel   <- builderGetObject builder castToLabel "progress"
            uriLabel        <- builderGetObject builder castToLabel "uri"
            scrollLabel     <- builderGetObject builder castToLabel "scroll"
            keysLabel       <- builderGetObject builder castToLabel "keys"

            -- Default background (for status bar)
            widgetModifyBg window StateNormal (Color 0 0 10000)

            -- Scroll position in status bar
            adjustment <- scrolledWindowGetVAdjustment scrollWindow
            _ <- onValueChanged adjustment $ do
                current <- adjustmentGetValue adjustment
                lower   <- adjustmentGetLower adjustment
                upper   <- adjustmentGetUpper adjustment
                page    <- adjustmentGetPageSize adjustment
                
                case upper-lower-page of
                    0 -> labelSetMarkup scrollLabel "ALL"
                    x -> labelSetMarkup scrollLabel $ show (round $ current/x*100) ++ "%"

            -- Pressed keys in statusbar
            _ <- after webView keyPressEvent $ do
                value      <- eventKeyVal
                modifiers  <- eventModifier

                let keyString = keyToString value
                case keyString of 
                    Just string -> liftIO $ labelSetMarkup keysLabel $ "<span foreground=\"green\">" ++ show modifiers ++ string ++ "</span>"
                    _           -> return ()

                return False

            -- Page load
            _ <- on webView loadStarted $ \_ -> do
                labelSetMarkup progressLabel "<span foreground=\"red\">0%</span>"

            _ <- on webView loadCommitted $ \_ -> do
                getUri <- (webViewGetUri webView)
                case getUri of 
                    Just uri -> labelSetMarkup uriLabel $ "<span weight=\"bold\" foreground=\"white\">" ++ escapeMarkup uri ++ "</span>"
                    _        -> labelSetMarkup uriLabel "<span weight=\"bold\" foreground=\"red\">ERROR</span>"

            _ <- on webView progressChanged $ \progress' ->
                labelSetMarkup progressLabel $ "<span foreground=\"yellow\">" ++ show progress' ++ "%</span>"

            _ <- on webView loadFinished $ \_ -> do
                labelSetMarkup progressLabel "<span foreground=\"green\">100%</span>"

                getUri   <- webViewGetUri webView
                getTitle <- webViewGetTitle webView
                case (getUri, getTitle) of
                    (Just uri, Just title)  -> historyHandler uri title
                    _                       -> return ()

            _ <- on webView loadError $ \_ _ _ -> do
                labelSetMarkup progressLabel "<span foreground=\"red\">ERROR</span>"
                return False

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
                    Just uri -> runExternalCommand $ "hbro " ++ uri
                    _        -> putStrLn "ERROR: wrong URI given, unable to open window."

                return True

            _ <- on webView hoveringOverLink $ \title hoveredUri -> do
                getUri <- (webViewGetUri webView)
                case (hoveredUri, getUri) of
                    (Just u, _) -> labelSetMarkup uriLabel $ "<span foreground=\"#5555ff\">" ++ escapeMarkup u ++ "</span>"
                    (_, Just u) -> labelSetMarkup uriLabel $ "<span foreground=\"white\" weight=\"bold\">" ++ escapeMarkup u ++ "</span>"
                    _           -> putStrLn "FIXME"

            
            -- Favicon
            --_ <- on webView iconLoaded $ \uri -> do something

            return ()
    )}


-- Definitions
    where
        -- Constants
        scriptsDir :: String
        scriptsDir = "~/.config/hbro/scripts/"

        socketDir :: String
        socketDir = "/tmp"

        -- Browse
        goBack :: GUI -> IO ()
        goBack gui = webViewGoBack (mWebView gui)

        goForward :: GUI -> IO ()
        goForward gui = webViewGoForward (mWebView gui)

        stop :: GUI -> IO ()
        stop gui = webViewStopLoading (mWebView gui)

        reload :: Bool -> GUI -> IO ()
        reload True gui = webViewReload (mWebView gui)
        reload _    gui = webViewReloadBypassCache (mWebView gui)

        -- Zoom
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

        promptFind :: Bool -> Bool -> Bool -> GUI -> IO ()
        promptFind caseSensitive forward wrap gui =
            prompt "Search" "" True gui (\gui' -> do
                keyWord <- entryGetText (mPrompt gui')
                found   <- webViewSearchText (mWebView gui) keyWord caseSensitive forward wrap
                return ())

        findNext :: Bool -> Bool -> Bool -> GUI -> IO()
        findNext caseSensitive forward wrap gui = do
            keyWord <- entryGetText (mPrompt gui)
            found   <- webViewSearchText (mWebView gui) keyWord caseSensitive forward wrap 
            return ()

        printPage :: GUI -> IO ()
        printPage gui = do
            frame <- webViewGetMainFrame (mWebView gui)
            webFramePrint frame

        fullscreen :: GUI -> IO ()
        fullscreen gui = windowFullscreen (mWindow gui)

        unfullscreen :: GUI -> IO ()
        unfullscreen gui = windowUnfullscreen (mWindow gui)


        -- Copy/paste
        copyUri :: GUI -> IO ()
        copyUri gui = do
            getUri <- webViewGetUri (mWebView gui)
            case getUri of
                Just u -> runCommand ("echo -n " ++ u ++ " | xclip") >> return ()
                _      -> return ()

        copyTitle :: GUI -> IO ()
        copyTitle gui = do
            getTitle <- webViewGetTitle (mWebView gui)
            case getTitle of
                Just t -> runCommand ("echo -n " ++ t ++ " | xclip") >> return ()
                _      -> return ()

        pasteUri :: GUI -> IO ()
        pasteUri gui = do
            uri <- readProcess "xclip" ["-o"] []
            loadURL uri gui


        -- Scrolling
        verticalHome :: GUI -> IO ()
        verticalHome gui = do
            adjustment  <- scrolledWindowGetVAdjustment (mScrollWindow gui)
            lower       <- adjustmentGetLower adjustment

            adjustmentSetValue adjustment lower

        verticalEnd :: GUI -> IO ()
        verticalEnd gui = do
            adjustment  <- scrolledWindowGetVAdjustment (mScrollWindow gui)
            upper       <- adjustmentGetUpper adjustment

            adjustmentSetValue adjustment upper

        horizontalHome :: GUI -> IO ()
        horizontalHome gui = do
            adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow gui)
            lower       <- adjustmentGetLower adjustment

            adjustmentSetValue adjustment lower

        horizontalEnd :: GUI -> IO ()
        horizontalEnd gui = do
            adjustment  <- scrolledWindowGetHAdjustment (mScrollWindow gui)
            upper       <- adjustmentGetUpper adjustment

            adjustmentSetValue adjustment upper 

        -- Handlers
        downloadHandler :: String -> IO ()
        downloadHandler uri = runExternalCommand $ "wget \"" ++ uri ++ "\""

        historyHandler :: String -> String -> IO ()
        historyHandler uri title = runCommand (scriptsDir ++ "/historyHandler.sh \"" ++ uri ++ "\" \"" ++ title ++ "\"") >> return ()


        -- Bookmarks
        addToBookmarks :: GUI -> IO ()
        addToBookmarks gui = do
            getUri <- webViewGetUri (mWebView gui)
            case getUri of
                Just uri -> prompt "Bookmark with tag:" "" False gui (\g -> do 
                    tags <- entryGetText (mPrompt g)
                    runExternalCommand $ scriptsDir ++ "bookmarks.sh add " ++ uri ++ " " ++ tags)
                _        -> return ()

        loadFromBookmarks :: GUI -> IO ()
        loadFromBookmarks gui = do 
            pid <- getProcessID
            runExternalCommand $ scriptsDir ++ "bookmarks.sh load \"" ++ socketDir ++ "/hbro." ++ show pid ++ "\""

