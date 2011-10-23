module Hbro.Extra.StatusBar where

-- {{{ Imports
import Hbro.Types
import Hbro.Util 

import Control.Monad.Trans(liftIO)

import Data.List

import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView

import Network.URL

import System.Glib.Signals
-- }}}


-- | Display scroll position in status bar.
-- Needs a Label entitled "scroll" from the builder.
statusBarScrollPosition :: Browser -> IO ()
statusBarScrollPosition browser = do
    scrollLabel <- builderGetObject builder castToLabel "scroll"
    adjustment  <- scrolledWindowGetVAdjustment scrollWindow

    _ <- onValueChanged adjustment $ do
        current <- adjustmentGetValue    adjustment
        lower   <- adjustmentGetLower    adjustment
        upper   <- adjustmentGetUpper    adjustment
        page    <- adjustmentGetPageSize adjustment
        
        case upper-lower-page of
            0 -> labelSetMarkup scrollLabel "ALL"
            x -> labelSetMarkup scrollLabel $ show (round $ current/x*100) ++ "%"

    labelSetMarkup scrollLabel "0%"

  where
    builder      = mBuilder      (mGUI browser)
    scrollWindow = mScrollWindow (mGUI browser)


-- | Display current zoom level in status bar.
-- Needs a Label entitled "zoom" from the builder.
statusBarZoomLevel :: Browser -> IO ()
statusBarZoomLevel browser = do
    zoomLevel <- webViewGetZoomLevel webView
    zoomLabel <- builderGetObject builder castToLabel "zoom"
            
    labelSetMarkup zoomLabel $ "<span foreground=\"white\">x" ++ escapeMarkup (show zoomLevel) ++ "</span>"
  where
    builder = mBuilder (mGUI browser)
    webView = mWebView (mGUI browser)



-- | Display pressed keys in status bar.
-- Needs a Label entitled "keys" from the builder.
statusBarPressedKeys :: Browser -> IO ()
statusBarPressedKeys browser = 
  let
    builder         = mBuilder      (mGUI browser)
    webView         = mWebView      (mGUI browser)
  in do
    keysLabel <- builderGetObject builder castToLabel "keys"
    labelSetAttributes keysLabel [
        AttrForeground {paStart = 0, paEnd = -1, paColor = Color 0 65535 0}
        ]
    
    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value
        case (keyString, modifiers) of 
            (Just k, [])    -> liftIO $ labelSetText keysLabel k
            (Just k, m)     -> liftIO $ labelSetText keysLabel (show m ++ k)
            _               -> return ()

        return False
    return ()


-- | Display load progress in status bar.
-- Needs a Label entitled "progress" from the builder.
statusBarLoadProgress :: Browser -> IO ()
statusBarLoadProgress browser = 
  let
    builder         = mBuilder      (mGUI browser)
    webView         = mWebView      (mGUI browser)
  in do
    progressLabel <- builderGetObject builder castToLabel "progress"
-- Load started
    _ <- on webView loadStarted $ \_ -> do
        labelSetAttributes progressLabel [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 0 0}
            ]
        labelSetText progressLabel "0%"
-- Progress changed    
    _ <- on webView progressChanged $ \progress' -> do
        labelSetAttributes progressLabel [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 65535 0}
            ]
        labelSetText progressLabel $ show progress' ++ "%"
-- Load finished
    _ <- on webView loadFinished $ \_ -> do
        labelSetAttributes progressLabel [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 0 65535 0}
            ]
        labelSetText progressLabel "100%"
-- Error
    _ <- on webView loadError $ \_ _ _ -> do
        labelSetAttributes progressLabel [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 0 0}
            ]
        labelSetText progressLabel "ERROR"
        return False
    
    return ()


-- | Display current URI, or the destination of a hovered link, in the status bar.
-- Needs a Label named "uri" from the builder.
statusBarURI :: Browser -> IO ()
statusBarURI browser = do
    uriLabel <- builderGetObject builder castToLabel "uri"
-- URI changed
    _ <- on webView loadCommitted $ \_ -> do
        getUri <- (webViewGetUri webView) 
        maybe (return ()) (\x -> setURILabel x uriLabel) getUri

-- Hovering link
    _ <- on webView hoveringOverLink $ \_title hoveredUri -> do
        getUri <- (webViewGetUri webView)
        let uri = case (hoveredUri, getUri) of
                             (Just u, _) -> u
                             (_, Just u) -> u
                             (_, _)      -> "ERROR"
        
        setURILabel uri uriLabel
        
    return ()

  where
    builder = mBuilder (mGUI browser)
    webView = mWebView (mGUI browser)
    setURILabel uri label = do
        let uri' = importURL uri
        case uri' of
            Just u -> do
                case url_type u of
                    Absolute host -> do
                        let host'           = exportHost host
                        let secure'         = secure host
                        let protocolColor   = Color 20000 20000 20000
                        let hostColor       = case secure' of
                              True -> Color 0 65535 0
                              _    -> Color 50000 50000 50000
                        let pathColor       = Color 20000 20000 65535
                        let parametersColor = Color 20000 20000 20000
                        let i               = findIndex (isPrefixOf "://") (tails host')
                        let j               = length host'
                        
                        
                        let colorAttributes = case i of
                                Just n -> [
                                  AttrForeground {paStart = 0, paEnd = n+3, paColor = protocolColor},
                                  AttrForeground {paStart = n+3, paEnd = j, paColor = hostColor},
                                  AttrForeground {paStart = j, paEnd = -1, paColor = pathColor}
                                  ]
                                _ -> [
                                  AttrForeground {paStart = 0, paEnd = j, paColor = hostColor},
                                  AttrForeground {paStart = j, paEnd = -1, paColor = pathColor}
                                  ]
                       
                        
                        labelSetAttributes label $ [AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}] ++ colorAttributes
                        
                        labelSetText label uri
                    _ -> return ()
            _ -> return ()
                         
