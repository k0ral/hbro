module Hbro.Extra.StatusBar where

-- {{{ Imports
--import Hbro.Types
import Hbro.Util 

import Control.Monad.Trans(liftIO)

import Data.List

import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView

import Network.URL

import System.Glib.Signals
-- }}}


-- | Write current scroll position in the given Label.
setupScrollWidget :: Label -> ScrolledWindow -> IO ()
setupScrollWidget widget window = do
    labelSetAttributes widget [
        AttrForeground {paStart = 0, paEnd = -1, paColor = Color 32767 32767 32767}
        ]
      
    adjustment  <- scrolledWindowGetVAdjustment window

    _ <- onValueChanged adjustment $ do
        current <- adjustmentGetValue    adjustment
        lower   <- adjustmentGetLower    adjustment
        upper   <- adjustmentGetUpper    adjustment
        page    <- adjustmentGetPageSize adjustment
        
        case upper-lower-page of
            0 -> labelSetText widget "ALL"
            x -> labelSetText widget $ show (round $ current/x*100) ++ "%"

    labelSetText widget "0%"


-- | /!\ Doesn't work for now.
-- Write current zoom level in the given Label.
statusBarZoomLevel :: Label -> WebView -> IO ()
statusBarZoomLevel widget webView = do
    zoomLevel <- webViewGetZoomLevel webView
            
    labelSetMarkup widget $ "<span foreground=\"white\">x" ++ escapeMarkup (show zoomLevel) ++ "</span>"


-- | Display pressed keys in status bar.
-- Needs a Label entitled "keys" from the builder.
statusBarPressedKeys :: Label -> WebView -> IO ()
statusBarPressedKeys widget webView = do
    labelSetAttributes widget [
        AttrForeground {paStart = 0, paEnd = -1, paColor = Color 0 65535 0}
        ]
    
    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value
        case (keyString, modifiers) of 
            (Just k, [])    -> liftIO $ labelSetText widget k
            (Just k, m)     -> liftIO $ labelSetText widget (show m ++ k)
            _               -> return ()

        return False
    return ()


-- | Write current load progress in the given Label.
statusBarLoadProgress :: Label -> WebView -> IO ()
statusBarLoadProgress widget webView = do
-- Load started
    _ <- on webView loadStarted $ \_ -> do
        labelSetAttributes widget [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 0 0}
            ]
        labelSetText widget "0%"
-- Progress changed    
    _ <- on webView progressChanged $ \progress' -> do
        labelSetAttributes widget [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 65535 0}
            ]
        labelSetText widget $ show progress' ++ "%"
-- Load finished
    _ <- on webView loadFinished $ \_ -> do
        labelSetAttributes widget [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 0 65535 0}
            ]
        labelSetText widget "100%"
-- Error
    _ <- on webView loadError $ \_ _ _ -> do
        labelSetAttributes widget [
            AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 0 0}
            ]
        labelSetText widget "ERROR"
        return False
    
    return ()


-- | Write current URI, or the destination of a hovered link, in the given Label.
statusBarURI :: Label -> WebView -> IO ()
statusBarURI widget webView = do
-- URI changed
    _ <- on webView loadCommitted $ \_ -> do
        getUri <- (webViewGetUri webView) 
        maybe (return ()) (flip setURILabel widget) getUri

-- Hovering link
    _ <- on webView hoveringOverLink $ \_title hoveredUri -> do
        getUri <- (webViewGetUri webView)
        let uri = case (hoveredUri, getUri) of
                             (Just u, _) -> u
                             (_, Just u) -> u
                             (_, _)      -> "ERROR"
        
        setURILabel uri widget
        
    return ()

  where
    setURILabel uri label = do
        let uri' = importURL uri
        case uri' of
            Just u -> do
                case url_type u of
                    Absolute host_ -> do
                        let host'           = exportHost host_
                        let secure'         = secure host_
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
                         
