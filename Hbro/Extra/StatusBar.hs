module Hbro.Extra.StatusBar where

-- {{{ Imports
--import Hbro.Keys
import Hbro.Types
--import Hbro.Util 

import Control.Monad hiding(forM_, mapM_)

import Data.Foldable
import Data.List
import Data.Maybe

import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

import Prelude hiding(mapM_)

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
    

-- | 
withFeedback :: Label -> KeyEventCallback -> [Modifier] -> String -> IO Bool
withFeedback widget callback modifiers keys = do  
-- Trigger callback
    result <- callback modifiers keys
    
-- Set color depending on result
    let color = case result of
            True -> Color 0 65535 0
            _    -> Color 65535 0 0
        
    labelSetAttributes widget [
        AttrForeground {paStart = 0, paEnd = -1, paColor = color}
        ]
    
-- Write keystrokes state to label
    case (modifiers, isSuffixOf "<Escape>" keys) of
        (_ , True) -> labelSetText widget []
        ([], _)    -> labelSetText widget keys
        (_, _)     -> labelSetText widget (show modifiers ++ keys)
          
    return result
  

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
setupURIWidget :: URIColors -> URIColors -> Label -> WebView -> IO ()
setupURIWidget normalColors secureColors widget webView = do
-- URI changed
    _ <- on webView loadCommitted $ \_ ->
        (mapM_ (labelSetURI normalColors secureColors widget)) =<< ((>>= parseURIReference) `fmap` (webViewGetUri webView))
                                          
-- Link (un)hovered
    _ <- on webView hoveringOverLink $ \_title hoveredURI -> do
        uri <- webViewGetUri webView
        
        forM_ (hoveredURI >>= parseURIReference) $ labelSetURI normalColors secureColors widget
        unless (isJust hoveredURI) $ forM_ (uri >>= parseURIReference) (labelSetURI normalColors secureColors widget)
                
    return ()


-- | 
labelSetURI :: URIColors -> URIColors -> Label -> URI -> IO ()
labelSetURI normalColors secureColors widget uri = do
    let colors = case uriScheme uri of
          "https:" -> secureColors
          _        -> normalColors
          
    let i:j:k:l:_ = map length [
          uriScheme uri,
          maybe "" uriRegName (uriAuthority uri),
          uriPath uri,
          uriQuery uri
          ]
 
    labelSetAttributes widget $ [
        AttrWeight{     paStart = 0,         paEnd = -1,          paWeight = WeightBold },
        AttrForeground{ paStart = 0,         paEnd = i+2,         paColor = mScheme colors },
        AttrForeground{ paStart = i+2,       paEnd = i+2+j,       paColor = mHost colors },
        AttrForeground{ paStart = i+2+j,     paEnd = i+2+j+k,     paColor = mPath colors },
        AttrForeground{ paStart = i+2+j+k,   paEnd = i+2+j+k+l,   paColor = mQuery colors },
        AttrForeground{ paStart = i+2+j+k+l, paEnd = -1,          paColor = mFragment colors }
        ]
                        
    labelSetText widget (show uri)
                         
          
data URIColors = URIColors {
    mScheme     :: Color,
    mHost       :: Color,
    mPort       :: Color,
    mUser       :: Color,
    mPath       :: Color,
    mQuery      :: Color,
    mFragment   :: Color
}

defaultURIColors :: URIColors
defaultURIColors = URIColors {
    mScheme   = Color 20000 20000 20000,
    mHost     = Color 50000 50000 50000,
    mPort     = Color 65535     0     0,
    mUser     = Color     0 65535     0,
    mPath     = Color     0 65535 65535,
    mQuery    = Color 20000 20000 20000,
    mFragment = Color 10000 10000 65535
}


defaultSecureURIColors :: URIColors
defaultSecureURIColors = defaultURIColors {
    mHost     = Color 50000 50000     0
}