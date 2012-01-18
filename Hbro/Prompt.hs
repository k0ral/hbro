module Hbro.Prompt where

-- {{{ Imports
import Hbro.Types

import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Trans

--import Data.Foldable
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.WebKit.WebView hiding(webViewLoadUri)

import Prelude hiding(mapM_)

import System.Console.CmdArgs (whenLoud)
import System.Glib.Signals
-- }}}

init :: Builder -> WebView -> IO PromptBar
init builder webView = do
    label  <- builderGetObject builder castToLabel "promptDescription"
    labelSetAttributes label [
      AttrStyle  {paStart = 0, paEnd = -1, paStyle = StyleItalic},
      AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}]
    
    entry                  <- builderGetObject builder castToEntry "promptEntry"
    box                    <- builderGetObject builder castToHBox  "promptBox"
    callbackRef            <- newIORef (const $ return ())
    incrementalCallbackRef <- newIORef (const $ return ())
    
-- Validate/cancel prompt
    void $ on entry keyPressEvent $ do
      key      <- eventKeyName
      liftIO $ do  
        callback <- readIORef callbackRef

        when (key == "Return") $ entryGetText entry >>= callback
        when (key == "Return" || key == "Escape") $ do        
            widgetHide box
            writeIORef callbackRef            (const $ return ())
            writeIORef incrementalCallbackRef (const $ return ())
            widgetGrabFocus webView
      return False
    
-- Incremental behavior
    void $ on entry editableChanged $ do
        callback <- readIORef incrementalCallbackRef
        entryGetText entry >>= callback
    
    return $ PromptBar box label entry callbackRef incrementalCallbackRef

open :: PromptBar -> String -> String -> IO ()
open _promptBar@PromptBar {mBox = promptBox, mDescription = description, mEntry = entry} newDescription defaultText = do
    whenLoud $ putStrLn "Opening prompt."
    labelSetText description newDescription
    entrySetText entry defaultText
    
    widgetShow promptBox
    widgetGrabFocus entry
    editableSetPosition entry (-1)
    
-- | Open prompt bar with given description and default value,
-- and register a callback to trigger at validation.
read :: PromptBar         -- ^ 
     -> String            -- ^ Prompt description
     -> String            -- ^ Initial value
     -> (String -> IO ()) -- ^ Callback function to trigger when validating prompt value
     -> IO ()
read = read' False

-- | Same as 'prompt', but callback is triggered for each change in prompt's entry.
readIncremental :: PromptBar -> String -> String -> (String -> IO ()) -> IO ()
readIncremental = read' True

read' :: Bool -> PromptBar -> String -> String -> (String -> IO ()) -> IO ()
read' incremental promptBar description defaultText callback = do
    open promptBar description defaultText

-- Register callback
    case incremental of
        True -> writeIORef (mIncrementalCallbackRef promptBar) callback
        _    -> writeIORef (mCallbackRef promptBar)            callback
-- }}}
