module Hbro.Prompt where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Monad hiding(forM_, mapM_)
--import Control.Monad.Trans

import Data.Foldable
import Data.IORef

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Layout.HBox

import Network.URI

import Prelude hiding(mapM_)
-- }}}

init :: Builder -> IO PromptBar
init builder = do
    label <- builderGetObject builder castToLabel "promptDescription"
    labelSetAttributes label [allItalic, allBold]

    entry                  <- builderGetObject builder castToEntry "promptEntry"
    box                    <- builderGetObject builder castToHBox  "promptBox"
    callbackRef            <- newIORef (const $ return () :: String -> K ())
    incrementalCallbackRef <- newIORef (const $ return () :: String -> K ())
        
    return $ PromptBar box label entry callbackRef incrementalCallbackRef

open :: String -> String -> K ()
open newDescription defaultText = with (mPromptBar . mGUI) $ \(PromptBar promptBox description entry _ _) -> do
    logVerbose "Opening prompt."
    labelSetText description newDescription
    entrySetText entry defaultText
    
    widgetShow promptBox
    widgetGrabFocus entry
    editableSetPosition entry (-1)

-- | Close prompt, clean its content and callbacks
clean :: K ()
clean = with (mPromptBar . mGUI) $ \(PromptBar box _ entry cRef iRef) -> do
     widgetRestoreText entry StateNormal 
     widgetHide box
     writeIORef cRef (const $ return ())
     writeIORef iRef (const $ return ())
   
    
-- | Open prompt bar with given description and default value,
-- and register a callback to trigger at validation.
read :: String           -- ^ Prompt description
     -> String           -- ^ Initial value
     -> (String -> K ()) -- ^ Callback function to trigger when validating prompt value
     -> K ()
read = read' False

-- | Same as 'prompt', but callback is triggered for each change in prompt's entry.
incrementalRead, iread :: String -> String -> (String -> K ()) -> K ()
incrementalRead = read' True
-- | Alias for incrementalRead.
iread           = incrementalRead

read' :: Bool -> String -> String -> (String -> K ()) -> K ()
read' incremental description startValue callback = do
    open description startValue
    with (mPromptBar . mGUI) $ \promptBar -> case incremental of
        True -> writeIORef (mIncrementalCallbackRef promptBar) callback
        _    -> writeIORef (mCallbackRef            promptBar) callback

-- | Same as "read" for URI values
readURI :: String -> String -> (URI -> K ()) -> K ()
readURI description startValue callback = withK (mPromptBar . mGUI) $ \promptBar -> do
    open description startValue
    checkURI startValue
    
    io . writeIORef (mIncrementalCallbackRef promptBar) $ checkURI
    io . writeIORef (mCallbackRef            promptBar) $ mapM_ callback . parseURIReference
  where
    checkURI value = with (mEntry . mPromptBar . mGUI) $ \entry -> do
        widgetModifyText entry StateNormal color
      where
        color = case isURIReference value of
            True -> Color     0 65535 0
            _    -> Color 65535     0 0
