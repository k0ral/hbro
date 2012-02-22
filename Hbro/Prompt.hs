module Hbro.Prompt where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
--import Hbro.Util

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

import System.Console.CmdArgs (whenLoud)
-- }}}

init :: Builder -> IO PromptBar
init builder = do
    label  <- builderGetObject builder castToLabel "promptDescription"
    labelSetAttributes label [
      AttrStyle  {paStart = 0, paEnd = -1, paStyle = StyleItalic},
      AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}]
    
    entry                  <- builderGetObject builder castToEntry "promptEntry"
    box                    <- builderGetObject builder castToHBox  "promptBox"
    callbackRef            <- newIORef (const $ return () :: String -> K ())
    incrementalCallbackRef <- newIORef (const $ return () :: String -> K ())
        
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
read :: String           -- ^ Prompt description
     -> String           -- ^ Initial value
     -> (String -> K ()) -- ^ Callback function to trigger when validating prompt value
     -> K ()
read = read' False

-- | Same as 'prompt', but callback is triggered for each change in prompt's entry.
incrementalRead, iread :: String -> String -> (String -> K ()) -> K ()
incrementalRead = read' True
iread           = incrementalRead

read' :: Bool -> String -> String -> (String -> K ()) -> K ()
read' incremental description startValue callback = with (mPromptBar . mGUI) $ \promptBar -> do
    open promptBar description startValue

-- Register callback
    case incremental of
        True -> writeIORef (mIncrementalCallbackRef promptBar) callback
        _    -> writeIORef (mCallbackRef            promptBar) callback

-- | "read"" for URI values
readURI :: String
        -> String
        -> (URI -> K ())
        -> K ()
readURI description startValue callback = with (mPromptBar . mGUI) $ \promptBar -> do
    open promptBar description startValue
    
--  writeIORef (mIncrementalCallbackRef promptBar) checkURI
    writeIORef (mCallbackRef            promptBar) $ mapM_ callback . parseURIReference
    
--checkURI :: String -> K ()
--checkURI value = case isURI value of
--    True -> 
