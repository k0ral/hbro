{-# LANGUAGE DoRec #-}
module Hbro.Keys (
-- * Other
    defaultKeyHandler,
    emacsKeyHandler,
-- * Util
    stringify,
    keyToString,
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Monad hiding(forM_)
--import Control.Monad.Trans

--import Data.Foldable
import Data.IORef
import qualified Data.Map as M
--import qualified Data.Set as S

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys

import Prelude hiding(mapM_)

--import System.Console.CmdArgs (whenLoud)
--import System.Glib.Signals
-- }}}

-- | Look for a callback associated to the given keystrokes and trigger it, if any.
defaultKeyHandler :: KeysList -> String -> K (String, Bool)
defaultKeyHandler keysList keystrokes = case M.lookup keystrokes (M.fromList keysList) of
    Just callback -> callback >> return (keystrokes, True) 
    _             ->             return (keystrokes, False)

-- | Emacs-like key handler.
emacsKeyHandler :: KeysList     -- ^ Key bindings
                -> [String]     -- ^ List of prefix keys
                -> String       
                -> K (String, Bool)
emacsKeyHandler keysList prefixes keystrokes = do
    keysRef <- getState "Hbro.Keys.manageSequentialKeys" "" 
    io $ modifyIORef keysRef (++ keystrokes)
    chainedKeys <- io $ readIORef keysRef

    case elem chainedKeys prefixes of
        True -> do
            io $ modifyIORef keysRef (++ " ")
            return (chainedKeys ++ " ", False)
        _    -> do
            io $ writeIORef keysRef []
            defaultKeyHandler keysList chainedKeys


-- | Convert a KeyVal to a String.
-- For printable characters, the corresponding String is returned, except for the space character for which "<Space>" is returned.
-- For non-printable characters, the corresponding keyName wrapped into "< >" is returned.
-- For modifiers, Nothing is returned.
keyToString :: KeyVal -> Maybe String
keyToString keyVal = case keyToChar keyVal of
    Just ' '    -> Just "<Space>"
    Just char   -> Just [char]
    _           -> case keyName keyVal of
        "Caps_Lock"         -> Nothing
        "Shift_L"           -> Nothing
        "Shift_R"           -> Nothing
        "Control_L"         -> Nothing
        "Control_R"         -> Nothing
        "Alt_L"             -> Nothing
        "Alt_R"             -> Nothing
        "Super_L"           -> Nothing
        "Super_R"           -> Nothing
        "Menu"              -> Nothing
        "ISO_Level3_Shift"  -> Nothing
        "dead_circumflex"   -> Just "^"
        "dead_diaeresis"    -> Just "¨"
        x                   -> Just ('<':x ++ ">")

-- | Convert a Modifier to a String. 
stringify :: Modifier -> String
stringify Control = "C-"
--stringify' Shift   = "S-"
stringify Alt     = "M-"
stringify _       = []

-- | Convert key bindings list to a map.
-- keysListToMap :: KeysList -> KeysMap
-- keysListToMap = M.fromList . (map (\(a, b) -> ((S.fromList a, b), c)))
