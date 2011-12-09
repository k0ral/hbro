module Hbro.Keys where

-- {{{ Imports
import Hbro.Types

import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Set as S

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.WebKit.WebView

import System.Console.CmdArgs (whenLoud, whenNormal)
import System.Glib.Signals
-- }}}

instance Ord Modifier where
    m <= m' =  fromEnum m <= fromEnum m'

-- | Basic key handler which doesn't support sequential keystrokes either modes.
setupSimpleKeyHandler :: WebView -> KeysMap -> IO ()
setupSimpleKeyHandler webView bindings = do
    _ <- after webView keyPressEvent $ do
        value      <- eventKeyVal
        modifiers  <- eventModifier

        let keyString = keyToString value

        liftIO $ case keyString of 
            Just string -> do 
                whenLoud $ putStr ("Key pressed: " ++ show modifiers ++ string ++ " ")
                case M.lookup (S.fromList modifiers, string) bindings of
                    Just callback -> callback >> (whenLoud $ putStr "(mapped)")
                    _ -> whenLoud $ putStr "(unmapped)"
            _ -> return ()

        return False
    return ()


-- | Convert a keyVal to a String.
-- For printable characters, the corresponding String is returned, except for the space character for which "<Space>" is returned.
-- For non-printable characters, the corresponding keyName between <> is returned.
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

-- | Convert key bindings list to a map.
-- Calls importKeyBindings'.
keysListToMap :: KeysList -> KeysMap
keysListToMap list = M.fromList $ importKeyBindings' list

-- | Convert modifiers list to modifiers sets.
-- The order of modifiers in key bindings don't matter.
-- Called by importKeyBindings.
importKeyBindings' :: KeysList -> [((S.Set Modifier, String), IO ())]
importKeyBindings' (((a, b), c):t) = ((S.fromList a, b), c):(importKeyBindings' t)
importKeyBindings' _ = []

