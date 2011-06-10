module Hbro.Util where

import Hbro.Gui (GUI)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Graphics.UI.Gtk
import System.Process


instance Ord Modifier where
    m <= m' =  fromEnum m <= fromEnum m'

-- | Converts a keyVal to a String.
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
        x                   -> Just ('<':x ++ ">")

-- | Converts key bindings list to a map.
-- | Calls importKeyBindings'.
importKeyBindings :: [(([Modifier], String), (GUI -> IO ()))] -> Map.Map (Set.Set Modifier, String) (GUI -> IO ()) 
importKeyBindings list = Map.fromList $ importKeyBindings' list

-- | Converts modifiers list to modifiers sets.
-- The order of modifiers in key bindings don't matter.
-- Called by importKeyBindings.
importKeyBindings' :: [(([Modifier], String), (GUI -> IO ()))] -> [((Set.Set Modifier, String), (GUI -> IO ()))]
importKeyBindings' (((a, b), c):t) = ((Set.fromList a, b), c):(importKeyBindings' t)
importKeyBindings' _ = []


-- | Like run `runCommand`, but return IO ()
runCommand' :: String -> IO ()
runCommand' command = runCommand command >> return ()

-- | Run external command and won't kill when parent process exit.
-- nohup for ignore all hup signal. 
-- `> /dev/null 2>&1` redirect all stdout (1) and stderr (2) to `/dev/null`
runExternalCommand :: String -> IO ()
runExternalCommand command = runCommand' $ "nohup " ++ command ++ " > /dev/null 2>&1"
