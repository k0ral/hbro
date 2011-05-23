module Util where

import Graphics.UI.Gtk

-- |Converts a keyVal to a String.
-- For printable characters, the corresponding String is returned, except for the space character for which "<Space>" is returned.
-- For non-printable characters, the corresponding keyName between <> is returned.
-- For modifiers, Nothing is returned.
keyToString :: KeyVal -> Maybe String
keyToString keyVal = case keyToChar keyVal of
    Just ' '    -> Just "<Space>"
    Just char   -> Just [char]
    _           -> case keyName keyVal of
        "Caps_Lock" -> Nothing
        "Shift_L"   -> Nothing
        "Shift_R"   -> Nothing
        "Control_L" -> Nothing
        "Control_R" -> Nothing
        "Alt_L"     -> Nothing
        "Alt_R"     -> Nothing
        "Super_L"   -> Nothing
        "Super_R"   -> Nothing
        "Menu"      -> Nothing
        "ISO_Level3_Shift" -> Nothing
        x           -> Just ('<':x ++ ">")
