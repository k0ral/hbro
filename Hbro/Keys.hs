{-# LANGUAGE DoRec #-}
module Hbro.Keys (
-- * Other
-- * Util
    stringify,
    keyToString,
    manageSequentialKeys
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Monad hiding(forM_)
--import Control.Monad.Trans

--import Data.Foldable
import Data.IORef
--import qualified Data.Map as M
--import qualified Data.Set as S

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys

import Prelude hiding(mapM_)

--import System.Console.CmdArgs (whenLoud)
--import System.Glib.Signals
-- }}}

instance Ord Modifier where
    m <= m' = fromEnum m <= fromEnum m'


manageSequentialKeys :: (String -> K (String, Bool)) -> String -> K (String, Bool)
manageSequentialKeys handler keystroke = do
    keysRef         <- getState "Hbro.Keys.manageSequentialKeys" "" 
    keys            <- io $ manageSequentialKeys' keysRef keystroke
    (keys', result) <- handler keys
    case result of
        True -> (io . modifyIORef keysRef $ const []) >> return ([], result)
        _    -> return (keys', result)

manageSequentialKeys' :: IORef String -> String -> IO String
manageSequentialKeys' previousKeys "<Escape>" = do
    writeIORef previousKeys []
    return []
manageSequentialKeys' previousKeys keystroke = do
    modifyIORef previousKeys (++ keystroke)
    return =<< readIORef previousKeys
 
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
