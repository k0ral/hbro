{-# LANGUAGE DoRec #-}
module Hbro.Keys (
-- * Key event callbacks  
    withKeys,
    simpleKeyEventCallback,
-- * Key event handlers    
    simpleKeyEventHandler,
    advancedKeyEventHandler,
-- * Util
    keyToString,
    keysListToMap  
) where

-- {{{ Imports
import Hbro.Types

--import Control.Monad
import Control.Monad.Trans

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.WebKit.WebView

import Prelude hiding(mapM_)

import System.Console.CmdArgs (whenLoud, whenNormal)
import System.Glib.Signals
-- }}}

instance Ord Modifier where
    m <= m' =  fromEnum m <= fromEnum m'

-- | Retrieve modifiers and pressed keys, and forward them to a handler.
withKeys :: ([Modifier] -> String -> IO ()) -> EventM EKey Bool
withKeys handler = do
    value      <- eventKeyVal
    modifiers  <- eventModifier

    liftIO $ mapM_ (handler modifiers) (keyToString value)

    return False

-- | Look for a callback associated to the given modifiers and pressed keys and trigger it, if any.
simpleKeyEventCallback :: KeysMap -> KeyEventCallback
simpleKeyEventCallback keysMap modifiers keys = do
    whenLoud $ putStr ("Key pressed: " ++ show modifiers ++ keys ++ " ")
        
    case M.lookup (S.fromList modifiers, keys) keysMap of
        Just callback -> callback >> (whenLoud $ putStrLn "(mapped)") >> return True
        _ -> (whenLoud $ putStrLn "(unmapped)") >> return False

-- | Basic key handler which doesn't support sequential keystrokes.
simpleKeyEventHandler :: KeyEventCallback -> ConnectId WebView -> WebView -> EventM EKey Bool
simpleKeyEventHandler callback _ _ = withKeys (\x y -> callback x y >> return ())

-- | Key handler with sequential keystrokes support.
advancedKeyEventHandler :: KeyEventCallback -> ConnectId WebView -> WebView -> EventM EKey Bool
advancedKeyEventHandler = advancedKeyEventHandler' []
  
advancedKeyEventHandler' :: String -> KeyEventCallback -> ConnectId WebView -> WebView -> EventM EKey Bool
advancedKeyEventHandler' previousKeys callback oldID webView = withKeys $ \modifiers newKey -> do
    let keys       = previousKeys ++ newKey
    let newHandler = \x -> do
        rec newID <- after webView keyPressEvent $ advancedKeyEventHandler' x callback newID webView
        return ()
            
    signalDisconnect oldID
    result <- callback modifiers keys
    case result of
        True -> newHandler []
        _    -> case newKey of
            "<Escape>" -> newHandler []
            _          -> newHandler keys

-- | Convert a keyVal to a String.
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

-- | Convert key bindings list to a map.
keysListToMap :: KeysList -> KeysMap
keysListToMap = M.fromList . (map (\((a,b),c) -> ((S.fromList a, b), c)))
