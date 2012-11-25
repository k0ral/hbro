{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Hbro.Keys where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Control.Monad hiding(forM_)
import Control.Monad.Error hiding(forM_)
--import Control.Monad.IO.Class
import Control.Monad.Reader hiding(forM_)
import Control.Monad.Trans.Control

--import Data.Foldable
import Data.Functor
import Data.IORef
import qualified Data.Map as M
--import qualified Data.Set as S

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.Keys

import Prelude hiding(mapM_)

--import System.Console.CmdArgs (whenLoud)
--import System.Glib.Signals
-- }}}

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



-- | Look for a callback associated to the given keystrokes and trigger it, if any.
defaultKeyHandler :: KeysList -> KeyHook
defaultKeyHandler (KeysList keysList) keystrokes = case M.lookup keystrokes (M.fromList keysList) of
    Just callback -> callback
    _             -> return ()

-- | Emacs-like key handler.
emacsKeyHandler :: (MonadIO m, MonadReader r m, HasConfig r, HasOptions r, HasGUI r, HasPromptBar r, HasZMQContext r, HasHooks r, HasKeys r, MonadError HError m, MonadBaseControl IO m)
                => KeysList     -- ^ Key bindings
                -> [String]     -- ^ List of prefix keys
                -> String
                -> m ()
emacsKeyHandler keysList prefixes keystrokes = do
    keys        <- asks _keys
    chainedKeys <- (++ keystrokes) <$> io (readIORef keys)
    logVerbose $ "Pressed keys: " ++ chainedKeys

    case elem chainedKeys prefixes of
        True -> do
            io $ writeIORef keys $ chainedKeys ++ " "
        _    -> do
            io $ writeIORef keys ""
            defaultKeyHandler keysList chainedKeys



-- | Convert key bindings list to a map.
-- keysListToMap :: KeysList -> KeysMap
-- keysListToMap = M.fromList . (map (\(a, b) -> ((S.fromList a, b), c)))
