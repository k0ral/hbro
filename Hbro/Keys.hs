{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TemplateHaskell, TupleSections #-}
-- | Key bindings model.
-- Designed to be imported as @qualified@.
module Hbro.Keys (
    Tree(..),
    Stroke,
    Bindings,
    Mode(..),
    Status(..),
    mode,
    strokes,
    StatusReader(..),
    StatusWriter(..),
    StatusState,
    mkStroke,
    merge,
    lookup,
    deserialize,
    prefixMod,
    serialize,
    toString,
    mkBinding,
    toBindings)
where

-- {{{ Imports
-- import Hbro.Util

import Control.Lens
import Control.Monad hiding(forM_)
-- import Control.Monad.Error hiding(forM_)
-- import Control.Monad.IO.Class
-- import Control.Monad.Reader hiding(forM_)
-- import Control.Monad.Trans.Control

import Data.Default
-- import Data.Foldable
import Data.Functor
-- import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S hiding(foldl)

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.Keys
-- import Graphics.UI.Gtk.General.Enums

import Prelude hiding(lookup, mapM_)
-- }}}


-- {{{ Types
-- | A tree implementation that labels edges
data Tree edge leaf = Empty | Leaf leaf | Branch (Map edge (Tree edge leaf)) deriving(Show)

-- | A single keystroke, i.e. a set of modifiers and a single key (its string description)
type Stroke = (Set Modifier, String)

-- | List of keys bound to actions
type Bindings m = Tree Stroke (m ())


data Mode = Normal | Insert deriving(Eq, Ord)

-- | Global state
data Status = Status {
    _mode     :: Mode,     -- ^ Current mode
    _strokes  :: [Stroke]  -- ^ Previous keystrokes
    }

instance Default Status where
    def = Status Normal []

makeLenses ''Status

-- | 'MonadReader' for 'Status'
class StatusReader m where
    readStatus :: Simple Lens Status a -> m a

-- | 'MonadWriter' for 'Status'
class StatusWriter m where
    writeStatus :: Simple Lens Status a -> a -> m ()

-- | 'MonadState' for 'Status'
type (StatusState m) = (StatusReader m, StatusWriter m)

{-instance Monoid KeyMap where
    mempty = KeyBindings M.empty
    mappend (KeyBindings a) (KeyBindings b) = KeyBindings (mappend a b)-}

instance Ord Modifier where
    compare x y = compare (show x) (show y)
-- }}}


mkStroke :: [Modifier] -> KeyVal -> Maybe Stroke
mkStroke m k = Just . (S.fromList m,) =<< toString k


--toTree :: Ord a => [([a], b)] -> Tree a b
--toTree = foldl merge Empty . map toBranch

toBranch :: Ord a => ([a], b) -> Tree a b
toBranch ([], a)    = Leaf a
toBranch ((h:t), a) = Branch (M.fromList [(h, toBranch (t, a))])

-- | In case of conflicts, the rightmost operand is preferred
merge :: Ord a => Tree a b -> Tree a b -> Tree a b
merge Empty      x          = x
merge x          Empty      = x
merge (Leaf _)   (Leaf b)   = Leaf b
merge (Leaf _)   (Branch b) = Branch b
merge (Branch _) (Leaf b)   = Leaf b
merge (Branch a) (Branch b) = Branch $ M.unionWith merge a b


lookup :: Ord a => [a] -> Tree a b -> Maybe (Tree a b)
lookup _     Empty      = Nothing
lookup []    (Leaf x)   = Just (Leaf x)
lookup []    x          = Just x
lookup _     (Leaf _)   = Nothing
lookup (h:t) (Branch m) = M.lookup h m >>= lookup t


-- | Convert a KeyVal to a String.
-- For printable characters, the corresponding String is returned, except for the space character for which "<Space>" is returned.
-- For non-printable characters, the corresponding keyName wrapped into "< >" is returned.
-- For modifiers, Nothing is returned.
toString :: KeyVal -> Maybe String
toString keyVal = case keyToChar keyVal of
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


serialize :: Stroke -> String
serialize (m, k) = S.foldr (++) "" (S.map serializeMod m) ++ k


serializeMod :: Modifier -> String
serializeMod Control = "C-"
-- serializeMod Shift   = "S-"
serializeMod Alt     = "M-"
serializeMod _       = ""

-- | Parse a 'String' representation of a keystrokes chain
deserialize :: String -> Maybe [Stroke]
deserialize ""          = Just []
deserialize (' ':t)     = deserialize t
deserialize ('C':'-':t) = prefixMod Control =<< deserialize t
deserialize ('M':'-':t) = prefixMod Alt     =<< deserialize t
-- deserialize ('S':'-':t) = prefixMod Shift   =<< deserialize t
deserialize (k:' ':t)   = prepend k <$> deserialize t
deserialize (k:t)       = prefixVal k =<< deserialize t


prefixMod :: Modifier -> [Stroke] -> Maybe [Stroke]
prefixMod modifier ((m, keys):t) = Just ((S.insert modifier m, keys):t)
prefixMod _        _             = Nothing


prefixVal :: Char -> [Stroke] -> Maybe [Stroke]
prefixVal k [] = Just [(S.empty, [k])]
prefixVal k ((modifiers, keys):t)
    | S.null modifiers = Just ((modifiers, k:keys):t)
    | otherwise        = Nothing


prepend :: Char -> [Stroke] -> [Stroke]
prepend k x = (S.empty, [k]):x


mkBinding :: String -> m () -> Maybe (Bindings m)
mkBinding keys action = toBranch . (, action) <$> deserialize keys

toBindings :: [(String, m ())] -> Bindings m
toBindings = foldl merge Empty . catMaybes . map (\(a, b) -> mkBinding a b)
