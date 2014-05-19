{-# LANGUAGE TemplateHaskell, TupleSections #-}
-- | Key bindings model.
-- Designed to be imported as @qualified@.
module Hbro.Keys (
-- * Modifiers
    Modifier(..),
    _Alt,
    _Control,
    _Shift,
-- * Mode
    Mode(..),
-- * Bindings implementation
    KeyStroke,
    Bindings,
    Status,
    Hooks,
    statusL,
    onKeyPressedL,
-- * Interface
    HasHooks(..),
    initializeHooks,
    Hbro.Keys.set,
) where

-- {{{ Imports
import Hbro.Gdk.KeyVal
import Hbro.Keys.Model ((.|), modifiersL, keyL)
import qualified Hbro.Keys.Model as Model
import Hbro.Util hiding(Control, lookup)

import Control.Concurrent.STM
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad hiding(forM_, guard, mapM_)
import Control.Monad.Reader hiding(forM_, guard, mapM_)

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord
import Data.Set as S hiding(foldl)

import qualified Graphics.UI.Gtk.Gdk.EventM as Gdk

import Prelude as P hiding(foldl, lookup, mapM_)
-- }}}

-- {{{ Modifiers
newtype Modifier = Modifier Gdk.Modifier deriving(Eq)

instance Show Modifier where
    show (Modifier Gdk.Control) = "C-"
    show (Modifier Gdk.Shift)   = "S-"
    show (Modifier Gdk.Alt)     = "M-"
    show (Modifier _)           = ""

instance Ord Modifier where compare = comparing show

instance Read Modifier where
    readsPrec _ []          = []
    readsPrec n (' ':t)     = readsPrec n t
    readsPrec _ ('C':'-':t) = (_Control, t) : []
    readsPrec _ ('M':'-':t) = (_Alt, t) : []
    -- readsPrec ('S':'-':t) = (Modifier Gdk.Shift, t)
    readsPrec _ _           = []

instance ToSet Modifier Modifier where toSet = S.singleton

_Alt, _Control, _Shift :: Modifier
_Alt     = Modifier Gdk.Alt
_Control = Modifier Gdk.Control
_Shift   = Modifier Gdk.Shift
-- }}}

-- {{{ Key mode (à la vi)
data Mode = Normal | Insert deriving(Eq, Ord)

instance Default Mode where def = Normal
-- }}}

-- {{{ Bindings implementation
type KeyStroke  = Model.KeyStroke Modifier KeyVal

instance Show KeyStroke where
   show s = S.foldr (++) "" (S.map show $ s^.modifiersL) ++ show (s^.keyL)

instance Read KeyStroke where
    readsPrec _ []      = []
    readsPrec n string  = case readKey of
        []                -> []
        [(key', string'')] -> [(modifiers' .| key', string'')]
        _                 -> []
      where
        readKey              = readsPrec n string'
        (modifiers', string') = foldl (\(a,_) (c,d) -> (S.insert c a, d)) (S.empty, []) $ readModifiers string
        readModifiers s      = case readsPrec n s of
            []              -> []
            [(modifier, t)] -> (modifier, t):(readModifiers t)
            _               -> []
    readList = maybeToList . fmap ((,"") . P.map fst) . sequence . P.map (listToMaybe . reads) . words

instance ToNonEmpty KeyStroke KeyStroke where
    toNonEmpty x = x :| []

instance ToNonEmpty KeyStroke KeyVal where
    toNonEmpty x = ((S.empty .| x) :| [])


-- type Binding m  = Model.Binding  Hbro.Keys.Stroke (m ())
type Bindings m = Model.Bindings KeyStroke (m ())
type Status m   = Model.Status KeyStroke Mode (m ())
data Hooks m    = Hooks
    { _status       :: TVar (Status m)
    , _onKeyPressed :: TMVar ([KeyStroke] -> m ())
    }

makeLensesWith ?? ''Hooks $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

class HasHooks m t | t -> m where _hooks :: Lens' t (Hooks m)

instance HasHooks m (Hooks m) where _hooks = id

initializeHooks :: IO (Hooks m)
initializeHooks = Hooks <$> newTVarIO def <*> newEmptyTMVarIO

set :: (MonadBase IO m, MonadReader r m, HasHooks n r) => Lens' (Hooks n) (TMVar a) -> a -> m ()
set l v = io . atomically . (`writeTMVar` v) =<< askl (_hooks.l)
-- }}}


{-instance Monoid KeyMap where
    mempty = KeyBindings M.empty
    mappend (KeyBindings a) (KeyBindings b) = KeyBindings (mappend a b)-}
