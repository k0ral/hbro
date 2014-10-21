{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
-- | Key bindings model.
-- Designed to be imported as @qualified@.
module Hbro.Keys (
-- * Modifiers
      Modifier(..)
    , _Alt
    , _Control
    , _Shift
    , modifier
-- * Mode
    , Mode(..)
-- * Bindings implementation
    , KeyStroke
    , keyStroke
    , Bindings
    , Status
    , Hooks
    , statusL
    , onKeyPressedL
-- * Interface
    , HasHooks(..)
    , initializeHooks
    , Hbro.Keys.set
    ) where

-- {{{ Imports
import           Hbro.Gdk.KeyVal
import           Hbro.Keys.Model            (keyL, modifiersL, (.|))
import qualified Hbro.Keys.Model            as Model
import           Hbro.Prelude

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.TH
import           Control.Monad.Reader       hiding (forM_, guard, mapM_)

import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Set                   as Set

import qualified Graphics.UI.Gtk.Gdk.EventM as Gdk

import           Text.Parsec
import           Text.Parsec.Text
-- }}}

-- {{{ Modifiers
newtype Modifier = Modifier Gdk.Modifier deriving(Eq)

instance Describable Modifier where
    describe (Modifier Gdk.Control) = "C-"
    describe (Modifier Gdk.Shift)   = "S-"
    describe (Modifier Gdk.Alt)     = "M-"
    describe (Modifier _)           = ""

instance Ord Modifier where compare = comparing describe

instance ToSet Modifier Modifier where toSet = Set.singleton

_Alt, _Control, _Shift :: Modifier
_Alt     = Modifier Gdk.Alt
_Control = Modifier Gdk.Control
_Shift   = Modifier Gdk.Shift

modifier :: Parser Modifier
modifier = spaces *> choice [ string "C-" >> return _Control
                            , string "M-" >> return _Alt
                            -- , string "S-" >> return _Shift
                            ]
-- }}}

-- {{{ Key mode (à la vi)
data Mode = Normal | Insert deriving(Eq, Ord)

instance Default Mode where def = Normal
-- }}}

-- {{{ Bindings implementation
type KeyStroke  = Model.KeyStroke Modifier KeyVal

instance Describable KeyStroke where
   describe s = foldr (++) "" (map describe . Set.toList $ s^.modifiersL) ++ describe (s^.keyL)

instance ToNonEmpty KeyStroke KeyStroke where
    toNonEmpty x = x :| []

instance ToNonEmpty KeyStroke KeyVal where
    toNonEmpty x = ((Set.empty .| x) :| [])

keyStroke :: Parser KeyStroke
keyStroke = do
    spaces
    m <- maybe Set.empty Set.singleton <$> optionMaybe modifier
    k <- keyVal
    return $ Model.KeyStroke m k

-- type Binding m  = Model.Binding  Hbro.Keys.Stroke (m ())
type Bindings m = Model.Bindings KeyStroke (m ())
type Status m   = Model.Status KeyStroke Mode (m ())

declareLenses [d|
  data Hooks m    = Hooks
    { statusL       :: TVar (Status m)
    , onKeyPressedL :: TMVar ([KeyStroke] -> m ())
    }
  |]

class HasHooks m t | t -> m where _hooks :: Lens' t (Hooks m)

instance HasHooks m (Hooks m) where _hooks = id

initializeHooks :: IO (Hooks m)
initializeHooks = Hooks <$> newTVarIO def <*> newEmptyTMVarIO

set :: (BaseIO m, MonadReader r m, HasHooks n r) => Lens' (Hooks n) (TMVar a) -> a -> m ()
set l v = atomically . (`writeTMVar` v) =<< askL (_hooks.l)
-- }}}


{-instance Monoid KeyMap where
    mempty = KeyBindings M.empty
    mappend (KeyBindings a) (KeyBindings b) = KeyBindings (mappend a b)-}
