{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
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
-- * KeyMap implementation
    , KeyStroke
    , keyStrokes
    , KeyMap
-- * Interface
    , KeyPressed(..)
    , KeyMapPressed(..)
    , bindKeys
    ) where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Event
import           Hbro.Gdk.KeyVal
import           Hbro.Keys.Model                 ((.|))
import qualified Hbro.Keys.Model                 as Model
import           Hbro.Logger
import           Hbro.Prelude                    hiding (isPrefixOf)

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Trans.Resource

import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map                        as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import qualified Graphics.UI.Gtk.Gdk.EventM      as Gdk

import           Text.Parsec                     hiding (many)
import           Text.Parsec.Text
-- }}}

-- {{{ Modifiers
instance Describable Gdk.Modifier where
    describe Gdk.Control = "C-"
    describe Gdk.Shift   = "S-"
    describe Gdk.Alt     = "M-"
    describe _           = ""

deriving instance Ord Gdk.Modifier

instance Describable (Modifier, KeyVal) where
  describe (m, k) = describe m <> describe k

newtype Modifier = Modifier (Set Gdk.Modifier) deriving(Eq)

instance Semigroup Modifier where
  (<>) = mappend

instance Monoid Modifier where
  mempty = Modifier mempty
  (Modifier a) `mappend` (Modifier b) = Modifier (a `mappend` b)

instance Describable Modifier where
  describe (Modifier x) = mconcat $ map describe $ Set.toList x

instance Ord Modifier where compare = comparing describe

_Alt, _Control, _Shift :: Modifier
_Alt     = Modifier $ Set.singleton Gdk.Alt
_Control = Modifier $ Set.singleton Gdk.Control
_Shift   = Modifier $ Set.singleton Gdk.Shift

modifier :: Parser Modifier
modifier = spaces *> (mconcat <$> many elementModifier)

elementModifier :: Parser Modifier
elementModifier = choice [ string "C-" >> return _Control
                         , string "M-" >> return _Alt
                         -- , string "S-" >> return _Shift
                         ]
-- }}}

-- {{{ Key mode (à la vi)
data Mode = Normal | Insert deriving(Eq, Ord)

instance Default Mode where def = Normal
-- }}}

-- {{{ KeyMap implementation
type KeyStroke = Model.KeyStroke Modifier KeyVal

instance Describable KeyStroke where
  describe (Model.KeyStroke m k) = describe m <> describe k

keyStrokes :: Parser KeyStroke
keyStrokes = do
    spaces
    m <- fromMaybe (Modifier Set.empty) <$> optionMaybe modifier
    k <- keyVal
    return $  m .| k

-- type Binding m  = Model.Binding  Hbro.Keys.Stroke (m ())
type KeyMap m = Model.KeyMap KeyStroke (m ())
-- }}}

data KeyPressed = KeyPressed deriving(Show)
instance Event KeyPressed where
  type Input KeyPressed = KeyStroke
  describeInput _ stroke = Just $ "Key pressed: " <> describe stroke

data KeyMapPressed = KeyMapPressed deriving(Show)
instance Event KeyMapPressed where
  type Input KeyMapPressed = ([KeyStroke], Bool)
  describeInput _ (strokes, _bound) = Just $ "Key pressed: " <> unwords (describe <$> strokes)


bindKeys :: (ControlIO m, MonadLogger m, MonadCatch m, MonadResource m)
         => Signal KeyPressed -> Signal KeyMapPressed -> KeyMap m -> m ReleaseKey
bindKeys input output keyMap = addRecursiveHandler input empty $ \previousStrokes newStroke -> do
    let k = Map.keys keyMap
        strokes = previousStrokes |: newStroke
        strokesL = NonEmpty.toList strokes
        found = Map.lookup strokes keyMap
        reset = isJust found || all (not . NonEmpty.isPrefixOf strokesL) k

    debug $ "Accumulated: " <> unwords (map describe strokesL)
    emit output (strokesL, isJust found)

    async . logErrors $ fromMaybe doNothing found
    return $ if reset then empty else strokesL
