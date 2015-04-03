{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Extension of @System.Glib.Attributes@ module.
module System.Glib.Attributes.Extended where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           Graphics.UI.Gtk.General.General.Extended

import           System.Glib.Attributes                   (Attr, AttrOp (..),
                                                           ReadWriteAttr)
import qualified System.Glib.Attributes                   as Glib
-- }}}

-- * Wrappers
get :: (MonadIO m) => o -> ReadWriteAttr o a b -> m a
get o a = gSync $ Glib.get o a

set :: (MonadIO m, MonadLogger m, Show a) => o -> Attr o a -> a -> m o
set object attribute newValue = do
  info $ "Set " ++ tshow attribute ++ " = " ++ tshow newValue
  gAsync $ Glib.set object [attribute := newValue]
  return object

set_ :: (MonadIO m, MonadLogger m, Functor m, Show a) => o -> Attr o a -> a -> m ()
set_ o a v = void $ set o a v

-- * Utils
modify :: (MonadIO m, MonadLogger m, Show a) => o -> Attr o a -> (a -> a) -> m a
modify object attribute f = do
  oldValue <- get object attribute
  set object attribute $ f oldValue
  return oldValue

-- | Same as 'modify', but discards the result.
modify_ :: (MonadIO m, MonadLogger m, Functor m, Show a) => o -> Attr o a -> (a -> a) -> m ()
modify_ object e = void . modify object e

toggle :: (MonadIO m, MonadLogger m) => o -> Attr o Bool -> m Bool
toggle object x = modify object x not

-- | Same as 'toggle', but discards the result.
toggle_ :: (MonadIO m, MonadLogger m, Functor m) => o -> Attr o Bool -> m ()
toggle_ object x = modify_ object x not
