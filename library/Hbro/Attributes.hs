{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Convenient wrappers around Glib's attribute system.
module Hbro.Attributes where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           System.Glib.Attributes (Attr, AttrOp (..))
import qualified System.Glib.Attributes as Glib
-- }}}

get :: (MonadIO m) => o -> Attr o a -> m a
get o a = gSync $ Glib.get o a

set :: (MonadIO m, Show a) => o -> Attr o a -> a -> m o
set object attribute newValue = do
  gAsync $ Glib.set object [attribute := newValue]
  return object

set_ :: (MonadIO m, Functor m, Show a) => o -> Attr o a -> a -> m ()
set_ o a v = void $ set o a v

modify :: (MonadIO m, Show a) => o -> Attr o a -> (a -> a) -> m a
modify object attribute f = do
    oldValue <- gSync $ Glib.get object attribute

    set object attribute $ f oldValue
    infoM $ "Set " ++ tshow attribute ++ " = " ++ tshow (f oldValue)
    return oldValue

-- | Same as 'modify', but discards the result
modify_ :: (MonadIO m, Functor m, Show a) => o -> Attr o a -> (a -> a) -> m ()
modify_ object e = void . modify object e

toggle :: (MonadIO m) => o -> Attr o Bool -> m Bool
toggle object x = modify object x not

-- | Same as 'toggle', but discards the result
toggle_ :: (MonadIO m, Functor m) => o -> Attr o Bool -> m ()
toggle_ object x = modify_ object x not
