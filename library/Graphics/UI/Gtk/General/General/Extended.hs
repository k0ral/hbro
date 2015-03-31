{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.UI.Gtk.General.General.Extended
    ( module X
    , module Graphics.UI.Gtk.General.General.Extended
    ) where

-- {{{ Imports
import           Hbro.Prelude

import           Graphics.UI.Gtk.General.General as X
-- }}}

-- | Lifted alias for 'postGUISync'
gSync :: MonadIO m => IO a -> m a
gSync  = io . postGUISync

-- | Lifted alias for 'postGUIAsync'
gAsync :: MonadIO m => IO a -> m ()
gAsync = io . postGUIAsync . void
