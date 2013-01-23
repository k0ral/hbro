{-# LANGUAGE FlexibleContexts #-}
module Hbro.Gtk.ScrolledWindow where

-- {{{ Imports
import Hbro.Util

import Control.Monad.Base

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
-- }}}

data Axis     = Horizontal | Vertical
data Position = Absolute Double | Relative Double


getAdjustment :: (MonadBase IO m) => Axis -> ScrolledWindow -> m Adjustment
getAdjustment Horizontal = io . scrolledWindowGetHAdjustment
getAdjustment Vertical   = io . scrolledWindowGetVAdjustment


-- | General scrolling command.
scroll :: (MonadBase IO m) => Axis -> Position -> ScrolledWindow -> m ()
scroll axis percentage scrollWindow = io $ do
     adj     <- io . getAdjustment axis $ scrollWindow
     page    <- io $ adjustmentGetPageSize adj
     current <- io $ adjustmentGetValue adj
     lower   <- io $ adjustmentGetLower adj
     upper   <- io $ adjustmentGetUpper adj

     let shift (Absolute x) = lower   + x/100 * (upper - page - lower)
         shift (Relative x) = current + x/100 * page
         limit x            = (x `max` lower) `min` (upper - page)

     io $ adjustmentSetValue adj $ limit (shift percentage)
