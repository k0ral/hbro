module Hbro.Gtk.ScrolledWindow where

-- {{{ Imports
import Hbro.Util
import Hbro.Types

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
-- }}}

getAdjustment :: (MonadIO m) => Axis -> ScrolledWindow -> m Adjustment
getAdjustment Horizontal = io . scrolledWindowGetHAdjustment
getAdjustment Vertical   = io . scrolledWindowGetVAdjustment


-- | General scrolling command.
scroll' :: (MonadIO m) => Axis -> Position -> ScrolledWindow -> m ()
scroll' axis percentage scrollWindow = io $ do
     adj     <- io . getAdjustment axis $ scrollWindow
     page    <- io $ adjustmentGetPageSize adj
     current <- io $ adjustmentGetValue adj
     lower   <- io $ adjustmentGetLower adj
     upper   <- io $ adjustmentGetUpper adj

     let shift (Absolute x) = lower   + x/100 * (upper - page - lower)
         shift (Relative x) = current + x/100 * page
         limit x            = (x `max` lower) `min` (upper - page)

     io $ adjustmentSetValue adj $ limit (shift percentage)

scroll :: (MonadIO m, MonadReader r m, HasScrollWindow r) => Axis -> Position -> m ()
scroll axis percentage = scroll' axis percentage =<< asks _scrollwindow
