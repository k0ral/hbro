module Hbro.Gui.Buildable where

-- {{{ Imports
import Hbro.Util

import Graphics.UI.Gtk.Builder
-- }}}

-- | UI elements that can be built from a @GtkBuilder@ object (that is: an XML file)
class Buildable a where buildWith :: (MonadBase IO m) => Builder -> m a
