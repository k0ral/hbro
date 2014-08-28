module Hbro.Gui.Buildable where

-- {{{ Imports
import           Hbro.Prelude

import           Graphics.UI.Gtk.Builder
-- }}}

-- | UI elements that can be built from a @GtkBuilder@ object (that is: an XML file)
class Buildable a where buildWith :: (BaseIO m) => Builder -> m a
