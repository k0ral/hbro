module Graphics.Rendering.Pango.Extended (module X, module Graphics.Rendering.Pango.Extended) where

import           Graphics.Rendering.Pango.Enums as X


allItalic, allBold :: PangoAttribute
allItalic = AttrStyle  {paStart = 0, paEnd = -1, paStyle  = StyleItalic}
allBold   = AttrWeight {paStart = 0, paEnd = -1, paWeight = WeightBold}

black, gray, red, green, blue, yellow :: Color
black  = Color     0     0     0
gray   = Color 32767 32767 32767
red    = Color 65535     0     0
green  = Color     0 65535     0
blue   = Color     0     0 65535
yellow = Color 65535 65535     0
