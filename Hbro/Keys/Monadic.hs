module Hbro.Keys.Monadic where

-- {{{ Imports
import Hbro.Keys
import qualified Hbro.Keys.Model as Keys
import Hbro.Util hiding(Control, lookup)

import Control.Concurrent.STM
import Control.Lens.Type
-- import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Reader

-- import Data.List.NonEmpty as NE
-- import Data.Map as M
-- import Data.Set (Set)

-- import Prelude hiding(foldl, mapM_)
-- }}}


modify :: (MonadReader r m, MonadBase IO m, HasHooks n r) => Lens' (Hooks n) (TVar a) -> (a -> a) -> m ()
modify l f = io . atomically . (`modifyTVar` f) =<< askl (_hooks.l)

bind' :: (HasHooks n r, ToNonEmpty KeyStroke s, MonadBase IO m, MonadReader r m)
      => Mode -> s -> n () -> m ()
bind' mode strokes action = modify statusL $ Keys.bind' mode strokes action

bind :: (HasHooks n r, ToNonEmpty KeyStroke s, MonadBase IO m, MonadReader r m)
     => s -> n () -> m ()
bind strokes action = modify statusL $ Keys.bind strokes action

-- setMode :: mode -> Status stroke mode action -> Status stroke mode action
-- setMode newMode = set _mode newMode . set _strokes []

-- press :: (Ord mode, Ord stroke) => stroke -> Status stroke mode action -> Status stroke mode action
-- press stroke status = set _strokes newStrokes status
--   where
--     newStrokes     = maybe [stroke] (const chainedStrokes) $ walk (NE.fromList chainedStrokes) =<< bindings
--     chainedStrokes = status^._strokes ++ [stroke]
--     bindings       = M.lookup (status^._mode) (status^._bindings)

-- getBoundFunction :: (Ord mode, Ord stroke) => Status stroke mode action -> Maybe action
-- getBoundFunction status = do
--     theStrokes  <- nonEmpty $ status^._strokes
--     theBindings <- M.lookup (status^._mode) (status^._bindings)
--     lookup theStrokes theBindings

-- resetStrokes :: Status stroke mode action -> Status stroke mode action
-- resetStrokes = set _strokes []
