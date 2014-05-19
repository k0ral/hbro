module Hbro.Keys.Signals where

-- {{{ Imports
import Hbro.Error
-- import Hbro.Gdk.KeyVal
import Hbro.Keys as Keys
import Hbro.Keys.Model hiding(KeyStroke)
import Hbro.Util

import Control.Concurrent.STM
import Control.Lens.Getter
import Control.Monad.Reader hiding(forM_)

import Data.Foldable
-- }}}

data KeyPressed = KeyPressed KeyStroke


dequeue :: (MonadBase IO m, MonadCatch m, MonadThrow m) => a -> TMVar KeyPressed -> Keys.Hooks (ReaderT a m) -> m ()
dequeue globalContext signal hooks = forever $ do
    -- io $ debugM "hbro.hooks" "Listening for key-pressed signal..."
    (KeyPressed stroke) <- io . atomically $ takeTMVar signal
    theStatus           <- io . atomically $ readTVar (hooks^.statusL)

    let newStatus    = press stroke theStatus
    let newChain     = newStatus^.keyStrokesL
    let f            = getBoundFunction newStatus

    io . debugM "hbro.hooks" $ "Key pressed signal acknowledged: " ++ show newChain

    g <- io . atomically . tryReadTMVar $ hooks^.onKeyPressedL
    forM_ g $ \g' -> do
        io $ debugM "hbro.hooks" "Global callback defined, executing it."
        logErrors_ . (`runReaderT` globalContext) $ g' newChain

    forM_ f $ \f' -> do
        io $ debugM "hbro.hooks" "Key is bound, executing callback..."
        logErrors_ $ (`runReaderT` globalContext) f'

    io . atomically . modifyTVar (hooks^.statusL) $ const newStatus
