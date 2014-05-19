{-# LANGUAGE TemplateHaskell #-}
module Hbro.Signals (
-- * Signals
      Signals
    , _ipcSignals
    , _promptSignals
    , _webViewSignals
    , initialize
    , attachGuiSignals
) where

-- {{{ Imports
-- import Hbro.Error
import Hbro.Gui hiding(initialize)
import Hbro.Gui.PromptBar (entryL)
import qualified Hbro.IPC.Signals as IPC
import qualified Hbro.Gui.PromptBar.Signals as Prompt
import Hbro.Util
import qualified Hbro.WebView.Signals as WebView

import Control.Lens hiding((??))
-- }}}

-- {{{ Signals
data Signals = Signals {
    __ipcSignals     :: IPC.Signals,
    __promptSignals  :: Prompt.Signals,
    __webViewSignals :: WebView.Signals }

makeLenses ''Signals

initialize :: (MonadBase IO m) => m Signals
initialize = Signals <$> IPC.initSignals
                         <*> Prompt.initSignals
                         <*> WebView.initSignals

attachGuiSignals :: (MonadBase IO m) => GUI -> Signals -> m ()
attachGuiSignals gui signals = do
    WebView.attach    (gui^.webViewL)          (signals^._webViewSignals)
    Prompt.attach     (gui^.promptBarL.entryL) (signals^._promptSignals)
-- }}}
