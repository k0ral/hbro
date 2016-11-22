{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module System.Process.Extended where

-- {{{ Imports
import           Hbro.Logger
import           Hbro.Prelude

import           System.Process
-- }}}

-- | Run external command and don't die when parent process exits.
spawn :: (MonadLogger m, MonadIO m) => Text -> [Text] -> m ()
spawn (unpack -> command) (map unpack -> options) = do
    debug $ "Executing command: " <> pack (showCommandForUser command options)
    liftIO . void $ createProcess (proc command options) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, close_fds = True }
