{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TemplateHaskell, UndecidableInstances #-}
module Hbro.Config where

-- {{{ Imports
import qualified Hbro.Keys as Key
import Hbro.IPC
import Hbro.Util

-- import Control.Conditional
import Control.Lens hiding(set)
import Control.Monad.Base
import Control.Monad.Error  hiding(forM_, mapM_)
-- import Control.Monad.Writer hiding(forM_, mapM_)

-- import Data.Foldable (forM_, mapM_)
-- import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.WebKit.WebNavigationAction

import Network.URI as N hiding(parseURI, parseURIReference)

import Prelude hiding(mapM_)
-- }}}

-- {{{ Types
data ResourceAction  = Load | Download
data Verbosity       = Quiet | Normal | Verbose deriving(Eq, Show)

-- | Custom settings provided by the user
data Config m = Config {
-- Paths
    _socketDir        :: FilePath,                          -- ^ Directory where IPC sockets will be created (e.g. @/tmp@)
    _UIFile           :: FilePath,                          -- ^ Path to XML file describing UI (used by @GtkBuilder@)
    _homePage         :: URI,                               -- ^ Startup page
-- Parameters
    _verbosity        :: Verbosity,                         -- ^ Logs verbosity
-- Hooks
    _keyBindings      :: Map Key.Mode (Key.Bindings m),     -- ^ Key bindings
    _onDownload       :: URI -> String -> Int -> m (),      -- ^ Callback triggered when a download is requested
    _onKeyStroke      :: [Key.Stroke] -> m (),              -- ^ Callback triggered when a key is pressed
    _onLinkClicked    :: MouseButton -> URI -> m (),        -- ^ Callback triggered when a link is clicked
    _onLoadRequested  :: URI -> m (),                       -- ^ Callback triggered when a load is requested
    _onLoadFinished   :: m (),                              -- ^ Callback triggered when a load is finished
    _onNewWindow      :: URI -> m (),                       -- ^ Callback triggered when a new window is requested
    _onResourceOpened :: URI -> String -> m ResourceAction, -- ^ Callback triggered when opening a non HTML resource
    _onTitleChanged   :: String -> m (),                    -- ^ Callback triggered when document title is changed
    _commands         :: CommandsMap m                      -- ^ Commands recognized through IPC system
}

makeLenses ''Config

instance Show (Config m) where
    show c = "Socket directory = " ++ c^.socketDir
        ++ "\nUI file          = " ++ c^.uIFile
        ++ "\nHome page        = " ++ (show $ c^.homePage)
        ++ "\nVerbosity        = " ++ (show $ c^.verbosity)


-- | 'MonadReader' for 'Config'
class (Monad m) => ConfigReader n m | m -> n where
    readConfig :: Simple Lens (Config n) a -> m a

-- | 'MonadWriter' for 'Config'
class (Monad m) => ConfigWriter n m | m -> n where
    writeConfig :: Simple Lens (Config n) a -> a -> m ()

-- | 'MonadState' for 'Config'
type ConfigState n m = (ConfigReader n m, ConfigWriter n m)

modifyConfig :: (ConfigState n m) => Simple Lens (Config n) a -> (a -> a) -> m ()
modifyConfig l f = writeConfig l . f =<< readConfig l

instance Eq NavigationReason where
  a == b = (fromEnum a) == (fromEnum b)

instance Show NavigationReason where
  show WebNavigationReasonLinkClicked   = "Link clicked"
  show WebNavigationReasonFormSubmitted = "Form submitted"
  show WebNavigationReasonBackForward   = "Back/forward"
  show WebNavigationReasonReload        = "Reload"
  show WebNavigationReasonFormResubmitted = "Form resubmitted"
  show WebNavigationReasonOther         = "Other"
-- }}}


-- | Return socket URI used for the current process.
getSocketURI :: (MonadBase IO m, ConfigReader n m) => m String
getSocketURI = getSocketPath =<< readConfig socketDir

-- | Run an action unless verbosity is 'Quiet'
unlessQuiet :: (MonadBase IO m, ConfigReader n m) => m () -> m ()
unlessQuiet f = do
    quiet' <- readConfig verbosity
    case quiet' of
        Quiet -> return ()
        _     -> f

-- | Run an action when verbosity is 'Verbose'
whenLoud :: (MonadBase IO m, ConfigReader n m) => m () -> m ()
whenLoud f = do
    verbose' <- readConfig verbosity
    case verbose' of
        Verbose -> f
        _       -> return ()


log, logV :: (MonadBase IO m, ConfigReader n m) => String -> m ()
log  = unlessQuiet . io . putStrLn
logV = whenLoud    . io . putStrLn

-- | Bind a keystrokes chain to a callback, in a given mode
bind :: (MonadBase IO m, ConfigState m m) => Key.Mode -> String -> m () -> m ()
bind mode strokes action = case newBindings of
    Just b -> do
        oldValue <- readConfig keyBindings
        let newValue = M.insertWith Key.merge mode b oldValue
        void $ writeConfig keyBindings newValue
        return ()
    _ -> return ()
  where
    newBindings = Key.mkBinding strokes action
