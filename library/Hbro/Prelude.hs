{-# LANGUAGE ConstraintKinds #-}
-- | Replacement for the traditional @Prelude@ module. This module doesn't import any other Hbro.* module, so it can be safely imported from anywhere in the project.
module Hbro.Prelude
    ( module X
-- * Typeclass
    , ToList(..)
    , ToSet(..)
    , ToNonEmpty(..)
    , Describable(..)
    , BaseIO
    , ControlIO
-- * Generic aliases/shortcuts
    , leftM
    , io
    , (>/>)
    , abort
    , doNothing
-- * Lens util
    , withM
    , withM_
    , fwd
    , askL
-- * Concurrent util
    , writeTMVar
    , withAsyncList
    , withAsyncList_
-- * Gtk util
    , gSync
    , gAsync
-- * Process control
    , spawn
-- * Pango util
    , allItalic
    , allBold
    , black
    , gray
    , red
    , green
    , blue
    , yellow
) where

-- {{{ Imports
import ClassyPrelude as X hiding(Builder(..), log, toList)

import Control.Applicative as X (Alternative(..), WrappedMonad)
import Control.Arrow as X (Kleisli(..), left, right)
import Control.Concurrent.Async.Lifted
import Control.Conditional as X (ToBool(..), (<|), (|>), (<<|), (|>>))
import Control.Lens
import Control.Monad.Base as X (MonadBase(..))
import Control.Monad.Trans.Control as X
import Control.Monad.Reader

import Data.Default as X
import Data.Foldable as X (asum)
import Data.Functor as X
import Data.List as X (tail)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe as X (fromJust)

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.General.General

import Safe as X (initSafe, tailSafe)

import System.Log.Logger
-- import System.Posix.Process
-- import System.Posix.Types
import System.Process
-- }}}

-- | We often want to use a single object where a list is expected.
class ToList m t | t -> m where toList :: t -> [m]

instance ToList m [m] where toList = id

-- | We often want to use a single object where a set is expected.
class ToSet m t | t -> m where toSet :: t -> Set m

instance ToSet m (Set m) where toSet = id

-- | We often want to use a single object where a non-empty list is expected.
class ToNonEmpty s t | t -> s where toNonEmpty :: t -> NonEmpty s

instance ToNonEmpty s (NonEmpty s) where toNonEmpty = id

-- | Like 'Show', for 'Text'
class Describable a where describe :: a -> Text

-- | Mix of 'MonadBase IO' and 'MonadIO'
type BaseIO m = (MonadBase IO m, MonadIO m)

-- | Mix of 'MonadBaseControl IO' and 'MonadIO'
type ControlIO m = (MonadBaseControl IO m, MonadIO m)

-- {{{ Generic aliases/shortcuts
-- 'left' for 'Kleisli' arrows
leftM :: Monad m => (a -> m b) -> Either a c -> m (Either b c)
leftM f = runKleisli (left $ Kleisli f)

-- | Alias for 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Like '(\</\>)' with first argument in @IO@ to build platform-dependent paths.
(>/>) :: (BaseIO m) => IO FilePath -> FilePath -> m FilePath
(>/>) a b = io $ (</> b) <$> a

-- | Alias for 'mzero'
abort :: MonadPlus m => m a
abort = mzero

-- | Alias for @return ()@
doNothing :: Monad m => m ()
doNothing = return ()
-- }}}

-- {{{ Lens util
-- | Alias for 'mapMOf'
withM :: Profunctor p => Over p (WrappedMonad m) s t a b -> p a (m b) -> s -> m t
withM = mapMOf

withM_ :: Monad m => Over (->) (WrappedMonad m) s t a a -> (a -> m ()) -> s -> m t
withM_ l f = mapMOf l (fwd f)

fwd :: (Monad m) => (a -> m ()) -> a -> m a
fwd f x = f x >> return x

-- | Call 'asks' with the given lens getter.
askL ::(MonadReader t m) => Lens' t a -> m a
askL l = asks $ view l
-- }}}

-- {{{ Concurrent util
-- | This is a combination of 'tryTakeTMVar' and 'putTMVar';
-- ie. it empties the 'TMVar' if needed, and puts the new value instead.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var val = do
    tryTakeTMVar var
    putTMVar var val

-- | Recursive 'withAsync'
withAsyncList :: (ControlIO m) => [m a] -> ([Async (StM m a)] -> m b) -> m b
withAsyncList a f = withAsyncList' a [] f

-- | Same as 'withAsyncList', but discards the result
withAsyncList_ :: (ControlIO m) => [m a] -> ([Async (StM m a)] -> m b) -> m ()
withAsyncList_ a f = void $ withAsyncList a f

withAsyncList' :: (ControlIO m) => [m a] -> [Async (StM m a)] -> ([Async (StM m a)] -> m b) -> m b
withAsyncList' [] x f  = f x
withAsyncList' (a:b) x f = withAsync a $ \x' -> withAsyncList' b (x':x) f
-- }}}

-- {{{ Gtk util
-- | Lifted alias for 'postGUISync'
gSync :: (BaseIO m) => IO a -> m a
gSync  = io . postGUISync

-- | Lifted alias for 'postGUIAsync'
gAsync :: (BaseIO m) => IO () -> m ()
gAsync = io . postGUIAsync
-- }}}

-- {{{ Process management
-- | Run external command and don't die when parent process exits.
spawn :: (BaseIO m) => String -> [String] -> m ()
spawn command options = io $ do
    debugM "hbro.prelude" $ "Executing command: " ++ unwords (command:options)
    void $ createProcess (proc command options) { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True }

-- Return the list of process IDs corresponding to all running instances of the browser.
-- getAllProcessIDs :: MonadIO m => m [ProcessID]
-- getAllProcessIDs = do
--     (_, pids, _)  <- io $ readProcessWithExitCode "pidof" ["hbro"] []
--     (_, pids', _) <- io $ readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
--     myPid         <- io $ getProcessID

--     return $ delete myPid . map (read :: String -> ProcessID) . nub . words $ pids ++ " " ++ pids'
-- }}}


{-errorHandler :: (MonadIO m, MonadReader r m, HasOptions r) => FilePath -> IOError -> m ()
errorHandler file e = do
  when (isAlreadyInUseError e) $ unlessQuiet . io . putStrLn $ "ERROR: file <" ++ file ++ "> is already opened and cannot be reopened."
  when (isDoesNotExistError e) $ unlessQuiet . io . putStrLn $ "ERROR: file <" ++ file ++ "> doesn't exist."
  when (isPermissionError   e) $ unlessQuiet . io . putStrLn $ "ERROR: user doesn't have permission to open file <" ++ file ++ ">."-}

-- {{{ Pango util
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
-- }}}
