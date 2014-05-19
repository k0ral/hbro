-- | Various useful functions. This module doesn't import any other Hbro.* module, so it can be safely imported from anywhere.
module Hbro.Util
    ( module X
-- * Typeclass
    , ToList(..)
    , ToSet(..)
    , ToNonEmpty(..)
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
    , askl
-- * Concurrent util
    , writeTMVar
    , withAsyncList
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
import Control.Applicative as X (WrappedMonad, (<*>), pure)
import Control.Arrow as X (Kleisli(..), left, right)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Conditional as X hiding((??), unless)
import Control.Lens
import Control.Monad as X (MonadPlus(..), unless)
import Control.Monad.Base as X (MonadBase(..))
import Control.Monad.Reader

import Data.Char as X
import Data.Default as X
import Data.Either as X
import Data.Foldable as X (mapM_)
import Data.Functor as X
import Data.List as X hiding(foldl, init, insert, sum)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe as X
import Data.Set (Set)
import Data.Typeable as X hiding(cast)

import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk.General.General

import Prelude hiding(log, mapM_)

import Safe as X (tailSafe)

import System.FilePath
-- import qualified System.Info as Sys
import System.Log.Logger as X
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

-- {{{ Generic aliases/shortcuts
-- 'left' for 'Kleisli' arrows
leftM :: Monad m => (a -> m b) -> Either a c -> m (Either b c)
leftM f = runKleisli (left $ Kleisli f)

-- | Alias for 'liftBase' in @IO@
io :: MonadBase IO m => IO a -> m a
io = liftBase

-- | Like '(\</\>)' with first argument in IO to build platform-dependent paths.
(>/>) :: (MonadBase IO m) => IO FilePath -> FilePath -> m FilePath
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

askl ::(MonadReader t m) => Lens' t a -> m a
askl l = asks $ view l
-- }}}

-- {{{ Concurrent util
-- | This is a combination of 'tryTakeTMVar' and 'putTMVar';
-- ie. it empties the 'TMVar' if needed, and puts the new value instead.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var val = do
    tryTakeTMVar var
    putTMVar var val

-- | Recursive 'withAsync'
withAsyncList :: [IO a] -> ([Async a] -> IO b) -> IO b
withAsyncList a f = withAsyncList' a [] f

withAsyncList' :: [IO a] -> [Async a] -> ([Async a] -> IO b) -> IO b
withAsyncList' [] x f  = f x
withAsyncList' (a:b) x f = withAsync a $ \x' -> withAsyncList' b (x':x) f
-- }}}

-- {{{ Gtk util
-- | Lifted alias for 'postGUISync'
gSync :: (MonadBase IO m) => IO a -> m a
gSync  = io . postGUISync

-- | Lifted alias for 'postGUIAsync'
gAsync :: (MonadBase IO m) => IO () -> m ()
gAsync = io . postGUIAsync
-- }}}

-- {{{ Process management
-- | Run external command and don't die when parent process exits.
spawn :: (MonadBase IO m) => String -> [String] -> m ()
spawn command options = io . void $ createProcess (proc command options) { std_in = CreatePipe,  std_out = CreatePipe, std_err = CreatePipe, close_fds = True }

-- Return the list of process IDs corresponding to all running instances of the browser.
-- getAllProcessIDs :: MonadBase IO m => m [ProcessID]
-- getAllProcessIDs = do
--     (_, pids, _)  <- io $ readProcessWithExitCode "pidof" ["hbro"] []
--     (_, pids', _) <- io $ readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
--     myPid         <- io $ getProcessID

--     return $ delete myPid . map (read :: String -> ProcessID) . nub . words $ pids ++ " " ++ pids'
-- }}}


{-errorHandler :: (MonadBase IO m, MonadReader r m, HasOptions r) => FilePath -> IOError -> m ()
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
