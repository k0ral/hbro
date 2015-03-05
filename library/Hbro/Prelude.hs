{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Replacement for the traditional @Prelude@ module. This module doesn't import any other Hbro.* module, so it can be safely imported from anywhere in the project.
module Hbro.Prelude
    ( module X
-- * Typeclass
    , Describable(..)
    , BaseIO
    , ControlIO
-- * Generic aliases/shortcuts
    , (|:)
    , (>:)
    , leftM
    , io
    , (>/>)
    , abort
    , doNothing
-- * Lens util
    , withM
    , withM_
    , fwd
-- * Gtk util
    , gSync
    , gAsync
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
import           ClassyPrelude                   as X hiding (Builder (..),
                                                       MonadReader (..),
                                                       ReaderT (..), error, log,
                                                       toList)

import           Control.Applicative             as X (Alternative (..),
                                                       WrappedMonad, optional)
import           Control.Arrow                   as X (Kleisli (..), left,
                                                       right)
import           Control.Conditional             as X (ToBool (..), (<<|), (<|),
                                                       (|>), (|>>))
import           Control.Lens
import           Control.Monad.Base              as X (MonadBase (..))
import           Control.Monad.Reader.Extended   as X hiding (get)
import           Control.Monad.Trans.Control     as X

import           Data.Default.Class              as X
import           Data.Foldable                   as X (asum)
import           Data.Functor                    as X
import           Data.List                       as X (tail)
import           Data.List.NonEmpty              hiding (reverse)
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      as X (fromJust)

import           Graphics.Rendering.Pango.Enums
import           Graphics.UI.Gtk.General.General

import           Safe                            as X (initSafe, tailSafe)

-- import System.Posix.Process
-- import System.Posix.Types
-- }}}


-- | Like 'Show', for 'Text'
class Describable a where describe :: a -> Text
instance Describable () where describe = const "()"

-- | Mix of @MonadBase IO@ and @MonadIO@
type BaseIO m = (MonadBase IO m, MonadIO m)

-- | Mix of @MonadBaseControl IO@ and @MonadIO@
type ControlIO m = (MonadBaseControl IO m, MonadIO m)

-- {{{ Generic aliases/shortcuts
-- | Build a 'NonEmpty' from the right
(|:) :: [a] -> a -> NonEmpty a
list |: e = NonEmpty.reverse (e :| reverse list)

-- | Infix operator to build couples
(>:) :: a -> b -> (a, b)
(>:) = (,)
infix 0 >:

-- | 'left' for 'Kleisli' arrows
leftM :: Monad m => (a -> m b) -> Either a c -> m (Either b c)
leftM f = runKleisli (left $ Kleisli f)

-- | Alias for 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Like '(\</\>)' with first argument in @IO@ to build platform-dependent paths.
(>/>) :: MonadIO m => IO FilePath -> FilePath -> m FilePath
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
-- }}}

-- {{{ Gtk util
-- | Lifted alias for 'postGUISync'
gSync :: MonadIO m => IO a -> m a
gSync  = io . postGUISync

-- | Lifted alias for 'postGUIAsync'
gAsync :: MonadIO m => IO a -> m ()
gAsync = io . postGUIAsync . void
-- }}}

-- {{{ Process management
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
