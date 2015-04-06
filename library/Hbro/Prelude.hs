{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
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
    , io
    , (>/>)
    , abort
    , doNothing
-- * File manipulation
    , readFileE
    , readFileE'
    , writeFileE
    , writeFileE'
-- * Lens util
    , withM
    , withM_
    , fwd
) where

-- {{{ Imports
import           ClassyPrelude                 as X hiding (Builder (..),
                                                     MonadReader (..),
                                                     ReaderT (..),
                                                     defaultTimeLocale, error,
                                                     log, toList)

import           Control.Applicative           as X (Alternative (..),
                                                     WrappedMonad, optional)
import           Control.Arrow                 as X (Kleisli (..), left, right)
import           Control.Conditional           as X (ToBool (..), (<<|), (<|),
                                                     (|>), (|>>))
import           Control.Lens
import           Control.Monad.Base            as X (MonadBase (..))
import           Control.Monad.Except
import           Control.Monad.Reader.Extended as X hiding (get)
import           Control.Monad.Trans.Control   as X

import qualified Data.ByteString               as Strict
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Default.Class            as X
import           Data.Foldable                 as X (asum)
import           Data.List                     as X (tail)
import           Data.List.NonEmpty            hiding (reverse)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe                    as X (fromJust)

import           Safe                          as X (initSafe, tailSafe)

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

-- | Infix equivalent for @(,)@
(>:) :: a -> b -> (a, b)
(>:) = (,)
infix 0 >:

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

-- {{{ File manipulation
-- | Lifted version of 'readFile'
readFileE :: (ControlIO m, MonadError Text m) => FilePath -> m Lazy.ByteString
readFileE = handleIO (throwError . tshow) . readFile

-- | Strict version of 'readFileE'
readFileE' :: (ControlIO m, MonadError Text m) => FilePath -> m Strict.ByteString
readFileE' = handleIO (throwError . tshow) . readFile

-- | Lifted version of 'writeFile'
writeFileE :: (ControlIO m, MonadError Text m) => FilePath -> Lazy.ByteString -> m ()
writeFileE f x = handleIO (throwError . tshow) $ writeFile f x

-- | Strict version of 'writeFileE'
writeFileE' :: (ControlIO m, MonadError Text m) => FilePath -> Strict.ByteString -> m ()
writeFileE' f x = handleIO (throwError . tshow) $ writeFile f x
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

-- {{{ Process management
-- Return the list of process IDs corresponding to all running instances of the browser.
-- getAllProcessIDs :: MonadIO m => m [ProcessID]
-- getAllProcessIDs = do
--     (_, pids, _)  <- io $ readProcessWithExitCode "pidof" ["hbro"] []
--     (_, pids', _) <- io $ readProcessWithExitCode "pidof" ["hbro-" ++ Sys.os ++ "-" ++ Sys.arch] []
--     myPid         <- io $ getProcessID

--     return $ delete myPid . map (read :: String -> ProcessID) . nub . words $ pids ++ " " ++ pids'
-- }}}
