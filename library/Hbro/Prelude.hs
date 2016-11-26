{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
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
    , show
    , (|:)
    , (>:)
    , io
    , (>/>)
    , abort
    , doNothing
-- * Lens util
    , lensGen
    , fwd
) where

-- {{{ Imports
import           Control.Applicative           as X (Alternative (..),
                                                     WrappedMonad, optional)
import           Control.Arrow                 as X (Kleisli (..), left, right)
import           Control.Conditional           as X (ToBool (..))
import           Control.Monad                 as X (MonadPlus (..), forM_,
                                                     forM_, guard, join, unless,
                                                     void, when, (<=<), (>=>))
import           Control.Monad.Base            as X (MonadBase (..))
import           Control.Monad.IO.Class        as X (MonadIO (..))
import           Control.Monad.Reader.Extended as X hiding (get)
import           Control.Monad.Trans.Control   as X

import           Data.ByteString               as X (ByteString)
import           Data.Default.Class            as X
import           Data.Foldable                 as X (asum)
import           Data.List                     as X (tail)
import           Data.List.NonEmpty            hiding (reverse)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Map                      as X (Map)
import           Data.Maybe                    as X (fromJust, fromMaybe,
                                                     isJust)
import           Data.Monoid                   as X
import           Data.MonoTraversable          as X
import           Data.Ord                      as X (comparing)
import           Data.Sequences                as X
import           Data.String                   as X (IsString (..))
import           Data.Text                     as X (Text)

import qualified GHC.Show                      as Show

import           Language.Haskell.TH.Syntax

import           Lens.Micro.Platform

import           Prelude                       as X hiding (break, drop,
                                                     dropWhile, error, filter,
                                                     lines, readFile, replicate,
                                                     reverse, show, span,
                                                     splitAt, take, takeWhile,
                                                     unlines, unwords, words,
                                                     writeFile)

import           Safe                          as X (initSafe, tailSafe)
import           System.FilePath
-- }}}

-- | Like 'Show', for 'Text'
class Describable a where describe :: a -> Text
instance Describable () where describe = const "()"

-- | Mix of @MonadBase IO@ and @MonadIO@
type BaseIO m = (MonadBase IO m, MonadIO m)

-- | Mix of @MonadBaseControl IO@ and @MonadIO@
type ControlIO m = (MonadBaseControl IO m, MonadIO m)

-- {{{ Generic aliases/shortcuts
-- | Generic 'Show.show'
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

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

-- {{{ Lens util
-- | Lens field generator
lensGen _ _ n = case nameBase n of
  '_':x -> [TopName (mkName $ x ++ "_")]
  _     -> []

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
