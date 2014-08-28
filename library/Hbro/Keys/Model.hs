{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
-- | Key bindings model.
-- Designed to be imported as @qualified@.
module Hbro.Keys.Model (
-- * Key strokes model
    KeyStroke(..),
    modifiersL,
    keyL,
    (.|),
-- * Bindings model
    Binding,
    Bindings,
    Tree(..),
    ModalBindings,
    (-->),
    insert,
    merge,
    Hbro.Keys.Model.lookup,
-- * State machine
    Status(Status),
    modeL,
    keyStrokesL,
    bindingsL,
    bind,
    bind',
    setMode,
    press,
    getBoundFunction,
    resetStrokes,
) where

-- {{{ Imports
import           Hbro.Prelude        hiding (lookup)

import           Control.Lens        ()
import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter
import           Control.Lens.TH

import           Data.List.NonEmpty  (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty  as NE (fromList, toList)
import qualified Data.Map            as M

import qualified Prelude             (lookup)
-- }}}

-- {{{ Key strokes model
-- | A single keystroke is a (possibly empty) set of (unorderered and unique) modifiers, and a single key.
data KeyStroke modifier key = KeyStroke
    { _modifiers :: Set modifier
    , _key       :: key
    } deriving (Eq, Ord)

makeLensesWith ?? ''KeyStroke $ lensRules
    & lensField .~ (\name -> Just (tailSafe name ++ "L"))

(.|) :: (ToSet modifier m) => m -> key -> KeyStroke modifier key
(.|) m k = KeyStroke (toSet m) k
-- }}}

-- {{{ Binding model
-- | A binding is simply a (non-empty) list of strokes bound to an action
type Binding keystroke action = (NonEmpty keystroke, action)

-- | A non-empty tree implementation that labels edges and leaves
type BranchedTree b l = [(b, Tree b l)]
data Tree b l         = Leaf l | Branch (NonEmpty (b, Tree b l))

-- | Bindings are stored together in a tree structure
type Bindings keystroke action = BranchedTree keystroke action

-- | Modal bindings (à la vi)
type ModalBindings mode keystroke action = Map mode (Bindings keystroke action)


(-->) :: (ToNonEmpty keystroke s) => s -> action -> Binding keystroke action
(-->) s a = (toNonEmpty s, a)


-- | Make a branch out of a single binding
toBranch :: Ord a => (NonEmpty a, b) -> BranchedTree a b
toBranch  ((h:|[]),   a) = [ (h, Leaf a) ]
toBranch  ((h:|h':t), a) = [ (h, Branch $ toBranch' ((h':|t), a)) ]

toBranch' :: (NonEmpty b, l) -> NonEmpty (b, Tree b l)
toBranch' ((h:|[]),   a) = (h, Leaf a) :| []
toBranch' ((h:|h':t), a) = (h, Branch $ toBranch' ((h':|t), a)) :| []

-- | Merge 2 trees. In case of conflicts, prefer the rightmost operand.
merge :: Ord a => BranchedTree a b -> BranchedTree a b -> BranchedTree a b
merge [] []         = []
merge []  b         = b
merge a  []         = a
merge (h:t) (h':t') = NE.toList $ merge' (h:|t) (h':|t')

merge' :: (Ord k, Ord a)
       => NonEmpty (k, Tree a b) -> NonEmpty (k, Tree a b) -> NonEmpty (k, Tree a b)
merge' a b = NE.fromList . M.assocs $ M.unionWith merge'' (M.fromList $ NE.toList a) (M.fromList $ NE.toList b)

merge'' :: Ord a => Tree a b -> Tree a b -> Tree a b
merge'' (Leaf _)   (Leaf b)   = Leaf b
merge'' (Leaf _)   (Branch b) = Branch b
merge'' (Branch _) (Leaf b)   = Leaf b
merge'' (Branch a) (Branch b) = Branch $ merge' a b

-- | Is there a leaf at the end of the given path ?
lookup :: Ord a => NonEmpty a -> BranchedTree a b -> Maybe b
lookup a b = case walk a b of
    Just (Leaf x) -> Just x
    _             -> Nothing

-- | Return the subtree rooted at the end of the given path
walk :: Ord a => NonEmpty a -> BranchedTree a b -> Maybe (Tree a b)
walk _       []  = Nothing
walk (h:|t) tree = Prelude.lookup h tree >>= walk' t

walk' :: (Ord a) => [a] -> Tree a t -> Maybe (Tree a t)
walk' []    tree       = Just tree
walk' (h:t) (Branch b) = walk (h:|t) (NE.toList b)
walk' _     _          = Nothing

-- | Insert new binding into a tree-structured bindings list
insert :: (Ord m, Ord s) => Binding s a -> m -> ModalBindings m s a -> ModalBindings m s a
insert binding theMode = M.insertWith (flip merge) theMode (toBranch binding)
-- }}}

-- | Global state including any necessary information to handle key bindings
data Status keystroke mode action = Status
    { _mode       :: mode                                -- ^ Current mode
    , _keyStrokes :: [keystroke]                         -- ^ Previous keystrokes
    , _bindings   :: ModalBindings mode keystroke action -- ^ Current bindings
    }

makeLensesWith ?? ''Status $ lensRules
    & lensField .~ (\name -> Just (tail name ++ "L"))

instance (Default mode) => Default (Status keystroke mode action) where
    def = Status def [] M.empty


bind' :: (Ord mode, Ord keystroke, ToNonEmpty keystroke s)
      => mode -> s -> action -> Status keystroke mode action -> Status keystroke mode action
bind' theMode theStrokes action (Status a b c) = Status a b $ insert (theStrokes --> action) theMode c

-- | Same as 'bind' with default mode.
bind :: (Ord mode, Default mode, Ord keystroke, ToNonEmpty keystroke s)
      => s -> action -> Status keystroke mode action -> Status keystroke mode action
bind s f = bind' def s f

setMode :: mode -> Status keystroke mode action -> Status keystroke mode action
setMode newMode = set modeL newMode . set keyStrokesL []

press :: (Ord mode, Ord keystroke) => keystroke -> Status keystroke mode action -> Status keystroke mode action
press keystroke status = set keyStrokesL newKeyStrokes status
  where
    oldKeyStrokes   = status^.keyStrokesL
    bindings        = M.lookup (status^.modeL) (status^.bindingsL)
    validKeyStrokes = isJust . join $ walk   <$> nonEmpty oldKeyStrokes <*> bindings
    boundKeyStroke  = isJust . join $ lookup <$> nonEmpty oldKeyStrokes <*> bindings
    newKeyStrokes   = (oldKeyStrokes ++ [keystroke]) <| (validKeyStrokes && not boundKeyStroke) |> [keystroke]

getBoundFunction :: (Ord mode, Ord keystroke) => Status keystroke mode action -> Maybe action
getBoundFunction status = do
    theStrokes  <- nonEmpty $ status^.keyStrokesL
    theBindings <- M.lookup (status^.modeL) (status^.bindingsL)
    lookup theStrokes theBindings

resetStrokes :: Status keystroke mode action -> Status keystroke mode action
resetStrokes = set keyStrokesL []
