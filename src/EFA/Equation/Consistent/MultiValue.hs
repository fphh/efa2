{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module EFA.Equation.Consistent.MultiValue where

import qualified EFA.Equation.Consistent.Dimension as Dim

import qualified Data.NonEmpty as NonEmpty
import Control.Applicative (Applicative, pure, (<*>), liftA2)
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))


{- |
The length of the list must match the depth of the tree.
The indices must be in strictly ascending order.
-}
data MultiValue i a =
        forall idx. (Dim.C idx) =>
           MultiValue (idx i) (Tree idx a)

data ExMultiValue idx i a = ExMultiValue (idx i) (Tree idx a)

newtype Leaf a = Leaf a
data Branch tree a = Branch (tree a) (tree a)


type family Tree (idx :: * -> *) :: * -> *
type instance Tree NonEmpty.Empty = Leaf
type instance Tree (NonEmpty.T idx) = Branch (Tree idx)


instance Functor Leaf where
   fmap f (Leaf a) = Leaf (f a)

instance Functor tree => Functor (Branch tree) where
   fmap f (Branch a0 a1) = Branch (fmap f a0) (fmap f a1)

-- | must only be applied if the index sets match
instance Applicative Leaf where
   pure = Leaf
   Leaf f <*> Leaf a = Leaf $ f a

instance Applicative tree => Applicative (Branch tree) where
   pure a = Branch (pure a) (pure a)
   Branch f g <*> Branch a b = Branch (f <*> a) (g <*> b)

instance Foldable Leaf where
   foldMap f (Leaf a) = f a

instance Foldable tree => Foldable (Branch tree) where
   foldMap f (Branch a0 a1) = foldMap f a0 <> foldMap f a1

instance Traversable Leaf where
   sequenceA (Leaf a) = fmap Leaf a

instance Traversable tree => Traversable (Branch tree) where
   sequenceA (Branch a0 a1) = liftA2 Branch (sequenceA a0) (sequenceA a1)


newtype
   WrapFunctor i a b idx =
      WrapFunctor {unwrapFunctor :: ExMultiValue idx i a -> ExMultiValue idx i b}

instance Dim.C idx => Functor (ExMultiValue idx i) where
   fmap f =
      unwrapFunctor $
      Dim.switch
         (WrapFunctor $ \(ExMultiValue is (Leaf a)) -> ExMultiValue is $ Leaf $ f a)
         (WrapFunctor $ \x ->
            case splitBranch x of
               (i, (a0, a1)) ->
                  exBranch i (fmap f a0) (fmap f a1))

exBranch ::
   i ->
   ExMultiValue idx i a ->
   ExMultiValue idx i a ->
   ExMultiValue (NonEmpty.T idx) i a
exBranch i (ExMultiValue is a) (ExMultiValue _ d) =
   ExMultiValue (NonEmpty.Cons i is) (Branch a d)

splitBranch ::
   ExMultiValue (NonEmpty.T idx) i a ->
   (i, (ExMultiValue idx i a, ExMultiValue idx i a))
splitBranch (ExMultiValue (NonEmpty.Cons i is) (Branch a0 a1)) =
   (i, (ExMultiValue is a0, ExMultiValue is a1))
