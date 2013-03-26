{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module EFA.Equation.MultiValueConsistent where

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Control.Applicative (Applicative, pure, (<*>), liftA2)
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))


{- |
The length of the list must match the depth of the tree.
The indices must be in strictly ascending order.
-}
data MultiValue i a =
        forall idx. (List idx) =>
           MultiValue (idx i) (Tree idx a)

data ExMultiValue idx i a = ExMultiValue (idx i) (Tree idx a)

newtype Leaf a = Leaf a
data Branch tree a = Branch (tree a) (tree a)


class
   (Applicative (Tree idx), Traversable (Tree idx),
    Functor idx) =>
      List idx where
   type Tree idx :: * -> *

instance List Empty.T where
   type Tree Empty.T = Leaf

instance List idx => List (NonEmpty.T idx) where
   type Tree (NonEmpty.T idx) = Branch (Tree idx)


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


instance Functor (Tree idx) => Functor (ExMultiValue idx i) where
   fmap f (ExMultiValue is x) = ExMultiValue is (fmap f x)
