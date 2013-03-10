{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.MultiValue where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import qualified Test.QuickCheck as QC

import qualified Control.Monad.Trans.State as MS
import qualified Data.Set as Set

import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, (<*>), liftA2)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))


{- |
The length of the list must match the depth of the tree.
The indices must be in strictly ascending order.
-}
data MultiValue i a = MultiValue [i] (Tree a)
   deriving (Show, Eq)

data Tree a =
     Leaf a
   | Branch (Tree a) (Tree a)
   deriving (Show, Eq)

instance Functor Tree where
   fmap f (Leaf a) = Leaf (f a)
   fmap f (Branch a0 a1) = Branch (fmap f a0) (fmap f a1)

-- | must only be applied if the index sets match
instance Applicative Tree where
   pure = Leaf
   Leaf f <*> Leaf a = Leaf $ f a
   Branch f g <*> Branch a b = Branch (f <*> a) (g <*> b)
   _ <*> _ = error "MultiValue.<*>: non-matching data structures"

instance Foldable Tree where
   foldMap f (Leaf a) = f a
   foldMap f (Branch a0 a1) = foldMap f a0 <> foldMap f a1


mergeTrees :: Ord i => [i] -> Tree a -> [i] -> Tree b -> Tree (a,b)
mergeTrees [] (Leaf a) _ bs = fmap ((,) a) bs
mergeTrees _ as [] (Leaf b) = fmap (flip (,) b) as
mergeTrees it@(i:is) a@(Branch a0 a1) jt@(j:js) b@(Branch b0 b1) =
   case compare i j of
      EQ -> Branch (mergeTrees is a0 js b0) (mergeTrees is a1 js b1)
      LT -> Branch (mergeTrees is a0 jt b ) (mergeTrees is a1 jt b )
      GT -> Branch (mergeTrees it a  js b0) (mergeTrees it a  js b1)
mergeTrees _ _ _ _ = error "MultiValue.mergeTrees: inconsistent data structure"

mergeIndices :: Ord i => [i] -> [i] -> [i]
mergeIndices [] js = js
mergeIndices is [] = is
mergeIndices it@(i:is) jt@(j:js) =
   case compare i j of
      EQ -> i : mergeIndices is js
      LT -> i : mergeIndices is jt
      GT -> j : mergeIndices it js


eqRelaxed :: (Ord i, Eq a) => MultiValue i a -> MultiValue i a -> Bool
eqRelaxed a b = case liftA2 (==) a b of MultiValue _is tree -> Fold.and tree


instance (Ord i) => Functor (MultiValue i) where
   fmap f (MultiValue is a) = MultiValue is (fmap f a)

instance (Ord i) => Applicative (MultiValue i) where
   pure = MultiValue [] . Leaf
   MultiValue is a <*> MultiValue js b =
      MultiValue
         (mergeIndices is js)
         (fmap (uncurry ($)) $ mergeTrees is a js b)


instance (Ord i, Num a) => Num (MultiValue i a) where
   fromInteger = pure . fromInteger
   negate = fmap negate
   (+) = liftA2 (+)
   (-) = liftA2 (-)
   (*) = liftA2 (*)
   abs = fmap abs
   signum = fmap signum

instance (Ord i, Fractional a) => Fractional (MultiValue i a) where
   fromRational = pure . fromRational
   recip = fmap recip
   (/) = liftA2 (/)


instance (Ord i, Sum a) => Sum (MultiValue i a) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)

instance (Ord i, Product a) => Product (MultiValue i a) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)

instance (Ord i, Constant a) => Constant (MultiValue i a) where
   zero = pure zero
   fromInteger = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Ord i, Integrate v) => Integrate (MultiValue i v) where
   type Scalar (MultiValue i v) = MultiValue i (Scalar v)
   integrate = fmap integrate


pair :: i -> a -> a -> MultiValue i a
pair i a0 a1 = MultiValue [i] (Branch (Leaf a0) (Leaf a1))

deltaPair :: Sum a => i -> a -> a -> MultiValue i a
deltaPair i a0 a1 = MultiValue [i] (Branch (Leaf a0) (Leaf (a0~+a1)))



instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a) =>
      QC.Arbitrary (MultiValue i a) where
   arbitrary = do
      let go (_:is) = liftA2 Branch (go is) (go is)
          go [] =
             MS.state $ \at ->
                case at of
                   [] -> error "wrong calculation of maximum length of index list"
                   a:as -> (Leaf a, as)

      at <- liftA2 (:) QC.arbitrary QC.arbitrary
      it <-
         fmap
            (take (floor $ logBase (2::Double) $ fromIntegral $ length at) .
             Set.toList . Set.fromList)
            QC.arbitrary
      return $ MultiValue it $ MS.evalState (go it) at


{-
possible tests:
addition and multiplication are commutative and associative
-}
