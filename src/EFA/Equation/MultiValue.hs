{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.MultiValue (
   MultiValue(..),  -- constructors exported for Stack
   singleton, pair, deltaPair,

   eqRelaxed,
   ) where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           ZeroTestable, allZeros, coincidingZeros,
           Constant, zero,
           Integrate, Scalar, integrate)


import qualified Test.QuickCheck as QC

import qualified Data.Set as Set
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, (<*>), liftA2)
import Data.Traversable (Traversable, traverse, sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapPair)



{- |
The indices of type @i@ must be in strictly descending order.
-}
{-
Cf. "Numerical Representations as Higher-Order Nested Datatypes" by Ralf Hinze
-}
data MultiValue i a =
     MultiValue a
   | Dim i (MultiValue i (a, a))
   deriving (Show, Eq)

instance Functor (MultiValue i) where
   fmap f n =
      case n of
         MultiValue a -> MultiValue $ f a
         Dim i c -> Dim i $ fmap (mapPair (f,f)) c

instance Ord i => Applicative (MultiValue i) where
   pure a = MultiValue a
   MultiValue f <*> a = fmap f a
   f <*> MultiValue a = fmap ($a) f
   fd@(Dim i f) <*> ad@(Dim j a) =
      case compare i j of
         EQ -> Dim i $ liftA2 mapPair f a
         GT -> Dim i $ liftA2 (\(f0,f1) x -> (f0 x, f1 x)) f ad
         LT -> Dim j $ liftA2 (\g (x0,x1) -> (g x0, g x1)) fd a

{- breaks abstraction -}
instance Foldable (MultiValue i) where
   foldMap f (MultiValue a) = f a
   foldMap f (Dim _i c) = foldMap (\(a0,a1) -> f a0 <> f a1)  c

{- breaks abstraction -}
instance Traversable (MultiValue i) where
   sequenceA (MultiValue a) = fmap MultiValue a
   sequenceA (Dim i c) = fmap (Dim i) $ traverse (uncurry $ liftA2 (,)) c


eqRelaxed :: (Ord i, Eq a) => MultiValue i a -> MultiValue i a -> Bool
eqRelaxed a b = Fold.and $ liftA2 (==) a b



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
   negate = fmap Arith.negate

instance (Ord i, Product a) => Product (MultiValue i a) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip
   constOne = fmap Arith.constOne

instance (Ord i, Constant a) => Constant (MultiValue i a) where
   zero = pure zero
   fromInteger = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Ord i, Integrate v) => Integrate (MultiValue i v) where
   type Scalar (MultiValue i v) = MultiValue i (Scalar v)
   integrate = fmap integrate

instance (Ord i, ZeroTestable a) => ZeroTestable (MultiValue i a) where
   allZeros = Fold.all allZeros
   coincidingZeros x y = Fold.or $ liftA2 coincidingZeros x y


singleton :: a -> MultiValue i a
singleton = MultiValue

pair :: i -> a -> a -> MultiValue i a
pair i a0 a1 = Dim i $ MultiValue (a0, a1)

deltaPair :: Sum a => i -> a -> a -> MultiValue i a
deltaPair i a0 a1 = pair i a0 (a0~+a1)



instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a) =>
      QC.Arbitrary (MultiValue i a) where
   arbitrary =
      sequenceA . flip constant QC.arbitrary .
         reverse . take 4 . Set.toAscList . Set.fromList =<< QC.arbitrary

   shrink cube =
      removeEachDimension cube ++ removeValues cube


-- * private functions

constant :: [i] -> a -> MultiValue i a
constant [] a = MultiValue a
constant (i:is) a = Dim i $ constant is (a,a)

removeEachDimension :: MultiValue i a -> [MultiValue i a]
removeEachDimension (MultiValue _) = []
removeEachDimension (Dim i cube) =
   fmap fst cube :
   fmap snd cube :
   map (Dim i) (removeEachDimension cube)

removeValues :: (QC.Arbitrary a) => MultiValue i a -> [MultiValue i a]
removeValues (MultiValue a) = map MultiValue $ QC.shrink a
removeValues (Dim i cube) = map (Dim i) $ removeValues cube
