{-# LANGUAGE TypeFamilies #-}
module EFA.Test.Arithmetic where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import Control.Applicative (Applicative, pure, (<*>), liftA2, liftA3)

import qualified Test.QuickCheck as QC


data Triple a = Triple a a a
   deriving (Show, Eq)


instance Functor Triple where
   fmap f (Triple x y z) = Triple (f x) (f y) (f z)

instance Applicative Triple where
   pure x = Triple x x x
   (Triple fx fy fz) <*> (Triple x y z) =
      Triple (fx x) (fy y) (fz z)


instance (Sum a) => Sum (Triple a) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)
   negate = fmap Arith.negate

instance (Product a) => Product (Triple a) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip

instance (Constant a) => Constant (Triple a) where
   zero = pure zero
   fromInteger = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Sum a) => Integrate (Triple a) where
   type Scalar (Triple a) = a
   integrate (Triple x y z) = x ~+ y ~+ z


instance QC.Arbitrary a => QC.Arbitrary (Triple a) where
   arbitrary = liftA3 Triple QC.arbitrary QC.arbitrary QC.arbitrary
