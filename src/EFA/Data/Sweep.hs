{-# LANGUAGE TypeFamilies #-}

module EFA.Data.Sweep where

import qualified Test.QuickCheck as QC
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Report.FormatValue as FormatValue

newtype Sweep a = Sweep a

instance 
  (Arith.Sum a) => 
  Arith.Sum (Sweep a) where
  (Sweep x) ~+ (Sweep y) = Sweep $ (Arith.~+) x y
  {-# INLINE (~+) #-}

  (Sweep x) ~- (Sweep y) = Sweep $ (Arith.~-) x y
  {-# INLINE (~-) #-}

  negate (Sweep x) = Sweep $ Arith.negate x
  {-# INLINE negate #-}

instance 
  (Arith.Product a, 
   Arith.Constant a
  ) =>
         Arith.Product (Sweep a) where
  (Sweep x) ~* (Sweep y) = Sweep $ (Arith.~*) x y
  {-# INLINE (~*) #-}

  (Sweep x) ~/ (Sweep y) = Sweep $ (Arith.~/) x y
  {-# INLINE (~/) #-}

  recip (Sweep x) = Sweep $ Arith.recip x
  {-# INLINE recip #-}

  constOne (Sweep x) = Sweep $ Arith.constOne x
  {-# INLINE constOne #-}

instance Arith.Integrate (Sweep a) where
  type Scalar (Sweep a) = (Sweep a)
  integrate = id
  {-# INLINE integrate #-}

instance (Arith.ZeroTestable a,
  Arith.Constant a, 
  Eq a
  ) => 
         Arith.ZeroTestable (Sweep a) where
  allZeros (Sweep x) = Arith.allZeros x
  {-# INLINE allZeros #-}

  coincidingZeros (Sweep x) (Sweep y) = Arith.coincidingZeros x y

instance 
  (FormatValue.FormatValue a) =>
  FormatValue.FormatValue (Sweep a) where
  formatValue (Sweep x) = FormatValue.formatValue x

-- = Only testing
instance (
  QC.Arbitrary a) =>
 QC.Arbitrary (Sweep a) where
  arbitrary = QC.arbitrary
