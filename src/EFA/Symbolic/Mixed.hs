{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Type safe combination of expressions that represent scalars or signals.
-}
module EFA.Symbolic.Mixed where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, integrate)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Utility (Pointed, point)


{- |
The scalar parameter is needed for the Integrate instance.
We may also need it for future extensions.
-}
newtype Signal term scalar signal = Signal {getSignal :: term signal}

liftSignal ::
   (term signal -> term signal) ->
   Signal term scalar signal ->
   Signal term scalar signal
liftSignal f (Signal x) = Signal $ f x

liftSignal2 ::
   (term signal -> term signal -> term signal) ->
   Signal term scalar signal ->
   Signal term scalar signal ->
   Signal term scalar signal
liftSignal2 f (Signal x) (Signal y) = Signal $ f x y


instance
   (Eq (term signal)) =>
      Eq (Signal term scalar signal) where
   (Signal x) == (Signal y)  =  x==y

instance
   (Ord (term signal)) =>
      Ord (Signal term scalar signal) where
   compare (Signal x) (Signal y)  =  compare x y

instance
   (Sum (term signal)) =>
      Sum (Signal term scalar signal) where
   (~+) = liftSignal2 (~+)
   (~-) = liftSignal2 (~-)
   negate = liftSignal Arith.negate

instance
   (Product (term signal)) =>
      Product (Signal term scalar signal) where
   (~*) = liftSignal2 (~*)
   (~/) = liftSignal2 (~/)
   recip = liftSignal Arith.recip

instance
   (Constant (term signal)) =>
      Constant (Signal term scalar signal) where
   zero = Signal zero
   fromInteger = Signal . Arith.fromInteger
   fromRational = Signal . Arith.fromRational

instance
   (FormatValue (term signal)) =>
      FormatValue (Signal term scalar signal) where
   formatValue (Signal term) = formatValue term



data
   ScalarAtom term scalar signal =
        ScalarVariable scalar
      | Integral (Signal term scalar signal)

instance
   (Eq scalar, Eq (term signal)) =>
      Eq (ScalarAtom term scalar signal) where
   (ScalarVariable x) == (ScalarVariable y)  =  x==y
   (Integral x) == (Integral y)  =  x==y
   _ == _  =  False

instance
   (Ord scalar, Ord (term signal)) =>
      Ord (ScalarAtom term scalar signal) where
   compare (ScalarVariable x) (ScalarVariable y)  =  compare x y
   compare (Integral x) (Integral y)  =  compare x y
   compare (ScalarVariable _) (Integral _) = LT
   compare (Integral _) (ScalarVariable _) = GT

instance
   (FormatValue scalar, FormatValue (term signal)) =>
      FormatValue (ScalarAtom term scalar signal) where
   formatValue (ScalarVariable var) = formatValue var
   formatValue (Integral signal) =
      Format.integral $ formatValue signal


newtype
   Scalar term scalar signal =
      Scalar {getScalar :: term (ScalarAtom term scalar signal)}

liftScalar ::
   (term (ScalarAtom term scalar signal) ->
    term (ScalarAtom term scalar signal)) ->
   Scalar term scalar signal ->
   Scalar term scalar signal
liftScalar f (Scalar x) = Scalar $ f x

liftScalar2 ::
   (term (ScalarAtom term scalar signal) ->
    term (ScalarAtom term scalar signal) ->
    term (ScalarAtom term scalar signal)) ->
   Scalar term scalar signal ->
   Scalar term scalar signal ->
   Scalar term scalar signal
liftScalar2 f (Scalar x) (Scalar y) = Scalar $ f x y


instance
   (Eq (term (ScalarAtom term scalar signal))) =>
      Eq (Scalar term scalar signal) where
   (Scalar x) == (Scalar y)  =  x==y

instance
   (Sum (term (ScalarAtom term scalar signal))) =>
      Sum (Scalar term scalar signal) where
   (~+) = liftScalar2 (~+)
   (~-) = liftScalar2 (~-)
   negate = liftScalar Arith.negate

instance
   (Product (term (ScalarAtom term scalar signal))) =>
      Product (Scalar term scalar signal) where
   (~*) = liftScalar2 (~*)
   (~/) = liftScalar2 (~/)
   recip = liftScalar Arith.recip

instance
   (Constant (term (ScalarAtom term scalar signal))) =>
      Constant (Scalar term scalar signal) where
   zero = Scalar zero
   fromInteger = Scalar . Arith.fromInteger
   fromRational = Scalar . Arith.fromRational

instance
   (FormatValue (term (ScalarAtom term scalar signal))) =>
      FormatValue (Scalar term scalar signal) where
   formatValue (Scalar term) = formatValue term


instance (Pointed term) => Integrate (Signal term scalar signal) where
   type Scalar (Signal term scalar signal) = Scalar term scalar signal
   integrate = Scalar . point . Integral


mapSignal ::
   (term signal0 -> term signal1) ->
   Signal term scalar signal0 ->
   Signal term scalar signal1
mapSignal f (Signal x) = Signal $ f x

mapScalar ::
   ((ScalarAtom term scalar0 signal0 ->
     ScalarAtom term scalar1 signal1) ->
    (term (ScalarAtom term scalar0 signal0) ->
     term (ScalarAtom term scalar1 signal1))) ->
   (scalar0 -> scalar1) ->
   (term signal0 -> term signal1) ->
   Scalar term scalar0 signal0 ->
   Scalar term scalar1 signal1
mapScalar mp f g (Scalar scalar) =
   Scalar $ flip mp scalar $ \scalarAtom ->
      case scalarAtom of
         ScalarVariable symbol -> ScalarVariable $ f symbol
         Integral (Signal signal) -> Integral $ Signal $ g signal
