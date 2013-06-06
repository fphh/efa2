{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Arithmetic where

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System as Sys

import Data.Ratio (Ratio)

import qualified Prelude as P
import Prelude hiding (negate, recip, fromInteger, fromRational)


infixl 6 ~+, ~-
infixl 7 ~*, ~/

{- |
The tilde symbolizes waves or signals
since we want to use these operations to unify arithmetic
on scalars, signals and more complex structures built from signals.
-}
class Sum a where
   (~+), (~-) :: a -> a -> a
   negate :: a -> a
   x ~- y = x ~+ negate y

class Sum a => Product a where
   (~*), (~/) :: a -> a -> a
   recip :: a -> a
   {- |
   We need this to generate constant signals with value 1
   and a length that matches another signal.
   It should be @constOne x = x/x@ where @0/0=1@.
   Once we support length measurements of signals,
   we should remove this method, again.
   -}
   constOne :: a -> a
   x ~/ y = x ~* recip y

class Product a => Constant a where
   zero :: a
   fromInteger :: Integer -> a
   fromRational :: Rational -> a


instance Sum Integer where (~+) = (+); (~-) = (-); negate = P.negate
instance Sum Float where (~+) = (+); (~-) = (-); negate = P.negate
instance Sum Double where (~+) = (+); (~-) = (-); negate = P.negate
instance Integral a => Sum (Ratio a) where (~+) = (+); (~-) = (-); negate = P.negate

instance Product Float where (~*) = (*); (~/) = (/); recip = P.recip; constOne = const 1
instance Product Double where (~*) = (*); (~/) = (/); recip = P.recip; constOne = const 1
instance Integral a => Product (Ratio a) where (~*) = (*); (~/) = (/); recip = P.recip; constOne = const 1

instance Constant Float where
   zero = 0
   fromInteger = P.fromInteger
   fromRational = P.fromRational

instance Constant Double where
   zero = 0
   fromInteger = P.fromInteger
   fromRational = P.fromRational

instance Integral a => Constant (Ratio a) where
   zero = 0
   fromInteger = P.fromInteger
   fromRational = P.fromRational




ruleAdd ::
   (Sys.C t, Sys.Value t a, Sum a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleAdd = Rule.generic3 (flip (~-)) (~-) (~+)

ruleNegate ::
   (Sys.C t, Sys.Value t a, Sum a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleNegate = Rule.generic2 negate negate

ruleMul ::
   (Sys.C t, Sys.Value t a, Product a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleMul = Rule.generic3 (flip (~/)) (~/) (~*)

ruleRecip ::
   (Sys.C t, Sys.Value t a, Product a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleRecip = Rule.generic2 recip recip


instance (Sys.C t, Sys.Value t a, Sum a) => Sum (Expr.T t s a) where
   (~+) = Expr.fromRule3 ruleAdd
   (~-) = Expr.fromRule3 (\z x y -> ruleAdd x y z)
   negate = Expr.fromRule2 ruleNegate

instance (Sys.C t, Sys.Value t a, Product a) => Product (Expr.T t s a) where
   (~*) = Expr.fromRule3 ruleMul
   (~/) = Expr.fromRule3 (\z x y -> ruleMul x y z)
   recip = Expr.fromRule2 ruleRecip
   constOne = Expr.fromRule2 $ Sys.assignment2 constOne

instance (Sys.C t, Sys.Value t a, Constant a) => Constant (Expr.T t s a) where
   zero = Expr.constant zero
   fromInteger  = Expr.constant . fromInteger
   fromRational = Expr.constant . fromRational




class Integrate v where
   type Scalar v :: *
   integrate :: v -> Scalar v

instance Integrate Float where
   type Scalar Float = Float
   integrate = id

instance Integrate Double where
   type Scalar Double = Double
   integrate = id

instance (Integral a) => Integrate (Ratio a) where
   type Scalar (Ratio a) = Ratio a
   integrate = id

instance (Constant a) => Integrate [a] where
   type Scalar [a] = a
   integrate = foldl (~+) zero

instance
   (Sys.C t, Sys.Value t (Scalar v), Integrate v) =>
      Integrate (Expr.T t s v) where
   type Scalar (Expr.T t s v) = Expr.T t s (Scalar v)
   integrate = Expr.fromRule2 . Sys.assignment2 $ integrate


{- |
Construct a zero that is compatible with the argument.
E.g. for a signal argument it creates
a signal of the same length filled with zeros.
In the future we might make this a method of the Sum class.
-}
clear :: Sum a => a -> a
clear x = x~-x

one :: Constant a => a
one = fromInteger 1
