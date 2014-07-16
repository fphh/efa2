{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Arithmetic where

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System as Sys

import Control.Applicative (liftA2)
import Data.Maybe.HT (toMaybe)
import Data.Bool.HT (if')
import Data.Ratio (Ratio)

import qualified Prelude as P
import Prelude hiding (negate, recip, abs, fromInteger, fromRational)


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

class ZeroTestable a where
   allZeros :: a -> Bool
   coincidingZeros :: a -> a -> Bool


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


instance ZeroTestable Integer where
   allZeros = (0==)
   coincidingZeros x y = x==0 && y==0

instance ZeroTestable Float where
   allZeros = (0==)
   coincidingZeros x y = x==0 && y==0

instance ZeroTestable Double where
   allZeros = (0==)
   coincidingZeros x y = x==0 && y==0

instance (Integral a) => ZeroTestable (Ratio a) where
   allZeros = (0==)
   coincidingZeros x y = x==0 && y==0

class NaNTestable a where
  checkIsNaN :: a -> Bool
  
instance NaNTestable Double where  
  checkIsNaN = isNaN
  
instance NaNTestable (Ratio a) where  
  checkIsNaN _ = False
  
instance NaNTestable Integer where  
  checkIsNaN _ = False
  
instance NaNTestable Float where  
  checkIsNaN = isNaN

ruleAdd ::
   (Sys.Value t a, Sum a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleAdd = Rule.generic3 (flip (~-)) (~-) (~+)

ruleNegate ::
   (Sys.Value t a, Sum a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleNegate = Rule.generic2 negate negate

ruleMul ::
   (Sys.Value t a, Product a, ZeroTestable a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleMul x y z =
   sequence_ $
   Sys.assignment3 (~*) x y z :
   Sys.runApplyMaybe (fmap zeroMul (Sys.arg x)) z :
   Sys.runApplyMaybe (fmap zeroMul (Sys.arg y)) z :
   Sys.runApplyMaybe (liftA2 zeroDiv (Sys.arg z) (Sys.arg y)) x :
   Sys.runApplyMaybe (liftA2 zeroDiv (Sys.arg z) (Sys.arg x)) y :
   []

zeroMul :: (ZeroTestable a) => a -> Maybe a
zeroMul x = toMaybe (allZeros x) x

zeroDiv :: (Product a, ZeroTestable a) => a -> a -> Maybe a
zeroDiv z x = toMaybe (not $ coincidingZeros x z) (z~/x)

ruleRecip ::
   (Sys.Value t a, Product a) =>
   Sys.Variable t s a -> Sys.Variable t s a -> Sys.T t s ()
ruleRecip = Rule.generic2 recip recip


instance (Sys.Value t a, Sum a) => Sum (Expr.T t s a) where
   (~+) = Expr.fromRule3 ruleAdd
   (~-) = Expr.fromRule3 (\z x y -> ruleAdd x y z)
   negate = Expr.fromRule2 ruleNegate

instance
   (Sys.Value t a, Product a, ZeroTestable a) =>
      Product (Expr.T t s a) where
   (~*) = Expr.fromRule3 ruleMul
   (~/) = Expr.fromRule3 (\z x y -> ruleMul x y z)
   recip = Expr.fromRule2 ruleRecip
   constOne = Expr.fromRule2 $ Sys.assignment2 constOne

instance
   (Sys.Value t a, Constant a, ZeroTestable a) =>
      Constant (Expr.T t s a) where
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
   (Sys.Value t (Scalar v), Integrate v) =>
      Integrate (Expr.T t s v) where
   type Scalar (Expr.T t s v) = Expr.T t s (Scalar v)
   integrate = Expr.fromRule2 . Sys.assignment2 $ integrate



class Integrate v => Scale v where
   scale :: Scalar v -> v -> v

instance Scale Float where
   scale = (*)

instance Scale Double where
   scale = (*)

instance (Integral a) => Scale (Ratio a) where
   scale = (*)

instance (Constant a) => Scale [a] where
   scale = map . (~*)

instance
   (Sys.Value t v, Sys.Value t (Scalar v), Scale v) =>
      Scale (Expr.T t s v) where
   scale = Expr.fromRule3 . Sys.assignment3 $ scale


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


square :: Product a => a -> a
square x = x~*x

(^!) :: Product a => a -> Int -> a
x^!n =
   let go 1 y = y
       go m y =
          case divMod m 2 of
             (q, r) ->
                let yq = go q $ square y
                in  if r==0 then yq else yq~*y
   in  if n<=0
         then error "exponent must be positive"
         else go n x


abs :: (Sum a, Ord a) => a -> a
abs x = max x $ negate x


data Sign = Negative | Zero | Positive deriving (Show, Eq, Enum)


sign :: (Ord a, Constant a) => a -> Sign
sign x =
   case compare x zero of
      LT -> Negative
      EQ -> Zero
      GT -> Positive

signApprox :: (Ord a, Constant a) => a -> a -> Sign
signApprox eps x =
   if' (x > eps) Positive $
   if' (x < negate eps) Negative $
   Zero


mean :: (Sum a, Product a, Constant a ) => a -> a -> a
mean x y = (x ~+ y) ~/ (one ~+ one)