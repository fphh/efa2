module EFA.Equation.Arithmetic where

import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.Rule as Rule
import qualified UniqueLogic.ST.System as Sys

import Data.Ratio (Ratio)

import qualified Prelude as P
import Prelude hiding (fromInteger, fromRational)


infixl 6 ~+, ~-
infixl 7 ~*, ~/

{- |
The tilde symbolizes waves or signals
since we want to use these operations to unify arithmetic
on scalars, signals and more complex structures built from signals.
-}
class Sum a where
   (~+), (~-) :: a -> a -> a

class Sum a => Product a where
   (~*), (~/) :: a -> a -> a

class Product a => Constant a where
   zero :: a
   fromInteger :: Integer -> a
   fromRational :: Rational -> a


instance Sum Integer where (~+) = (+); (~-) = (-)
instance Sum Float where (~+) = (+); (~-) = (-)
instance Sum Double where (~+) = (+); (~-) = (-)
instance Integral a => Sum (Ratio a) where (~+) = (+); (~-) = (-)

instance Product Float where (~*) = (*); (~/) = (/)
instance Product Double where (~*) = (*); (~/) = (/)
instance Integral a => Product (Ratio a) where (~*) = (*); (~/) = (/)

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
   (Sum a) =>
   Sys.Variable s a -> Sys.Variable s a -> Sys.Variable s a -> Sys.M s ()
ruleAdd = Rule.generic3 "add" (flip (~-)) (~-) (~+)

ruleMul ::
   (Product a) =>
   Sys.Variable s a -> Sys.Variable s a -> Sys.Variable s a -> Sys.M s ()
ruleMul = Rule.generic3 "mul" (flip (~/)) (~/) (~*)

instance (Sum a) => Sum (Expr.T s a) where
   (~+) = Expr.fromRule3 ruleAdd
   (~-) = Expr.fromRule3 (\z x y -> ruleAdd x y z)

instance (Product a) => Product (Expr.T s a) where
   (~*) = Expr.fromRule3 ruleMul
   (~/) = Expr.fromRule3 (\z x y -> ruleMul x y z)

instance (Constant a) => Constant (Expr.T s a) where
   zero = Expr.constant zero
   fromInteger  = Expr.constant . fromInteger
   fromRational = Expr.constant . fromRational
