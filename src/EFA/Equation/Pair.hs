{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Pair (
   T(Cons, first, second),
   ) where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

{-
Ord instance intentionally omitted
since it means lexigraphic ordering
which is certainly unexpected in many cases.
-}
{- |
Data type that allows to perform the same computation
in different representations parallelly.
Don't think of it as a vector type.
Rather than this,
we use it for simultaneous symbolic and numeric computations.
-}
data T a b = Cons {first :: a, second :: b}
   deriving (Eq, Show)

instance (FormatValue a, FormatValue b) => FormatValue (T a b) where
   formatValue (Cons a b) = Format.pair (formatValue a) (formatValue b)


liftP1 ::
   (a -> a) ->
   (b -> b) ->
   T a b -> T a b
liftP1 f g (Cons a b) =
   Cons (f a) (g b)

liftP2 ::
   (a -> a -> a) ->
   (b -> b -> b) ->
   T a b -> T a b -> T a b
liftP2 f g (Cons a0 b0) (Cons a1 b1) =
   Cons (f a0 a1) (g b0 b1)

instance (Num a, Num b) => Num (T a b) where
   fromInteger n = Cons (fromInteger n) (fromInteger n)
   (+) = liftP2 (+) (+)
   (-) = liftP2 (-) (-)
   (*) = liftP2 (*) (*)
   negate = liftP1 negate negate
   abs = liftP1 abs abs
   signum = liftP1 signum signum

instance (Fractional a, Fractional b) => Fractional (T a b) where
   fromRational x = Cons (fromRational x) (fromRational x)
   (/) = liftP2 (/) (/)
   recip = liftP1 recip recip


instance (Sum a, Sum b) => Sum (T a b) where
   (~+) = liftP2 (~+) (~+)
   (~-) = liftP2 (~-) (~-)
   negate = liftP1 Arith.negate Arith.negate

instance (Product a, Product b) => Product (T a b) where
   (~*) = liftP2 (~*) (~*)
   (~/) = liftP2 (~/) (~/)
   recip = liftP1 Arith.recip Arith.recip
   constOne = liftP1 Arith.constOne Arith.constOne

instance (Constant a, Constant b) => Constant (T a b) where
   zero = Cons Arith.zero Arith.zero
   fromInteger n = Cons (Arith.fromInteger n) (Arith.fromInteger n)
   fromRational x = Cons (Arith.fromRational x) (Arith.fromRational x)

instance (Integrate a, Integrate b) => Integrate (T a b) where
   type Scalar (T a b) = T (Scalar a) (Scalar b)
   integrate (Cons a b) = Cons (integrate a) (integrate b)
