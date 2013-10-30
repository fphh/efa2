module EFA.Signal.Base where

import qualified EFA.Equation.Arithmetic as Arith


data Sign = Positive | Zero | Negative deriving (Show, Eq, Enum)


sign :: (Ord a, Arith.Constant a) => a -> Sign
sign x | x > eps = Positive
       | x < Arith.negate eps = Negative
       | otherwise = Zero
       where eps = Arith.zero -- eps = 1^^(-12::Int) -- eps = 1^^(-1::Int)

signExact :: (Ord a, Arith.Constant a) => a -> Sign
signExact x =
   case compare x Arith.zero of
      GT -> Positive
      EQ -> Zero -- TODO add intervals later on Zero - Detection
      LT -> Negative
