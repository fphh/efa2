module EFA.Signal.Base where

import qualified EFA.Equation.Arithmetic as Arith


data Sign = PSign | ZSign | NSign deriving (Show, Eq, Enum)


sign :: (Ord a, Arith.Constant a) => a -> Sign
sign x | x > eps = PSign
       | x < Arith.negate eps = NSign
       | otherwise = ZSign
       where eps = Arith.zero -- eps = 1^^(-12::Int) -- eps = 1^^(-1::Int)

signExact :: (Ord a, Arith.Constant a) => a -> Sign
signExact x =
   case compare x Arith.zero of
      GT -> PSign
      EQ -> ZSign -- TODO add intervals later on Zero - Detection
      LT -> NSign
