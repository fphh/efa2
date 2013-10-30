module EFA.Signal.Base where

import qualified EFA.Equation.Arithmetic as Arith


-- Own User Defined Sign Variable
data Sign = PSign
          | ZSign
          | NSign deriving (Show, Eq, Enum)
-- data Sign = PSign | ZSign | NSign deriving (Show, Eq, Ord)


-- | determine Signal Sign
sign :: (Ord a, Arith.Constant a) => a -> Sign
sign x | x > eps = PSign
       | x < Arith.negate eps = NSign
       | otherwise = ZSign
       where eps = Arith.zero -- eps = 1^^(-12::Int) -- eps = 1^^(-1::Int)

{-
sign x =
   case compare x 0 of
      GT -> PSign
      EQ -> ZSign -- TODO add intervals later on Zero - Detection
      LT -> NSign
-}
