{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.Base where



----------------------------------------------------------
-- | 1. Data types
type Val = Double -- or Ratio Integer

class DArith0 d where
      neg :: d -> d
      rec :: d -> d

instance DArith0 Val where
         neg = negate
         rec = recip

infix 7 ..*, ../
infix 6 ..+, ..-

-- | Calculation classes for basic Datatypes
class BProd d1 d2 where
   (..*) :: d1 -> d2 -> d1
   (../) :: d1 -> d2 -> d1

instance BProd Val Val where
   (..*) x y = x*y
   (../) 0 0 = 0
   (../) x y = x/y

instance BProd Val Bool where
   (..*) x True = x
   (..*) _ False = 0
   (../) _ False = 0
   (../) x True = x

instance BProd Bool Bool where
   -- And
   (..*) x True = x
   (..*) _ False = False
   -- Or
   (../) x False = x
   (../) _ True = True


-- | Calculation classes for basic Datatypes
class BSum d1 where
   (..+) :: d1 -> d1 -> d1
   (..-) :: d1 -> d1 -> d1

instance BSum Val where
   (..+) x y = x+y
   (..-) x y = x-y


class DEq d1 where
   type Equal d1 :: *
   (..==) :: d1 -> d1 -> Equal d1
   (../=) :: d1 -> d1 -> Equal d1
   (..>=) :: d1 -> d1 -> Equal d1
   (..<=) :: d1 -> d1 -> Equal d1
   (..>) ::  d1 -> d1 -> Equal d1
   (..<) ::  d1 -> d1 -> Equal d1

instance DEq Val where
   type Equal Val = Bool
   (..==)  x y = x == y
   (../=)  x y = x /= y
   (..>=)  x y = x >= y
   (..<=)  x y = x <= y
   (..>)   x y = x > y
   (..<)   x y = x < y


infix 4 ..==, ../= , ..>= , ..<= , ..> , ..<

-- Own User Defined Sign Variable
data Sign = PSign
          | ZSign
          | NSign deriving (Show, Eq, Enum)
-- data Sign = PSign | ZSign | NSign deriving (Show, Eq, Ord)


-- | determine Signal Sign
sign :: (Ord a, Num a) => a -> Sign
sign x | x > eps = PSign
       | x < -eps = NSign
       | otherwise = ZSign
       where eps = 0 -- eps = 1^^(-12::Int) -- eps = 1^^(-1::Int)

{-
sign x =
   case compare x 0 of
      GT -> PSign
      EQ -> ZSign -- TODO add intervals later on Zero - Detection
      LT -> NSign
-}

data ZeroCrossing = ZeroCrossing Val | NoCrossing deriving (Show, Eq)
