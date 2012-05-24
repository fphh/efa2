{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 


module EFA2.Signal.Base (module EFA2.Signal.Base) where



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
class DArith d1 d2 d3 | d1 d2 -> d3 where
 (..*) ::  d1 ->  d2 -> d3
 (../) ::  d1 -> d2 -> d3
 (..+) ::  d1 -> d2 -> d3
 (..-) ::  d1 -> d2 -> d3
 
instance DArith Val Val Val where
 (..*) x y = x*y
 (../) 0 0 = 0
 (../) x y = x/y
 (..+) x y = x+y
 (..-) x y = x-y

instance DArith Val Bool Val where
 (..*) x True = x
 (..*) x False = 0
 (../) x False = 0
 (../) x True = x
 
instance DArith Bool Bool Bool where
-- And  
 (..*) x True = x
 (..*) x False = False
-- Or 
 (../) x False = x
 (../) x True = True

class DEq d1 d2 d3 | d1 d2 -> d3 where
  (..==) :: d1 -> d2 -> d3
  (../=) :: d1 -> d2 -> d3
  (..>=) :: d1 -> d2 -> d3
  (..<=) :: d1 -> d2 -> d3
  (..>) ::  d1 -> d2 -> d3
  (..<) ::  d1 -> d2 -> d3
  
instance DEq Val Val Bool where
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
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign

--sign' :: (Eq a, Ord a, Num a) => a -> Ordering
--sign' x = compare x 0

-- type PSample = Val
-- -- type TSample = Val
-- type DTSample = Val -- Time step
-- type FPSample = Val -- Flow Power


