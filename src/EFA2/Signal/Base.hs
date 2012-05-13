{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 


module EFA2.Signal.Base (module EFA2.Signal.Base) where



----------------------------------------------------------
-- | 1. Data types
type Val = Double -- or Ratio
data Sign = PSign | ZSign | NSign deriving (Show, Eq, Ord)


-- | Calculation classes for basic Datatypes
class DArith d1 d2 d3 | d1 d2 -> d3 where
 (..*) ::  d1 ->  d2 -> d3
 (../) ::  d1 -> d2 -> d3
 (..+) ::  d1 -> d2 -> d3
 (..-) ::  d1 -> d2 -> d3
 
instance DArith Val Val Val where
 (..*) x y = x*y
 (../)  0 0 = 0
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



sign :: Val -> Sign 
sign x | x == 0 = ZSign
sign x | x > 0 = PSign
sign x | x < 0 = NSign
