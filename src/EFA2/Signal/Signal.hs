{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base

----------------------------------------------------------
-- | EFA data containers

newtype Scalar a = Scalar a deriving (Show, Eq, Ord)
newtype Signal a = Signal a deriving (Show, Eq, Ord)
newtype Signal2 a = Signal2 a deriving (Show, Eq, Ord)

newtype Distrib a= Distrib a deriving (Show, Eq, Ord)
newtype FSignal a = FSignal a deriving (Show, Eq, Ord)
newtype SampleVec a = SampleVec a deriving (Show, Eq, Ord)
newtype FSampleVec a = FSampleVec a deriving (Show, Eq, Ord)
newtype ClassVec a = ClassVec a deriving (Show, Eq, Ord)

----------------------------------------------------------
-- Unpack Classes

class FromOne s v d where
  fromOne :: s (v d) -> v d
  
instance FromOne Scalar v d where  
  fromOne (Scalar x) = x


class FromTwo s v d where
  fromTwo :: s ( v (v d))



----------------------------------------------------------
-- Zipping for normal Arithmetics 

class SipWith s1 s2 s3 d1 d2 d3 where
  sipWith :: (d1 -> d2 -> d3) -> s1 (v1 d1) -> (s2 (v2 d2) -> (s3 (v3 d3))
  
-- 0d - 0d
instance (VZipper d1 d2 d3 v1) => SipWith (Scalar (v1 d1)) (Scalar (v1 d2)) (Scalar (v1 d3))  d1 d2 d3 where
         sipWith f (Scalar x) (Scalar y) = Scalar $ vzipWith f x y 

-- 0d - 1d
instance (VFunctor d2 d3 v2) => SipWith (Scalar (Value d1)) (Signal (v2 d2)) (Signal (v2 d3)) d1 d2 d3  where
         sipWith f (Scalar (Value x)) (Signal y) = Signal $ vmap (f x) y 

-- 1d - 1d
instance (VZipper d1 d2 d3 v1) => SipWith (Signal (v1 d1)) (Signal (v1 d2)) (Signal (v1 d3)) d1 d2 d3 where
         sipWith f (Signal x) (Signal y) = Signal $ vzipWith f x y  

{-
 -- 1d - 2d
instance (VZipper d1 d2 d3 v1, VFunctor (v1 d2) (v1 d3) v1) => SipWith (Signal (v1 d1)) (Signal2 (v1(v1 d2))) (Signal2 (v1(v1 d3))) d1 d2 d3  where
         sipWith f (Signal x) (Signal2 y) = Signal2 $ vmap (vzipWith f x) y  

-- 2d - 2d
instance (VZipper d1 d2 d3 v1, VZipper (v1 d1) (v1 d2) (v1 d3) v1) => SipWith (Signal2 (v1 (v1 d1))) (Signal2 (v1 (v1 d2))) (Signal2 (v1(v1 d3))) d1 d2 d3 where
         sipWith f (Signal2 x) (Signal2 y) = Signal2 $ vzipWith (vzipWith f) x y  
-}


----------------------------------------------------------
-- Signal Aritmetics
{-
class SArith s1 s2 s3 | s1 s2 -> s3   where
   (.*) :: s1 -> s2 -> s3
--   (./) :: s1 -> s2 -> s3
--   (.+) :: s1 -> s2 -> s3
--   (.-) :: s1 -> s2 -> s3

instance (SipWith (c1 (v1 d1)) (c2(v2 d2)) (c3 (v3 d3)) d1 d2 d3, DArith d1 d2 d3) => SArith (c1 (v1 d1)) (c2 (v2 d2)) (c2 (v2 d3)) where
--instance (SipWith (s1 d1) (s2 d2) (s3 d3) d1 d2 d3 , DArith d1 d2 d3) => SArith s1 s2 s3 d1 d2 d3 where
  
  (.*) x y = sipWith (..*) x y
--  (./) x y = sipWith (../) x y
--  (.+) x y = sipWith (..+) x y
--  (.-) x y = sipWith (..-) x y

-}

class SArith s1 s2 s3 | s1 s2 -> s3   where
  (.*) :: s1 -> s2 -> s3
     
instance  (SipWith (c1 (v1 d1)) (c2(v2 d2)) (c3 (v3 d3)) d1 d2 d3, DArith d1 d2 d3) =>  SArith (c1 (v1 d1)) (c2(v2 d2)) (c3 (v3 d3)) where
          (.*) x y = sipWith (..*) x y
    