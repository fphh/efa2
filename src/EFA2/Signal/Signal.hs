{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base

----------------------------------------------------------
-- | EFA data containers

newtype Data a b = Data (a b) deriving (Show, Eq, Ord)
--newtype Signal a b = Signal (a b) deriving (Show, Eq, Ord)

data ((a :: * -> *) :> (b :: * -> *)) :: * -> * where
     D0 :: v0 -> Nil v0 
     D1 :: v1 v0 -> (v1 :> Nil) v0
     D2 :: v2 (v1 v0) -> (v2 :> v1 :> Nil) v0
     D3 :: v3 (v2 (v1 v0)) -> (v3 :> v2 :> v1 :> Nil) v0

infixr 9 :>
data Nil' c = Nil' deriving (Show)
type Nil = Nil' :> Nil'

----------------------------------------------------------
-- Zipping for normal Arithmetics 

class SipWith c1 c2 d1 d2 d3  where
  sipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c2 d3

-- 0d - 0d
instance SipWith (Data Nil) (Data Nil) d1 d2 d3 where
         sipWith f (Data (D0 x)) (Data (D0 y)) = Data $ D0 $ f x y 

-- 0d - 1d
instance (VFunctor d2 d3 v1) => SipWith (Data Nil) (Data (v1 :> Nil))  d1 d2 d3  where
         sipWith f  (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vmap (f x) y 

-- 0d - 2d
instance (VFunctor d2 d3 v1,VFunctor (v1 d2) (v1 d3) v2) => SipWith (Data Nil) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         sipWith f  (Data (D0 x)) (Data (D2 y)) = Data $ D2 $ vmap (vmap (f x)) y 

-- 1d - 1d
instance (VZipper d1 d2 d3 v1) => SipWith (Data (v1 :> Nil)) (Data (v1 :> Nil))  d1 d2 d3  where
         sipWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vzipWith f x y 

-- 1d - 2d
instance (VZipper d1 d2 d3 v1, VFunctor (v1 d2) (v1 d3) v2) => SipWith (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         sipWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vmap (vzipWith f x) y 

-- 2d - 2d
instance (VZipper d1 d2 d3 v1, VZipper (v1 d1) (v1 d2) (v1 d3) v2) => SipWith (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         sipWith f  (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vzipWith (vzipWith f) x y 


----------------------------------------------------------
-- Zipping for cross Arithmetics 
{-
class CrossWith c1 c2 c3 d1 d2 d3  where
  crossWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3

-- 1d - 1d
instance CrossWith (Data (v1 :> Nil)) (Data (v1 :> Nil))  (Data (v1 :> v1 :> Nil))  d1 d2 d3  where
         crossWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D2 $ (vmap g) y
           where g y' = vmap (f y') x  
-}

{-
----------------------------------------------------------
-- Signal Aritmetics

class SArith s1 s2 s3 | s1 s2 -> s3   where
  (.*) :: s1 -> s2 -> s3
  (./) :: s1 -> s2 -> s3
  (.+) :: s1 -> s2 -> s3
  (.-) :: s1 -> s2 -> s3
     
instance  (SipWith c1 c2 c3 v1 v2 v3 d1 d2 d3, DArith d1 d2 d3) =>  SArith (c1 (v1 d1)) (c2(v2 d2)) (c3 (v3 d3)) where
          (.*) x y = sipWith (..*) x y
          (./) x y = sipWith (../) x y
          (.+) x y = sipWith (..+) x y
          (.-) x y = sipWith (..-) x y
-}    