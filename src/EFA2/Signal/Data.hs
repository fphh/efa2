{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs #-}

module EFA2.Signal.Data (module EFA2.Signal.Data) where
import EFA2.Signal.Vector
import EFA2.Signal.Base

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

----------------------------------------------------------
-- | EFA data containers

newtype Data ab c = Data (ab c) deriving (Show, Eq, Ord)

data ((a :: * -> *) :> (b :: * -> *)) :: * -> * where
     D0 :: v0 -> Nil v0 
     D1 :: v1 v0 -> (v1 :> Nil) v0
     D2 :: v2 (v1 v0) -> (v2 :> v1 :> Nil) v0
     D3 :: v3 (v2 (v1 v0)) -> (v3 :> v2 :> v1 :> Nil) v0

infixr 9 :>
data Nil' c = Nil' deriving (Show)
type Nil = Nil' :> Nil'

instance (Show v0) => Show (Nil v0) where
         show (D0 x) = show x

instance (Show (v1 v0)) => Show ((v1 :> Nil) v0) where
         show (D1 x) = show x

instance (Show (v2 (v1 v0))) => Show ((v2 :> v1 :> Nil) v0) where
         show (D2 x) = show x

instance (Show (v3 (v2 (v1 v0)))) => Show ((v3 :> v2 :> v1 :> Nil) v0) where
         show (D3 x) = show x

----------------------------------------------------------
-- | Type Synonym Convenience

type DVal a = Data Nil a
type UVec a = (Data (UV.Vector :> Nil) a)
type UVec2 a = (Data (V.Vector :> UV.Vector :> Nil) a)
type UVec3 a = (Data (V.Vector :> V.Vector :> UV.Vector :> Nil) a)

type Vec a = (Data (V.Vector :> Nil) a)
type Vec2 a = (Data (V.Vector :> V.Vector :> Nil) a)
type Vec3 a = (Data (V.Vector :> V.Vector :> V.Vector :> Nil) a)

type List a = (Data ([] :> Nil) a)
type List2 a = (Data ([] :> [] :> Nil) a)
type List3 a = (Data ([]:> [] :> []:> Nil) a)

----------------------------------------------------------
-- | Zipping for normal Arithmetics 

class DZipWith c1 c2 d1 d2 d3  where
  dzipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c2 d3

-- 0d - 0d
instance DZipWith (Data Nil) (Data Nil) d1 d2 d3 where
         dzipWith f (Data (D0 x)) (Data (D0 y)) = Data $ D0 $ f x y 

-- 0d - 1d
instance (VFunctor d2 d3 v1) => DZipWith (Data Nil) (Data (v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vmap (f x) y 

-- 0d - 2d
instance (VFunctor d2 d3 v1,VFunctor (v1 d2) (v1 d3) v2) => DZipWith (Data Nil) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D2 y)) = Data $ D2 $ vmap (vmap (f x)) y 

-- 1d - 1d
instance (VZipper d1 d2 d3 v1) => DZipWith (Data (v1 :> Nil)) (Data (v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vzipWith f x y 

-- 1d - 2d
instance (VZipper d1 d2 d3 v1, VFunctor (v1 d2) (v1 d3) v2) => DZipWith (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vmap (vzipWith f x) y 

-- 2d - 2d
instance (VZipper d1 d2 d3 v1, VZipper (v1 d1) (v1 d2) (v1 d3) v2) => DZipWith (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vzipWith (vzipWith f) x y 


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


----------------------------------------------------------
-- fold Functions
         
class DFold c ca d where
      dfoldl :: (acc -> d -> acc) -> ca acc -> c d -> ca acc 
      dfoldr :: (d -> acc -> acc) -> ca acc -> c d -> ca acc

instance  (VFold y d) => DFold (Data (y :> Nil))  (Data Nil) d where
         dfoldl f (Data ( D0 acc)) (Data (D1 x)) = Data $ D0 $ vfoldl f acc x
         dfoldr f (Data (D0 acc)) (Data (D1 x)) = Data $ D0 $ vfoldr f acc x

----------------------------------------------------------
-- get data Range

class DGetRange c1 c2 d | c1 -> c2 where
  dgetRange :: c1 d -> c2 (d,d) 
  
instance  (Bounded d,Ord d,VFold y d) => DGetRange (Data (y :> Nil)) (Data Nil) d where 
  dgetRange x = dfoldl f (Data $ D0 $ (minBound, maxBound)) x
    where f (omin, omax) x' = (max omin x', min omax x') 
        
----------------------------------------------------------
-- get data Range
        
class DSingleton c1 c2 d | c1 -> c2 where
  dmaximum :: c1 d -> c2 d
  dminimum :: c1 d -> c2 d
  
instance (VSingleton y d) => DSingleton (Data (y :> Nil)) (Data Nil) d where
  dmaximum (Data (D1 x)) =  Data $ D0 $ vmaximum x          
  dminimum (Data (D1 x)) =  Data $ D0 $ vminimum x          