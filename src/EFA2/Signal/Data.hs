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
         show (D0 x) = "D0 (" ++ show x ++ ")"

instance (Show (v1 v0)) => Show ((v1 :> Nil) v0) where
         show (D1 x) = "D1 (" ++ show x ++ ")"

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

class DZipWith c1 c2 c3 d1 d2 d3 | c1 c2 -> c3 where
  dzipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3

-- 0d - 0d
instance DZipWith (Data Nil) (Data Nil) (Data Nil) d1 d2 d3 where
         dzipWith f (Data (D0 x)) (Data (D0 y)) = Data $ D0 $ f x y 

-- 0d - 1d
instance (VWalker v1 d2 d3) => DZipWith (Data Nil) (Data (v1 :> Nil))  (Data (v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vmap (f x) y 

-- 1d - 0d
instance (VWalker v1 d1 d3) => DZipWith  (Data (v1 :> Nil)) (Data Nil) (Data (v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D1 x))  (Data (D0 y)) = Data $ D1 $ vmap ((flip f) y) x 

-- 0d - 2d
instance (VWalker v1 d2 d3,VWalker v2 (v1 d2) (v1 d3)) => DZipWith (Data Nil) (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D2 y)) = Data $ D2 $ vmap (vmap (f x)) y 

-- 2d - 0d
instance (VWalker v1 d1 d3,VWalker v2 (v1 d1) (v1 d3)) => DZipWith (Data (v2 :> v1 :> Nil))  (Data Nil) (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D2 x)) (Data (D0 y)) = Data $ D2 $ vmap (vmap ((flip f) y)) x 

-- 1d - 1d
instance (VZipper v1 d1 d2 d3) => DZipWith (Data (v1 :> Nil)) (Data (v1 :> Nil)) (Data (v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vzipWith f x y 

-- 1d - 2d
instance (VZipper v1 d1 d2 d3, VWalker v2 (v1 d2) (v1 d3) ) => DZipWith (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil))   (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vmap (vzipWith f x) y 

-- 2d - 1d
instance (VZipper v1 d2 d1 d3, VWalker v2 (v1 d1) (v1 d3)) => DZipWith  (Data (v2 :> v1 :> Nil)) (Data (v1 :> Nil))  (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D2 x)) (Data (D1 y)) = Data $ D2 $ vmap (vzipWith (flip f) y) x 

-- 2d - 2d
instance (VZipper v1 d1 d2 d3, VZipper v2 (v1 d1) (v1 d2) (v1 d3)) => DZipWith (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dzipWith f  (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vzipWith (vzipWith f) x y 


----------------------------------------------------------
-- Zipping for cross Arithmetics 

class CrossWith c1 c2 c3 d1 d2 d3 | c1 c2 -> c3 where
  crossWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3

-- 1d - 1d -> 2d
instance (VWalker v2 d1 (v1 d3), VWalker v1 d2 d3) => CrossWith (Data (v2 :> Nil)) (Data (v1 :> Nil))  (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         crossWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D2 $ (vmap g) x
           where g xi = vmap (f xi) y  

-- 1d - 2d
instance (VZipper v2 d1 (v1 d2) (v1 d3), VWalker v1 d2 d3) => CrossWith (Data (v2 :> Nil)) (Data (v2 :> v1 :> Nil))   (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         crossWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vzipWith g x y
           where g xi yi = vmap (f xi) yi 

-- 2d - 1d
instance (VZipper v2 (v1 d1) d2 (v1 d3), VWalker v1 d1 d3) => CrossWith (Data (v2 :> v1 :> Nil))  (Data (v2 :> Nil))  (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         crossWith f  (Data (D2 x)) (Data (D1 y)) = Data $ D2 $ vzipWith g x y
           where g xi yi = vmap ((flip f) yi) xi 

----------------------------------------------------------
-- fold Functions
         
class D0Fold c1 c2 d1 d2 where
      d0foldl :: (d1 -> d2 -> d1) -> c1 d1 -> c2 d2 -> c1 d1 
      d0foldr :: (d1 -> d2 -> d2) -> c1 d2 -> c2 d1 -> c1 d2

-- 1d -> 0d
instance  VWalker v1 d1 d2 => D0Fold (Data Nil) (Data (v1 :> Nil)) d1 d2 where
         d0foldl f (Data ( D0 x)) (Data (D1 y)) = Data $ D0 $ vfoldl f x y
         d0foldr f (Data (D0 x)) (Data (D1 y)) = Data $ D0 $ vfoldr f x y

-- 2d -> 0d
instance (VWalker v1 d1 d2, VWalker v2 d1 (v1 d2)) => D0Fold (Data Nil) (Data (v2 :> v1 :> Nil)) d1 d2 where
         d0foldl f (Data ( D0 x)) (Data (D2 y)) = Data $ D0 $ vfoldl (vfoldl f) x y
--         d0foldr f (Data ( D0 x)) (Data (D2 y)) = Data $ D0 $ vfoldr (vfoldr f) x y
{-         
         dfoldr f (Data (D0 x)) (Data (D2 y)) = Data $ D0 $ vfoldr (vfoldr f) x y
    Could not deduce (d2 ~ v3 d1)
    from the context (VWalker v1 d1 d2, VWalker v2 d1 (v1 d2))
      bound by the instance declaration at EFA2/Signal/Data.hs:130:10-101
    or from (Nil' ~ Nil', Nil' ~ Nil')
-}

class D1Fold c1 c2 v1 d1 d2 where
      d1foldl :: (v1 d1 -> v1 d2 -> v1 d1) -> c1 d1 -> c2 d2 -> c1 d1 
--      d0foldr :: (v1 d1 -> v2 d2 -> v2 d2) -> c1 d2 -> c2 d1 -> c1 d2

-- 2d -> 1d
instance (VWalker v2 (v1 d1) (v1 d2)) => D1Fold (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil)) v1 d1 d2  where
         d1foldl f (Data (D1 x)) (Data (D2 y)) = Data $ D1 $ vfoldl f x y

----------------------------------------------------------
-- Append Class

class DAppend c1 c2 c3 d | c1 c2 -> c3 where
  dempty :: c3 d
  (.++) :: c1 d -> c2 d -> c3 d
  
-- 0d - 1d
instance VSingleton v1 d => DAppend (Data (v1 :> Nil))  (Data Nil)  (Data (v1 :> Nil)) d where
  dempty = Data $ D1 $ vempty
  (.++) (Data (D1 x)) (Data (D0 y)) = Data $ D1 $ vappend x (vsingleton y)
  
-- 1d -- 0d
instance VSingleton v1 d => DAppend  (Data Nil) (Data (v1 :> Nil)) (Data (v1 :> Nil)) d where
  dempty = Data $ D1 $ vempty
  (.++) (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vappend (vsingleton x) y

-- 1d -- 1d 
instance VSingleton v1 d => DAppend (Data (v1 :> Nil))  (Data (v1 :> Nil))  (Data (v1 :> Nil)) d where
  dempty = Data $ D1 $ vempty
  (.++) (Data (D1 x)) (Data (D1 y)) =  Data $ D1 $ vappend x y

-- 2d -- 2d 
instance (VSingleton v1 d, VSingleton v2 (v1 d), VZipper v2 (v1 d) (v1 d) (v1 d)) => DAppend (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil)) d where
  dempty = Data $ D2 $ vempty
  (.++) (Data (D2 x)) (Data (D2 y)) =  Data $ D2 $ vzipWith vappend x y
  

----------------------------------------------------------
-- Alternative Append Class

class DAppendAlt c1 c2 c3 d | c1 c2 -> c3 where
  (.++/) :: c1 d -> c2 d -> c3 d

-- 1d -- 2d 
instance (VSingleton v1 d,VSingleton v2 (v1 d)) => DAppendAlt (Data (v1 :> Nil))  (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil)) d where
  (.++/) (Data (D1 x)) (Data (D2 y)) =  Data $ D2 $ vappend (vsingleton x) y

-- 2d -- 1d 
instance (VSingleton v1 d,VSingleton v2 (v1 d)) => DAppendAlt (Data (v2 :> v1 :> Nil)) (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where
  (.++/) (Data (D2 x)) (Data (D1 y)) =  Data $ D2 $ vappend  x (vsingleton y)

-- 2d -- 2d 
instance (VSingleton v1 d,VSingleton v2 (v1 d)) => DAppendAlt (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil))  (Data (v2 :> v1 :> Nil)) d where
  (.++/) (Data (D2 x)) (Data (D2 y)) =  Data $ D2 $ vappend x y


{-

----------------------------------------------------------
-- get data Range

class DGetRange c1 c2 d | c1 -> c2 where
  dgetRange :: c1 d -> c2 (d,d) 
  
instance  (Bounded d,Ord d,VWalker y d) => DGetRange (Data (y :> Nil)) (Data Nil) d where 
  dgetRange x = dfoldl f (Data $ D0 $ (minBound, maxBound)) x
    where f (omin, omax) x' = (max omin x', min omax x') 
-}        
----------------------------------------------------------
-- get data Range
        
class DSingleton c1 c2 d | c1 -> c2 where
  dmaximum :: c1 d -> c2 d
  dminimum :: c1 d -> c2 d
  
instance (VSingleton y d) => DSingleton (Data (y :> Nil)) (Data Nil) d where
  dmaximum (Data (D1 x)) =  Data $ D0 $ vmaximum x          
  dminimum (Data (D1 x)) =  Data $ D0 $ vminimum x          