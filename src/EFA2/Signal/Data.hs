{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, StandaloneDeriving #-}

module EFA2.Signal.Data (module EFA2.Signal.Data) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import Data.Monoid

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
         show (D2 x) = "D2 (" ++ show x ++ ")"

instance (Show (v3 (v2 (v1 v0)))) => Show ((v3 :> v2 :> v1 :> Nil) v0) where
         show (D3 x) = "D3 (" ++ show x ++ ")"

---------------------------------------------------------
-- | Type Synonym Convenience

type DVal a = Data Nil a
type UVec a = (Data (UV.Vector :> Nil) a)
type UVec2 a = (Data (V.Vector :> UV.Vector :> Nil) a)
type UVec3 a = (Data (V.Vector :> V.Vector :> UV.Vector :> Nil) a)

type UVec2L a = (Data ([] :> UV.Vector :> Nil) a)

type Vec a = (Data (V.Vector :> Nil) a)
type Vec2 a = (Data (V.Vector :> V.Vector :> Nil) a)
type Vec3 a = (Data (V.Vector :> V.Vector :> V.Vector :> Nil) a)

type List a = (Data ([] :> Nil) a)
type List2 a = (Data ([] :> [] :> Nil) a)
type List3 a = (Data ([]:> [] :> []:> Nil) a)

----------------------------------------------------------
-- | mapping

class DMap c d1 d2 where
  dmap :: (d1 -> d2) -> c d1 -> c d2
  
instance DMap (Data Nil) d1 d2 where  
  dmap f (Data (D0 x)) = Data $ D0 $ f x
  
instance (VWalker v1 d1 d2) => DMap (Data (v1 :> Nil)) d1 d2 where  
  dmap f (Data (D1 x)) = Data $ D1 $ vmap f x

instance (VWalker v1 d1 d2, VWalker v2 (v1 d1) (v1 d2)) => DMap (Data (v2 :> v1 :> Nil)) d1 d2 where  
  dmap f (Data (D2 x)) = Data $ D2 $ vmap (vmap f) x

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

class DCrossWith c1 c2 c3 d1 d2 d3 | c1 c2 -> c3 where
  dcrossWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3

-- 1d - 1d -> 2d
instance (VWalker v2 d1 (v1 d3), VWalker v1 d2 d3) => DCrossWith (Data (v2 :> Nil)) (Data (v1 :> Nil))  (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dcrossWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D2 $ vmap g x
           where g xi = vmap (f xi) y  

-- 1d - 2d
instance (VZipper v2 d1 (v1 d2) (v1 d3), VWalker v1 d2 d3) => DCrossWith (Data (v2 :> Nil)) (Data (v2 :> v1 :> Nil))   (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dcrossWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vzipWith g x y
           where g xi yi = vmap (f xi) yi 

-- 2d - 1d
instance (VZipper v2 (v1 d1) d2 (v1 d3), VWalker v1 d1 d3) => DCrossWith (Data (v2 :> v1 :> Nil))  (Data (v2 :> Nil))  (Data (v2 :> v1 :> Nil)) d1 d2 d3  where
         dcrossWith f  (Data (D2 x)) (Data (D1 y)) = Data $ D2 $ vzipWith g x y
           where g xi yi = vmap ((flip f) yi) xi 

----------------------------------------------------------
-- fold Functions

class DFold c d1 d2 where
      dfoldl :: (d1 -> d2 -> d1) -> d1 -> c d2 -> d1 
      dfoldr :: (d1 -> d2 -> d2) -> d2 -> c d1 -> d2
      
instance (VWalker v1 d1 d2) => DFold (Data (v1 :> Nil)) d1 d2 where
         dfoldl f x  (Data (D1 y)) = vfoldl f x y 
         dfoldr f x  (Data (D1 y)) = vfoldr f x y 
     
instance (VWalker v2 d1 (v1 d2), VWalker v1 d1 d2, VWalker v2 (v1 d1) d2) =>  DFold (Data (v2 :> v1 :> Nil)) d1 d2 where
         dfoldl f x  (Data (D2 y)) = vfoldl (vfoldl f) x y 
         dfoldr f x  (Data (D2 y)) = vfoldr (flip (vfoldr f)) x  y 


class D1Fold c v1 d1 d2 where
      d1foldl :: (v1 d1 -> v1 d2 -> v1 d1) -> (v1 d1) -> c d2 -> (v1 d1) 
      d1foldr :: (v1 d1 -> v1 d2 -> v1 d2) -> (v1 d2) -> c d1 -> (v1 d2) 

-- 2d -> 1d
instance (VWalker v2 (v1 d1) (v1 d2)) => D1Fold (Data (v2 :> v1 :> Nil)) v1 d1 d2  where
         d1foldl f x (Data (D2 y)) = vfoldl f x y
         d1foldr f x (Data (D2 y)) = vfoldr f x y

----------------------------------------------------------
-- Monoid

instance (VSingleton v1 d) => Monoid (Data (v1 :> Nil) d) where
   mempty = Data $ D1 $ vempty        
   mappend (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vappend x y    

instance (VSingleton v2 (v1 d)) => Monoid (Data (v2 :> v1 :> Nil) d) where
   mempty = Data $ D2 $ vempty        
   mappend (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vappend x y    

----------------------------------------------------------
-- get data Range
        
class DSingleton c1 c2 d | c1 -> c2 where
  dmaximum :: c1 d -> c2 d
  dminimum :: c1 d -> c2 d
  
instance (VSingleton y d) => DSingleton (Data (y :> Nil)) (Data Nil) d where
  dmaximum (Data (D1 x)) =  Data $ D0 $ vmaximum x          
  dminimum (Data (D1 x)) =  Data $ D0 $ vminimum x          
  
  
----------------------------------------------------------
-- From / To List
  
class FromToList c d where
  dfromList :: [d] -> c d
  dtoList :: c d -> [d] 
  
instance  (UV.Unbox d) => FromToList (Data (UV.Vector :> Nil)) d where  
  dfromList x = Data $ D1 $ UV.fromList x
  dtoList (Data (D1 x)) = UV.toList x
  
instance FromToList (Data (V.Vector :> Nil)) d where  
  dfromList x = Data $ D1 $ V.fromList x
  dtoList (Data (D1 x)) = V.toList x  
  
instance FromToList (Data ([] :> Nil)) d where  
  dfromList x = Data $ D1 $ x
  dtoList (Data (D1 x)) = x
  
{-  
instance FromToList (Data ([] :> UV.Vector :> Nil)) d where  
  dfromList x = Data $ D2 $ x
  dtoList (Data (D2 x)) = x
-}  

----------------------------------------------------------
-- All

class DAll c d where
  dall :: (d -> Bool) -> c d -> Bool
  dany :: (d -> Bool) -> c d -> Bool
  
instance (VSingleton v d) => DAll (Data (v :> Nil)) d where  
  dall f (Data (D1 x)) = vall f x
  dany f (Data (D1 x)) = vany f x
  
----------------------------------------------------------
-- Transpose
  
class DTranspose c d where  
  dtranspose :: c d -> c d
  
instance DTranspose (Data (v1 :> Nil)) d where    
  dtranspose x = x
  
instance VTranspose v1 v2 d => DTranspose (Data (v2 :> v1 :> Nil)) d where    
  dtranspose (Data (D2 x)) = Data $ D2 $ vtranspose x
