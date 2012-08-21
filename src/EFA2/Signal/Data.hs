{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, KindSignatures, TypeOperators, GADTs #-}

module EFA2.Signal.Data (module EFA2.Signal.Data) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import Data.Monoid

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

----------------------------------------------------------
-- | EFA data containers

newtype Data ab c = Data (ab c) deriving (Show)


data ((a :: * -> *) :> (b :: * -> *)) :: * -> * where
     D0 :: v0 -> Nil v0 
     D1 :: v1 v0 -> (v1 :> Nil) v0
     D2 :: v2 (v1 v0) -> (v2 :> v1 :> Nil) v0
     D3 :: v3 (v2 (v1 v0)) -> (v3 :> v2 :> v1 :> Nil) v0 -- deriving (Show, Eq Ord)

infixr 9 :>
data Nil' c = Nil' -- deriving (Show,Eq,Ord)
type Nil = Nil' :> Nil'


instance (Show d) => Show (Nil d) where
         show (D0 x) = "D0 (" ++ show x ++ ")"

instance (Show (v1 d)) => Show ((v1 :> Nil) d) where
         show (D1 x) = "D1 (" ++ show x ++ ")"

instance (Show (v2 (v1 d))) => Show ((v2 :> v1 :> Nil) d) where
         show (D2 x) = "D2 (" ++ show x ++ ")"

instance (Show (v3 (v2 (v1 d)))) => Show ((v3 :> v2 :> v1 :> Nil) d) where
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


class DAppend c1 c2 c3 d | c1 c2 -> c3  where 
  dappend :: c1 d -> c2 d -> c3 d
  
instance (VSingleton v1 d, VConvert v1 v1 d) => DAppend (Data (v1 :> Nil)) (Data (v1 :> Nil)) (Data (v1 :> Nil)) d where  
  dappend (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vappend x (vconvert y)

instance (VSingleton v1 d) => DAppend (Data (v1 :> Nil)) (Data Nil) (Data (v1 :> Nil)) d where  
  dappend (Data (D1 x)) (Data (D0 y)) = Data $ D1 $ vappend x (vsingleton y)

instance (VSingleton v1 d) => DAppend (Data Nil) (Data (v1 :> Nil)) (Data (v1 :> Nil)) d where  
  dappend (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vappend (vsingleton x) y

instance (VSingleton v2 (v1 d)) => DAppend (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where  
  dappend (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vappend x y

instance (VSingleton v2 (v1 d)) => DAppend (Data (v2 :> v1 :> Nil)) (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where  
  dappend (Data (D2 x)) (Data (D1 y)) = Data $ D2 $ vappend x (vsingleton y)

instance (VSingleton v2 (v1 d)) => DAppend (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where  
  dappend (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vappend (vsingleton x) y

----------------------------------------------------------
-- get data Range
        
class DMaximum c1 c2 d | c1 -> c2 where
  dmaximum :: c1 d -> c2 d
  dminimum :: c1 d -> c2 d
  
instance (VSingleton y d) => DMaximum (Data (y :> Nil)) (Data Nil) d where
  dmaximum (Data (D1 x)) =  Data $ D0 $ vmaximum x          
  dminimum (Data (D1 x)) =  Data $ D0 $ vminimum x          
  
  
----------------------------------------------------------
-- From / To List
  
class DFromList c d where
  dfromList :: [d] -> c d
  dtoList :: c d -> [d]

instance  (VFromList v d) => DFromList (Data (v :> Nil)) d where  
  dfromList x = Data $ D1 $ vfromList x
  dtoList (Data (D1 x)) = vtoList x

{-
instance DFromList (Data Nil) d where  
  dfromList [x] = Data $ D0 x
  dtoList (Data (D0 x)) = [x]
  -}

class DFromList2 c d where
  dfromList2 :: [[d]] -> c d
  dtoList2 :: c d -> [[d]] 

instance (VFromList v1 d, VFromList v2 (v1 d), VFromList v2 [d], VWalker v2 (v1 d) [d])=> DFromList2 (Data (v2 :> v1 :> Nil)) d where  
  dfromList2 x = Data $ D2 $ vfromList $ vmap vfromList x
  dtoList2 (Data (D2 x)) = vtoList $ vmap vtoList x
  

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


----------------------------------------------------------
-- Head & Tail
  
class DHead c1 c2 d where  
  dhead :: c1 d -> c2 d
  dlast :: c1 d -> c2 d
  
instance VSingleton v1 d => DHead (Data (v1 :> Nil)) (Data Nil) d where
  dhead (Data (D1 x)) = Data $ D0 $ vhead x
  dlast (Data (D1 x)) = Data $ D0 $ vlast x

instance (VSingleton v2 (v1 d))=> DHead (Data (v2 :> v1 :> Nil)) (Data (v1 :> Nil)) d where
  dhead (Data (D2 x)) = Data $ D1 $ vhead x
  dlast (Data (D2 x)) = Data $ D1 $ vlast x



class DTail c1 c2 d | c1 -> c2, c2 -> c1  where
  dtail :: c1 d -> c2 d
  dinit :: c1 d -> c2 d
  
instance  (VSingleton v1 d) => DTail (Data (v1 :> Nil)) (Data (v1 :> Nil)) d where
  dtail (Data (D1 x)) = Data $ D1 $ vtail x 
  dinit (Data (D1 x)) = Data $ D1 $ vinit x

instance (VSingleton v2 (v1 d)) => DTail (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where
  dtail (Data (D2 x)) = Data $ D2 $ vtail x
  dinit (Data (D2 x)) = Data $ D2 $ vinit x
  
  
----------------------------------------------------------
-- Singleton

class DSingleton c1 c2 d where
  dsingleton :: c1 d -> c2 d
  
instance  (VSingleton v2 (v1 d)) => DSingleton (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil)) d where  
  dsingleton (Data (D1 x)) = Data $ D2 $ vsingleton x

instance  (VSingleton v1 d) => DSingleton (Data Nil) (Data (v1 :> Nil)) d where  
  dsingleton (Data (D0 x)) = Data $ D1 $ vsingleton x
  
  
----------------------------------------------------------
-- Sort

class DSort c d where
  dsort :: c d -> c d 
  
instance (VSort v1 d) => DSort (Data (v1 :> Nil)) d where   
  dsort (Data (D1 x)) = Data $ D1 $ vsort x
  

----------------------------------------------------------
-- Filter

class DFilter c d where  
  dfilter :: (d -> Bool) -> c d -> c d
  
instance VFilter v1 d => DFilter (Data (v1 :> Nil)) d where  
  dfilter f (Data (D1 x)) = Data $ D1 $ vfilter f x
  
  
----------------------------------------------------------
-- Eq

instance Eq d => Eq (Data Nil d) where
  (==) (Data (D0 x)) (Data (D0 y)) = x == y
  (/=) (Data (D0 x)) (Data (D0 y)) = x /= y

instance (Eq (v1 d), Eq d) => Eq (Data (v1 :> Nil) d) where
  (==) (Data (D1 xs)) (Data (D1 ys)) = xs == ys
  (/=) (Data (D1 xs)) (Data (D1 ys)) = xs /= ys

instance (Eq (v2 (v1 d)), Eq (v1 d), Eq d) => Eq (Data (v2 :> v1 :> Nil) d) where
  (==) (Data (D2 xs)) (Data (D2 ys)) = xs == ys
  (/=) (Data (D2 xs)) (Data (D2 ys)) = xs /= ys

instance Ord d => Ord (Data Nil d) where
  (>) (Data (D0 x)) (Data (D0 y)) = x > y
  (<) (Data (D0 x)) (Data (D0 y)) = x < y
  (>=) (Data (D0 x)) (Data (D0 y)) = x >= y
  (<=) (Data (D0 x)) (Data (D0 y)) = x <= y


----------------------------------------------------------
-- Convert
  
class DConvert  c1 c2 d where  
  dconvert :: c1 d -> c2 d

instance UV.Unbox d => DConvert (Data ([] :> Nil)) (Data (UV.Vector :> Nil)) d where
  dconvert (Data (D1 x)) = Data $ D1 $ vconvert x
  
instance DConvert (Data ([] :> Nil)) (Data (V.Vector :> Nil)) d where
  dconvert (Data (D1 x)) = Data $ D1 $ vconvert x  
  
instance UV.Unbox d => DConvert (Data (UV.Vector :> Nil)) (Data ([] :> Nil)) d where
  dconvert (Data (D1 x)) = Data $ D1 $ vconvert x    
  
instance DConvert (Data (V.Vector :> Nil)) (Data ([] :> Nil)) d where
  dconvert (Data (D1 x)) = Data $ D1 $ vconvert x      

instance UV.Unbox d => DConvert (Data ([] :> [] :> Nil)) (Data (V.Vector :> UV.Vector :> Nil)) d where
  dconvert (Data (D2 x)) = Data $ D2 $ vconvert (map vconvert x)      

----------------------------------------------------------
-- Length
  
class DLength c d where  
 dlength :: c d -> Int  
 
instance GetLength (v d) => DLength (Data (v :> Nil)) d where
  dlength (Data (D1 x)) = vlen x
  
instance  GetLength (v2 (v1 d)) => DLength (Data (v2 :> v1 :> Nil)) d where
  dlength (Data (D2 x)) = vlen x

----------------------------------------------------------
-- Reverse
  
class DReverse c d where  
  dreverse :: c d -> c d 
  
instance VReverse v d => DReverse (Data (v :> Nil)) d where
  dreverse (Data (D1 x)) = Data $ D1 $ vreverse x 
  
