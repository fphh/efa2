{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, GeneralizedNewtypeDeriving,FlexibleContexts,OverlappingInstances #-} 

module EFA2.Signal.Vector (module EFA2.Signal.Vector) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative
import EFA2.Utils.Utils

----------------------------------------------------------
-- | 1. Numeric Data types
type Val = Double -- or Ratio
type IState = Int
type BState = Bool 

-- Phantom Type Flag to mark functions
data Unboxed
data Boxed


-- | Calculation classes for basic Datatypes
class DMult d1 d2 d3 | d1 d2 -> d3 where
 dmult :: Unboxed -> d1 ->  d2 -> d3
 ddiv :: Unboxed -> d1 -> d2 -> d3
 
instance DMult Val Val Val where
 dmult _ x y = x*y
 ddiv _  0 0 = 0
 ddiv _ x y = x/y

instance DMult Val Bool Val where
 dmult _ x True = x
 dmult _ x False = 0
 ddiv _ x False = 0
 ddiv _ x True = x
 
instance DMult Bool Bool Bool where
-- And  
 dmult _ x True = x
 dmult _ x False = False
-- Or 
 ddiv _ x False = x
 ddiv _ x True = True

class DSum d1 d2 d3 | d1 d2 -> d3 where
 dadd ::  Unboxed -> d1 -> d2 -> d3
 dsub ::  Unboxed -> d1 -> d2 -> d3

instance DSum Val Val Val where
 dadd _ x y = x+y
 dsub _ x y = x-y

class (Eq a, Ord a) => DEq a where
  (..==) :: u -> a -> a -> Bool
  (../=) :: u -> a -> a -> Bool
  (..>=) :: u -> a -> a -> Bool
  (..<=) :: u -> a -> a -> Bool
  (..>) ::  u -> a -> a -> Bool
  (..<) ::  u -> a -> a -> Bool
  
instance  (Eq a, Ord a) => DEq a where
  (..==) _ x y = x == y 
  (../=) _ x y = x /= y
  (..>=) _ x y = x >= y
  (..<=) _ x y = x <= y
  (..>) _  x y = x > y
  (..<) _  x y = x < y

data Sign = PSign | ZSign | NSign deriving Show

sign :: Boxed -> Val -> Sign 
sign _ x | x == 0 = ZSign
sign _ x | x > 0 = PSign
sign _ x | x < 0 = NSign


----------------------------------------------------------
-- | 2. Data Structures with 
 
type Vec  = V.Vector
type UVec  = UV.Vector
type List a = [a]

newtype EVal   d = EVal d deriving (Show)
newtype EList   d = EList [d] deriving (Show) 
newtype EVec    d = EVec (Vec d)  deriving (Show)
newtype EUVec d =  EUVec (UVec d) deriving (Show)

newtype EList2  d = EList2 [[d]]  deriving (Show)

---------------------------------------------------------------
-- Functor

-- Own Functor class which could swap containers
class EFunctor u c1 c2 d1 d2 | u c1 -> c2  where
  emap :: (u -> d1 -> d2) -> c1 d1 -> c2 d2 

instance  EFunctor u EVal EVal d1 d2 where
  emap f (EVal x) = EVal (f undefined x)

instance (UV.Unbox d1, UV.Unbox d2) => EFunctor Unboxed EUVec EUVec d1 d2  where   
  emap f (EUVec x) = EUVec $ UV.map (f undefined) x

instance  (UV.Unbox d2) => EFunctor Unboxed EVec EUVec d1 d2  where   
  emap f (EVec x) = EUVec $ UV.convert $ V.map (f undefined) x

instance  (UV.Unbox d1) => EFunctor Boxed EUVec EVec d1 d2  where   
   emap f (EUVec x) = EVec $ V.map (f undefined) $ UV.convert x

instance  EFunctor Boxed EVec EVec d1 d2  where   
   emap f (EVec x) = EVec $ V.map (f undefined) x

instance  EFunctor u EList EList d1 d2  where   
  emap f (EList x) = EList $ map (f undefined) x


---------------------------------------------------------------
-- ZipWith


-- Zip Classe
class EZipWith u c1 c2 c3 d1 d2 d3 | u c1 c2 -> c3 where -- | d1 d2 -> d3, c1 c2 -> c3 where 
  ezipWith :: (u -> d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3 


-- Uses eMap
         -- Unboxed
instance (UV.Unbox d2, UV.Unbox d3) => EZipWith Unboxed EVal EUVec EUVec d1 d2 d3  where
  ezipWith f (EVal x) (EUVec y) =  EUVec $ UV.map (f undefined x) y  
  
instance (UV.Unbox d3) => EZipWith Unboxed EVal EVec EUVec d1 d2 d3  where
   ezipWith f (EVal x) (EVec y) = EUVec $ UV.convert $ V.map (f undefined x) y
  
instance (UV.Unbox d2) => EZipWith Boxed EVal EUVec EVec d1 d2 d3  where
  ezipWith f (EVal x) (EUVec y) = EVec $ V.map  (f undefined x) $ UV.convert y
  
instance  EZipWith Boxed EVal EVec EVec d1 d2 d3  where
  ezipWith f (EVal x) (EVec y) = EVec $ V.map (f undefined x) y  
  
instance  EZipWith u EVal EList EList d1 d2 d3  where
  ezipWith f (EVal x) (EList y) = EList $ map (f undefined x) y  


-- zipWith with Length Check
m1 = "Error in EZipWith -- unequal length"


 -- With each other -- zip Needed
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => EZipWith Unboxed EUVec EUVec EUVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EUVec y) = if lCheck u v then EUVec $ UV.zipWith (f undefined) x y else error m1 
  

instance (UV.Unbox d1, UV.Unbox d2) => EZipWith Boxed EUVec EUVec EVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EUVec y) = if lCheck u v then EVec $ V.zipWith (f undefined) (V.convert x) (V.convert y) else error m1  
  

instance (UV.Unbox d1) => EZipWith Boxed EUVec EVec EVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EVec y) = if lCheck u v then EVec $ V.zipWith (f undefined) (V.convert x) y  else error m1  

instance (UV.Unbox d2) => EZipWith Boxed EVec EUVec EVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EUVec y) = if lCheck u v then EVec $ V.zipWith (f undefined) x (V.convert y)else error m1   

instance (UV.Unbox d3) => EZipWith Unboxed EVec EVec EUVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith (f undefined) x y  else error m1

instance (UV.Unbox d1,UV.Unbox d3) => EZipWith Unboxed EUVec EVec EUVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith (f undefined) (V.convert x) y  else error m1

instance (UV.Unbox d2,UV.Unbox d3) => EZipWith Unboxed EVec EUVec EUVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EUVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith (f undefined) x (V.convert y)  else error m1

-- boxed
instance EZipWith Boxed EVec EVec EVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EVec y) =  if lCheck u v then EVec $ V.zipWith (f undefined) x y else error m1 

instance EZipWith Boxed EList EList EList d1 d2 d3  where
  ezipWith f u@(EList x) v@(EList y) = if lCheck u v then EList $ zipWith (f undefined) x y  else error m1


---------------------------------------------------------------
-- Length & Length Check

class GetLength c d where
  len :: (c d) -> (Int,[Int])

instance GetLength EVal d where 
  len (EVal x) = (1,[])

instance GetLength EVec d where 
  len (EVec x) = (GV.length x,[])

instance UV.Unbox d => GetLength EUVec d where 
   len (EUVec x) = (UV.length x,[])

instance GetLength EList d where 
   len (EList x) = (length x,[])

class  SameLength c1 c2 d1 d2 where
  lCheck :: c1 d1  -> c2 d2 -> Bool
  
instance  (GetLength c1 d1, GetLength c2 d2) => SameLength c1 c2 d1 d2 where 
  lCheck x y = len x == len y

---------------------------------------------------------------
-- Monoid

class EMonoid c1 c2 d where
  eempty :: c2 d 
  (.++) :: c1 d -> c2 d -> c2 d
    
instance EMonoid EList EList d where 
  eempty = EList [] 
  (.++) (EList x) (EList y) = EList (x++y)  

instance EMonoid EList2 EList2 d where 
  eempty = EList2 [] 
  (.++) (EList2 x) (EList2 y) = EList2 (x++y)  

instance EMonoid EList EList2 d where 
  eempty = EList2 [[]] 
  (.++) (EList x) (EList2 y) = EList2 (map (x++) y)  

instance EMonoid EVec EVec d where 
  eempty = EVec $ GV.fromList [] 
  (.++) (EVec x) (EVec y) = EVec (x GV.++ y)  

instance UV.Unbox d => EMonoid EUVec EUVec d where 
  eempty = EUVec $ UV.fromList [] 
  (.++) (EUVec x) (EUVec y) = EUVec (x UV.++ y)  

  
---------------------------------------------------------------
-- Vector Packing 
  
class EC c s d where
  toEC :: s d -> c d  
  fromEC:: c d -> s d

instance EC EList [] d where 
  toEC x = EList x  
  fromEC (EList x) = x
  
instance EC EVec Vec d where 
  toEC x = EVec x  
  fromEC (EVec x) = x

instance EC EUVec UVec d where 
  toEC x = EUVec x  
  fromEC (EUVec x) = x

--------------------------------------------------------------
-- Vector Conversion
  
class EConvert c1 c2 d where   
  econvert :: c1 d -> c2 d
  
instance (UV.Unbox d) => EConvert EVec EUVec d where
   econvert (EVec x) = EUVec $ V.convert x 

instance (UV.Unbox d) => EConvert EUVec EVec d where
   econvert (EUVec x) = EVec $ UV.convert x 

instance EConvert EVec EList d where
  econvert (EVec x)  = EList $ V.toList x 

instance  (UV.Unbox d) => EConvert EUVec EList d where
  econvert (EUVec x)  = EList $ UV.toList x
  
instance EConvert EList EVec d where
  econvert (EList x)  = EVec $ V.fromList x 

instance  (UV.Unbox d) => EConvert EList EUVec d where
  econvert (EList x)  = EUVec $ UV.fromList x

}
---------------------------------------------------------------
-- Arith Funktionen  


class   CMult e1 e2 e3 where
  (.*) :: e1 -> e2 -> e3
  (./) :: e1 -> e2 -> e3
  
instance (EZipWith Unboxed c1 c2 c3 (s1 d1) (s2 d2) (s3 d3), DMult (s1 d1) (s2 d2) (s3 d3), EC c1 s1 d1, EC c2 s2 d2, EC c3 s3 d3) => CMult (c1 (s1 d1)) (c2 (s2 d2)) (c3 (s3 d3)) where
  (.*) x y =  ezipWith dmult x y
  (./) x y = ezipWith ddiv x y
 

class  CSum e1 e2 e3 where
  (.+) :: e1 -> e2 -> e3
  (.-) :: e1 -> e2 -> e3

instance (EZipWith Unboxed c1 c2 c3 (s1 d1) (s2 d2) (s3 d3), DSum (s1 d1) (s2 d2) (s3 d3)) => CSum (c1 (s1 d1)) (c2 (s2 d2)) (c3 (s3 d3)) where
  (.+) x y = ezipWith (dadd) x y
  (.-) x y = ezipWith (dsub) x y


{-
class CEq c1 c2 c3 d1 where
  (.==) :: c1 d1 -> c2 d1 -> c3 Bool
  (./=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.>=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.<=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.>) :: c1 d1 -> c2 d1 -> c3 Bool
  (.<) :: c1 d1 -> c2 d1 -> c3 Bool
  
instance (EZipWith Unboxed c1 c2 c3 d1 d1 Bool, Eq d1,Ord d1) => CEq c1 c2 c3 d1  where
  (.==)  x y = ezipWith (..==) x y
  (./=)  x y = ezipWith (../=) x y
  (.>=)  x y = ezipWith (..>=) x y
  (.<=)  x y = ezipWith (..<=) x y
  (.>)  x y = ezipWith (..>) x y
  (.<)  x y = ezipWith (..<) x y
-}



