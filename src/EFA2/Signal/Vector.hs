{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures,IncoherentInstances, GeneralizedNewtypeDeriving,FlexibleContexts #-}

module EFA2.Signal.Vector (module EFA2.Signal.Vector) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative

----------------------------------------------------------
-- | 1. Numeric Data types
type Val = Double -- or Ratio
type IState = Int
type BState = Bool 


-- | Calculation classes for basic Datatypes
class DMult d1 d2 d3 | d1 d2 -> d3 where
 (.*.) :: d1 -> d2 -> d3
 (./.) :: d1 -> d2 -> d3
 
instance DMult Val Val Val where
 (.*.) x y = x*y
 (./.) 0 0 = 0
 (./.) x y = x/y

instance DMult Val Bool Val where
 (.*.) x True = x
 (.*.) x False = 0
 (./.) x False = 0
 (./.) x True = x
 
instance DMult Bool Bool Bool where
 (.*.) x True = x
 (.*.) x False = False
 (./.) x False = False
 (./.) x True = x

class DSum d1 d2 d3 | d1 d2 -> d3 where
 (.+.) :: d1 -> d2 -> d3
 (.-.) :: d1 -> d2 -> d3

instance DSum Val Val Val where
 (.+.) x y = x+y
 (.-.) x y = x-y


data Sign = PSign | ZSign | NSign deriving Show
sign :: Val -> Sign 
sign x | x == 0 = NSign
sign x | x > 0 = PSign
sign x | x < 0 = NSign


----------------------------------------------------------
-- | 2. Data Structures with 
 
type Vec  = V.Vector
type UVec  = UV.Vector
-- type Vec2 = V.Vector V.Vector 
type List a = [a]

newtype EVal   d = EVal d deriving (Show)
newtype EList   d = EList [d] deriving (Show) 
newtype EList2  d = EList2 [[d]]  deriving (Show)
newtype EVec    d = EVec (Vec d)  deriving (Show)
newtype EVec2   d = EVec2 (Vec (Vec d))  deriving (Show)
newtype (UV.Unbox d) => EUVec  d =  EUVec (UVec d) deriving (Show)

---------------------------------------------------------------
-- Functor

-- Own Functor class which could swap containers
class EFunctor c1 c2 d1 d2 where
  emap :: (d1 -> d2) -> c1 d1 -> c2 d2 

-- Unbox
instance (UV.Unbox d2) => EFunctor EVal EUVec d1 d2  where   
  emap f (EVal x) = EUVec $ UV.fromList [f x]
  
instance (UV.Unbox d1, UV.Unbox d2) => EFunctor EUVec EUVec d1 d2  where   
  emap f (EUVec x) = EUVec $ UV.map f x

instance  (UV.Unbox d2) => EFunctor EVec EUVec d1 d2  where   
  emap f (EVec x) = EUVec $ UV.convert $ V.map f x

instance  (UV.Unbox d1) => EFunctor EUVec EVec d1 d2  where   
  emap f (EUVec x) = EVec $ V.map f $ UV.convert x

-- Equal to Equal
instance  EFunctor EVec EVec d1 d2  where   
  emap f (EVec x) = EVec $ V.map f x

instance  EFunctor EList EList d1 d2  where   
  emap f (EList x) = EList $ map f x

instance  EFunctor EVal EVal d1 d2 where
  emap f (EVal x) = EVal (f x)

instance  EFunctor EVec2 EVec2 d1 d2  where   
  emap f (EVec2 x) = EVec2 $ V.map (V.map f) x
  
instance  EFunctor EList2 EList2 d1 d2  where   
  emap f (EList2 x) = EList2 $ map (map f) x

---------------------------------------------------------------
-- ZipWith


-- Zip Classe
class EZipWith c1 c2 c3 d1 d2 d3 where -- | d1 d2 -> d3, c1 c2 -> c3 where 
  ezipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3 


-- Map Works  
         -- Unboxed
instance (EFunctor EUVec EUVec (d2 -> d3) d3, UV.Unbox d2, UV.Unbox d3) => EZipWith EVal EUVec EUVec d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  

instance (EFunctor EVec EVec (d2 -> d3) d3, UV.Unbox d3) => EZipWith EVal EVec EUVec d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  
  
instance (EFunctor EVec EVec (d2 -> d3) d3, UV.Unbox d2) => EZipWith EVal EUVec EVec d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  
  
  
  -- boxed
instance (EFunctor EVec EVec (d2 -> d3) d3) => EZipWith EVal EVec EVec d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  
  
instance (EFunctor EVec2 EVec2 (d2 -> d3) d3) => EZipWith EVal EVec2 EVec2 d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  

instance (EFunctor EList EList (d2 -> d3) d3) => EZipWith EVal EList EList d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  
  
instance (EFunctor EList2 EList2 (d2 -> d3) d3) => EZipWith EVal EList2 EList2 d1 d2 d3  where
  ezipWith f (EVal x) y = emap (f x) y  
 

-- zipWith with Length Check
m1 = "Error in EZipWith -- unequal length"

-- With each other -- zip Needed
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => EZipWith EUVec EUVec EUVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EUVec y) = if lCheck u v then EUVec $ UV.zipWith f x y else error m1 
  
instance (UV.Unbox d1, UV.Unbox d2) => EZipWith EUVec EUVec EVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EUVec y) = if lCheck u v then EVec $ V.zipWith f (V.convert x) (V.convert y) else error m1  
  
instance (UV.Unbox d1) => EZipWith EUVec EVec EVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EVec y) = if lCheck u v then EVec $ V.zipWith f (V.convert x) y  else error m1  

instance (UV.Unbox d2) => EZipWith EVec EUVec EVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EUVec y) = if lCheck u v then EVec $ V.zipWith f x (V.convert y)else error m1   

instance (UV.Unbox d3) => EZipWith EVec EVec EUVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith f x y  else error m1

instance (UV.Unbox d1,UV.Unbox d3) => EZipWith EUVec EVec EUVec d1 d2 d3  where
  ezipWith f u@(EUVec x) v@(EVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith f (V.convert x) y  else error m1

instance (UV.Unbox d2,UV.Unbox d3) => EZipWith EVec EUVec EUVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EUVec y) = if lCheck u v then EUVec $ UV.convert $ V.zipWith f x (V.convert y)  else error m1

-- boxed
instance EZipWith EVec EVec EVec d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EVec y) =  if lCheck u v then EVec $ V.zipWith f x y else error m1 

instance EZipWith EVec2 EVec2 EVec2 d1 d2 d3  where
  ezipWith f u@(EVec2 x) v@(EVec2 y) = if lCheck u v then EVec2 $ V.zipWith (V.zipWith f) x y else error m1 

instance EZipWith EList EList EList d1 d2 d3  where
  ezipWith f u@(EList x) v@(EList y) = if lCheck u v then EList $ zipWith f x y  else error m1

instance (Show (c1 d1) , Show (c2 d2), Show d1, Show d2) => EZipWith EList EList c3 d1 d2 d3  where
  ezipWith f u v = error ("Fehler2 : " ++ show u ++ show v)

instance EZipWith EList2 EList2 EList2 d1 d2 d3  where
  ezipWith f u@(EList2 x) v@(EList2 y) = if lCheck u v then EList2 $ zipWith (zipWith f) x y else error m1 

-- 1dim to 2dim
instance EZipWith EList EList2 EList2 d1 d2 d3  where
  ezipWith f u@(EList x) v@(EList2 y) = if lCheck u v then EList2 $ map (zipWith f x) y else error m1 
  
instance EZipWith EVec EVec2 EVec2 d1 d2 d3  where
  ezipWith f u@(EVec x) v@(EVec2 y) = if lCheck u v then EVec2 $ V.map (V.zipWith f x) y  else error m1

instance  (EZipWith c1 c2 c3 d1 d2 d3,Show (c1 d1) , Show (c2 d2))  => EZipWith c2 c1 c3 d2 d1 d3 where
   ezipWith f u v = error ("Fehler : " ++ show u ++ show v)

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

instance GetLength EList2 d where 
  len (EList2 x) = (length x,map length x)

instance GetLength EVec2 d where 
  len (EVec2 x) = (GV.length x, GV.toList $ GV.map GV.length x)
  
class  SameLength c1 c2 d1 d2 where
  lCheck :: c1 d1  -> c2 d2 -> Bool
  
instance  (GetLength c1 d1, GetLength c2 d2) => SameLength c1 c2 d1 d2 where 
  lCheck x y = len x == len x

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

instance EMonoid EVec2 EVec2 d where 
  eempty = EVec2 $ GV.fromList [GV.fromList[]] 
  (.++) (EVec2 x) (EVec2 y) = EVec2 (x GV.++ y)  

instance EMonoid EVec EVec2 d where 
  eempty = EVec2 $ GV.fromList [GV.fromList[]] 
  (.++) (EVec x) (EVec2 y) = EVec2 $ GV.map (GV.++x) y  

instance UV.Unbox d => EMonoid EUVec EUVec d where 
  eempty = EUVec $ UV.fromList [] 
  (.++) (EUVec x) (EUVec y) = EUVec (x UV.++ y)  
  
---------------------------------------------------------------
-- Vector Packing (Val und Vec2 von Vector Vector und [[]] nicht enthalten da problematisch)
  
class EC c1 c2 d where
  toEC :: c1 d -> c2 d  
  fromEC:: c2 d -> c1 d

instance EC [] EList d where
  toEC x = EList x  
  fromEC (EList x) = x
  
instance EC [] EVec d where
  toEC x = EVec (V.fromList x)  
  fromEC (EVec x) = V.toList x
  
-- instance EC List EVec2 d where
--   toEC x = EVec2 $ V.fromList (map V.fromList x)  
--   fromEC (EVec2 x) = map V.toList x
  
instance UV.Unbox d => EC [] EUVec d where
  toEC x = EUVec $ UV.fromList x  
  fromEC (EUVec x) = UV.toList x
   
instance UV.Unbox d => EC UVec EUVec d where
  toEC x = EUVec x  
  fromEC (EUVec x) = x

instance EC Vec EVec d where
  toEC x = EVec x  
  fromEC (EVec x) = x

--------------------------------------------------------------
-- Vector Conversion
  
class EConvert c1 c2 d where   
  econvert :: c1 d -> c2 d
  
instance (EC c1 c2 d, EConvert c1 c2 d) => EConvert c2 c1 d where

instance (UV.Unbox d) => EConvert EVec EUVec d where
   econvert (EVec x) = EUVec $ V.convert x 

instance (UV.Unbox d) => EConvert EUVec EVec d where
   econvert (EUVec x) = EVec $ UV.convert x 

instance EConvert EVec EList d where
  econvert (EVec x)  = EList $ V.toList x 

instance  (UV.Unbox d) => EConvert EUVec EList d where
  econvert (EUVec x)  = EList $ UV.toList x
  
instance EConvert EVec2 EList2 d where
  econvert (EVec2 x)  = EList2 $ V.toList $ V.map (V.toList) x
  
instance EConvert EList2 EVec2 d where
  econvert (EList2 x)  = EVec2 $ V.fromList $ map (V.fromList) x

---------------------------------------------------------------
-- Arith Funktionen  

class (EZipWith c1 c2 c3 d1 d2 d3, DMult d1 d2 d3) => CMult c1 c2 c3 d1 d2 d3 where
  (.*) :: c1 d1 -> c2 d2 -> c3 d3
  (./) :: c1 d1 -> c2 d2 -> c3 d3
  
instance (EZipWith c1 c2 c3 d1 d2 d3, DMult d1 d2 d3) => CMult c1 c2 c3 d1 d2 d3 where
  (.*) x y = ezipWith (.*.) x y
  (./) x y = ezipWith (./.) x y
 

class (EZipWith c1 c2 c3 d1 d2 d3, DSum d1 d2 d3) => CSum c1 c2 c3 d1 d2 d3 where
  (.+) :: c1 d1 -> c2 d2 -> c3 d3
  (.-) :: c1 d1 -> c2 d2 -> c3 d3
  (.+) x y = ezipWith (.+.) x y
  (.-) x y = ezipWith (.-.) x y

instance (EZipWith c1 c2 c3 d1 d2 d3, DSum d1 d2 d3) => CSum c1 c2 c3 d1 d2 d3 where
  (.+) x y = ezipWith (.+.) x y
  (.-) x y = ezipWith (.-.) x y

{-
class (EZipWith c1 c2 c3 d1 d2 d3, Eq d1,Ord d1, Ord d2 ) => CEq c1 c2 c3 d1 d2 d3 where
  (.==) :: c1 d1 -> c2 d1 -> c3 Bool
  (./=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.>=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.<=) :: c1 d1 -> c2 d1 -> c3 Bool
  (.>) :: c1 d1 -> c2 d1 -> c3 Bool
  (.<) :: c1 d1 -> c2 d1 -> c3 Bool
  
instance (EZipWith c1 c2 c3 d1 d2 d3, Eq d1,Ord d1, Ord d2, Show (c1 d1), Show (c2 d2), Show (c2 d1), Show (c1 d2)) => CEq c1 c2 c3 d1 d2 d3 where
  (.==)  x y = ezipWith (==) x y
  (./=)  x y = ezipWith (/=) x y
  (.>=)  x y = ezipWith (>=) x y
  (.<=)  x y = ezipWith (<=) x y
  (.>)  x y = ezipWith (>) x y
  (.<)  x y = ezipWith (<) x y
-}  



