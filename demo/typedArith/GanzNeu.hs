{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures #-}


import qualified Data.Vector as GV
import qualified Data.Vector.Unboxed as UV

import Data.Monoid
import Control.Applicative
-- import Control.Functor.Zip

--------------------------------

-- Hierarchy and Structure below
-- 1. t / T = EFA Types 
-- 2. c / C = EFA Data Containers
-- 3. d / D = Data type
-- 4. s / S = Underlying Data Structures
-- 5. Big Calculation Classes

----------------------------------------------------------
-- | 1. EFA Types
-- data EType d t p c
--d = delta
--t = type 
--p = partial
--c = composition 

-- | Physical Types & Classification
data P
data DT
data E
data Z
data IZ
data UZ

class ET_Not_Num t
instance ET_Not_Num IZ
instance ET_Not_Num UZ

class ET_Unboxed t
instance ET_Unboxed UZ

class TMult t1 t2 t3 | t1 t2 -> t2 where
instance TMult P DT E

----------------------------------------------------------
-- | 2. EFA data containers

-- data ECont d c 
-- d = derivative
-- c = container
data Value 
data Signal 
data Distrib
data MultiValue
data MultiSignal 
data MultiDistrib 

-- classify data Structures 
class EC_Not_Value c
instance EC_Not_Value Signal
instance EC_Not_Value Distrib
instance EC_Not_Value MultiValue
instance EC_Not_Value MultiDistrib

class EC_OneDim c
instance EC_OneDim Signal
instance EC_OneDim Distrib

class EC_TwoDim c
instance EC_TwoDim MultiSignal
instance EC_TwoDim MultiDistrib

class CMult c1 c2 c3 | c1 c2 -> c3 where

instance CMult c1 Value c1
instance CMult c1 c1 c1

----------------------------------------------------------
-- | 3. Numeric Data types
type Val = Double -- or Ratio
type IState = Int
type BState = Bool 
data Sign = PSign | ZSign | NSign 

class DMult d1 d2 d3 | d1 d2 -> d3 where
 (.*) :: d1 -> d2 -> d3
 (./) :: d1 -> d2 -> d3
 
instance DMult Val Val Val where
 (.*) x y = x*y
 (./) 0 0 = 0
 (./) x y = x/y

instance DMult Val Bool Val where
 (.*) x True = x
 (.*) x False = 0
 (./) x False = 0
 (./) x True = x
 
instance DMult Bool Bool Bool where
 (.*) x True = x
 (.*) x False = False
 (./) x False = False
 (./) x True = x

----------------------------------------------------------
-- | 4. Data Structures
type Vec = GV.Vector
type UVec = UV.Vector

newtype EVal t c d = EVal d deriving (Show)
newtype EList  t c d = EList [d] deriving (Show) 
newtype EList2 t c d = EList2 [[d]]  deriving (Show)
newtype EVec  t c d = EVec (Vec d)  deriving (Show)
newtype EVec2  t c d = EVec2 (Vec (Vec d))  deriving (Show)
newtype EUVec  t c d = EUVec (UVec d) deriving (Show)

class ZeroDim a where
instance ZeroDim (EVal t c d)

class OneDim a where
instance OneDim (EList t c d)
instance OneDim (EVec t c d)
instance OneDim (EUVec t c d)

class TwoDim a where
instance OneDim (EList2 t c d)
instance OneDim (EVec2 t c d)
  
class Unboxed a where
instance Unboxed (EUVec t c d)

class Boxed a where
instance Boxed (EVec t c d)
instance Boxed (EVec2 t c d)

-- class defs
class QuickAppend a where
  (.++) :: [a] -> [a] -> [a]
  (.++) = (++)

instance QuickAppend (EList t c d)

instance Monoid (EList t c d) where 
  mempty = EList [] 
  mappend (EList l1) (EList l2) = EList (l1++l2)  

instance Monoid (EVec t c d) where 
  mempty = EVec $ GV.fromList [] 
  mappend (EVec l1) (EList l2) = EList (l1 GV.++ l2)  

instance Monoid (EUVec t c d) where 
  mempty = EVec $ UV.fromList [] 
  mappend (EVec l1) (EList l2) = EList (l1 UV.++ l2)  

-- Functor 
class EFunctor (f:: * -> *) d1 d2 where
  emap :: (d1 -> d2)  -> d1 -> d2 

-- Val 
instance EFunctor (EVal t c) d1 d2 where
  emap f (EVal x) = EVal (f x)

-- keep List 
instance EFunctor (EList t c) d1 d2 where
  emap f (EList x) = EList (map f x)
  
-- convert to unboxed Vector
instance (UV.Unbox d2) => EFunctor (EVec t c) d1 d2 where
  emap f (EVec x) = EUVec GV.convert $ GV.map f x

-- map over boxed Vector
instance EFunctor (EVec t c) d1 d2 where
  emap f (EVec x) = EVec (GV.map f x)

-- keep Unboxed Vector
instance (UV.Unbox d2) => EFunctor (EUVec t c) d1 d2 where
  emap f (EUVec x) = EUVec (UV.map f x)
  
-- convert to boxed Vector  
instance EFunctor (EUVec t c) d1 d2 where
  emap f (EUVec x) = EVec GV.convert (UV.map f x)

-- Map over 2dim List
instance EFunctor (EList2 t c) d1 d2 where
  emap f (EList2 x) = EList2 (map (map f) x)

-- Map over 2dim Vector
instance EFunctor (EVec2 t c) d1 d2 where
  emap f (EVec2 x) = EVec2 (GV.map (GV.map f) x)

-- | specific zipWith
class SipWith d1 d2 d3 s1 s2 s3 where
  sipWith :: (d1 -> d2 -> d3) -> s1 a -> s2 b -> s3 c  
   
-- | Rotate Arguments
instance (SipWith d1 d2 d3 s1 s2 s3) =>  SipWith d1 d2 d3 s2 s1 s3 

-- | Zero Dimensional against anything
instance (ZeroDim d1) => SipWith d1 d2 d3 s1 s2 s3 where
   sipWith f x y = emap (f x) y   
   
-- | One Dimensional against Two Dim
instance (OneDim d1, TwoDim d1)  => SipWith d1 d2 d3 s1 s2 s3 where
   sipWith f x y = if lCheck then emap (f x) y else "Error in sipWith - different Length"  

-- | 1d - Lists against each other
instance SipWith d1 d2 d3 EList EList EList where
   sipWith f (EList x) (EList y) = if lCheck then EList zipWith f x y else "Error in sipWith - different Length"  

-- | 2d - Lists against each other
instance SipWith d1 d2 d3 EList2 EList2 EList2 where
   sipWith f (EList x) (EList y) = if lCheck then EList zipWith (zipWith f) x y else "Error in sipWith - different Length"  

-- | Vectors with unboxed outcome
instance (UV.Unbox d3) => SipWith d1 d2 d3 EVec EVec s3 where
   sipWith f (EVec x) (EVec y) = if lCheck then  GV.convert $ GV.zipWith f x y else "Error in sipWith - different Length"  

instance (UV.Unbox d3) => SipWith d1 d2 d3 EUVec EUVec s3 where
   sipWith f (EUVec x) (EVec y) = if lCheck then  UV.zipWith f x y else "Error in sipWith - different Length"  

instance (UV.Unbox d3) => SipWith d1 d2 d3 EVec EUVec s3 where
   sipWith f (EUVec x) (EVec y) = if lCheck then  GV.convert $ GV.zipWith f x (GV.convert y) else "Error in sipWith - different Length"  

-- | Vectors with boxed outcome
instance SipWith d1 d2 d3 EVec EVec s3 where
   sipWith f (EVec x) (EVec y) = if lCheck then  GV.zipWith f x y else "Error in sipWith - different Length"  

instance SipWith d1 d2 d3 EUVec EUVec s3 where
   sipWith f (EUVec x) (EVec y) = if lCheck then  GV.zipWith f (GV.convert x) (GV.convert y) else "Error in sipWith - different Length"  

instance SipWith d1 d2 d3 EVec EUVec s3 where
   sipWith f (EUVec x) (EVec y) = if lCheck then  GV.convert $ GV.zipWith f x (GV.convert y) else "Error in sipWith - different Length"  


class GetLength a where
  len :: a -> Int
  
instance GetLength EVal where 
  len x = 1

instance GetLength EList where 
  len x = length x

instance GetLength EVec where 
  len x = GV.length x

instance GetLength EUVec where 
  len x = UV.length x


class SameLength a b where
  lCheck :: a -> b -> Bool
  
instance (SameLength a b) => SameLength b a 

instance (ZeroDim a) => SameLength a b where
  lCheck x y = True
  
instance (OneDim a, OneDim b) =>  SameLength a b where 
  lCheck x y = len x == len y
  
instance (TwoDim a, TwoDim b, Functor a, Functor b) => SameLength a b where 
  lCheck x y = len x == len y && all $ (map len x) == all (map len y)  
  


----------------------------------------------------------
-- | 5. Big Calculation Classes 
{-
class (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3,SipWith d1 d2 d3 s1 s2 s3) => SMult t1 t2 t3 c1 c2 c3 d1 d2 d3 (s1 :: * -> * -> * )  (s2 :: * -> * -> * ) (s3 :: * -> * -> * ) where
  (~*) :: s1 t1 c1 d1 -> s2 t2 c2 d2 -> s3 t3 c3 d3
  (~*) x y = sipWith (.*) x y 
-}    
    
class (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3 c1 c2 c3 d1 d2 d3 s1 s2 s3 where
  (~*) :: s1 t1 c1 d1 -> s2 t2 c2 d2 -> s3 t3 c3 d3


instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3   d1 d2 d3  EList EList EList where
  (~*) (EList x) (EList y) = EList $ zipWith (.*) x y

instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EList EVal EList where
  (~*) (EList x) (EVal y) = EList $ map (.*y) x

instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EList EList where
  (~*) (EVal x) (EList y) = EList $ map (.*x) y

instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EVal EVal where
  (~*) (EVal x) (EVal y) = EVal x.*y


instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EVal EVal where
  (~*) (EUVec x) (EUVec y) = EUVec UV.zipWith (.*) x y

instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3, UV.Unbox d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EVal EVal where
  (~*) (EVal x) (EUVec y) = EUVec UV.map (.*x) y
  
instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EVal EVal where
  (~*) (EVal x) (EVec y) = EUVec GV.map (.*x) y

instance  (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3) => SMult t1 t2 t3  c1 c2 c3  d1 d2 d3  EVal EVal EVal where
  (~*) (EVal x) (EVec y) = EUVec GV.map (.*x) y


-- lists % Vectors of different data types
l1 = [0.3,0.5]                                                  
l2 = [0.2,0.8]                                                 

s1 = EList l1 ::   EList P Signal Val
s2 = EList l2 ::   EList DT Signal Val  
s3 = s1~*s2  ::  EList E Signal Val
  
main = do 
  putStrLn (show s1) 
  