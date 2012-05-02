{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances #-}


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

-- class defs
class QuickAppend a where
  (.++) :: [a] -> [a] -> [a]
  (.++) = (++)

instance QuickAppend (EList t c d)

instance Monoid (EList t c d) where 
  mempty = EList [] 
  mappend (EList l1) (EList l2) = EList (l1++l2)  

instance Functor (EList t c) where
  fmap f (EList l) = EList (map f l)

class SipWith f1 f2 f3 where
 sipWith :: (a -> b -> c) -> f1 a -> f2 b -> f3 c  
   
instance SipWith (EList t c) (EList t c) (EList t c) where   
  sipWith f (EList l1) (EList l2)  = EList (zipWith f l1 l2)
  
class DS_Unboxed s
instance DS_Unboxed (EUVec t c d)


----------------------------------------------------------
-- | 5. Big Calculation Classes 

class SMult t1 t2 t3 c1 c2 c3 d1 d2 d3 s1 s2 s3  where
  (~*) :: s1 t1 c1 d1 ->  s2 t2 c2 d2 ->  s3 t3 c3 d3
  
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
  