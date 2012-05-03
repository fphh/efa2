{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures,IncoherentInstances #-}


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
  mappend (EList x) (EList y) = EList (x++y)  

instance Monoid (EVec t c d) where 
  mempty = EVec $ GV.fromList [] 
  mappend (EVec x) (EVec y) = EVec (x GV.++ y)  

instance UV.Unbox d => Monoid (EUVec t c d) where 
  mempty = EUVec $ UV.fromList [] 
  mappend (EUVec x) (EUVec y) = EUVec (x UV.++ y)  



-- Val 
instance Functor (EVal t c) where
  fmap f (EVal x) = EVal (f x)

-- keep List 
instance Functor (EList t c) where
  fmap f (EList x) = EList (map f x)
  
-- map over boxed Vector
instance Functor (EVec t c) where
  fmap f (EVec x) = EVec (GV.map f x)

-- -- keep Unboxed Vector
-- instance  Functor (EUVec t c) where
--   fmap f (EUVec x) =  EUVec (UV.map f x)
  
-- Map over 2dim List
instance Functor (EList2 t c)  where
  fmap f (EList2 x) = EList2 (map (map f) x)

-- Map over 2dim Vector
instance Functor (EVec2 t c) where
  fmap f (EVec2 x) = EVec2 (GV.map (GV.map f) x)



-- | specific zipWith
class  SipWith f g where
  sipWith :: (d1 -> d2 -> d3)  -> f d1 -> g d2 -> g d3 

-- | rotate
instance SipWith f g => SipWith g f

-- | Value
instance SipWith (EVal t c) (EVal t c) where
  sipWith f (EVal x) (EVal y) = EVal (f x y)

instance SipWith (EVal t c) (EList t c) where
  sipWith f (EVal x) (EList y) = EList $ map (f x) y

instance SipWith (EVal t c) (EList2 t c) where
  sipWith f (EVal x) (EList2 y) = EList2 $ map (map (f x)) y

instance SipWith (EVal t c) (EVec t c) where
  sipWith f (EVal x) (EVec y) = EVec $ GV.map (f x) y 

-- instance SipWith (EVal t c) (EUVec t c) where
--   sipWith f (EVal x) (EUVec y) = EUVec $ UV.map (f x) y 
  
instance SipWith (EVal t c) (EVec2 t c) where
  sipWith f (EVal x) (EVec2 y) = EVec2 $ GV.map (GV.map (f x)) y 

instance SipWith (EList t c) (EList t c) where
   sipWith f (EList x) (EList y) =  if lCheck x y then EList $ zipWith f x y else error "Error in sipWith - different Length"

instance  SipWith (EVec t c) (EVec t c) where
   sipWith f (EVec x) (EVec y) =  if lCheck x y then EVec $ GV.zipWith f x y else error "Error in sipWith - different Length"

-- instance  SipWith (EUVec t c) (EUVec t c) where
--    sipWith f (EUVec x) (EUVec y) =  if lCheck x y then EUVec $ UV.zipWith f x y else error "Error in sipWith - different Length"

instance  SipWith (EList2 t c) (EList2 t c) where
   sipWith f (EList2 x) (EList2 y) =  if lCheck x y then EList2 $ zipWith (zipWith f) x y else error "Error in sipWith - different Length"

instance  SipWith (EVec2 t c) (EVec2 t c) where
   sipWith f (EVec2 x) (EVec2 y) =  if lCheck x y then EVec2 $ GV.zipWith (GV.zipWith f) x y else error "Error in sipWith - different Length"
  
class GetLength a where
  len :: a -> (Int,[Int])
  

instance GetLength (GV.Vector a)  where 
  len x = (GV.length x,[])

-- instance GetLength (UV.Vector a)  where 
--   len x = (UV.length x,[])

instance GetLength [[a]] where 
  len x = (length x,map length x)

instance GetLength (GV.Vector (GV.Vector a)) where 
  len x = (GV.length x, GV.toList $ GV.map GV.length x)
  
instance GetLength [a] where 
  len x = (length x,[])
  
instance GetLength a where 
  len x = (1,[])

class  SameLength a b where
  lCheck :: a -> b -> Bool
  
instance  (GetLength a, GetLength b) => SameLength a b where 
  lCheck x y = len x == len x


----------------------------------------------------------
-- | 5. Big Calculation Classes 

class (TMult t1 t2 t3, CMult c1 c2 c3, DMult d1 d2 d3, SipWith (s1 t1 c1) (s2 t2 c2)) => SMult t1 t2 t3 c1 c2 c3 d1 d2 d3 s1 s2 s3 where
  (~*) :: s1 t1 c1 d1 -> s2 t2 c2 d2 -> s3 t3 c3 d3
  (~*) x y = convert $ sipWith (.*) x y 
    
    
{-
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
-}

-- lists % Vectors of different data types
l1 = [0.3,0.5]                                                  
l2 = [0.2,0.8]                                                 

s1 = EList l1 ::   EList P Signal Val
s2 = EList l2 ::   EList DT Signal Val  
s3 = s1~*s2  ::  EList E Signal Val
  
main = do 
  putStrLn (show s1) 
  