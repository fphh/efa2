{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures,IncoherentInstances, GeneralizedNewtypeDeriving,FlexibleContexts #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import Data.Monoid
import Control.Applicative

-- | Physical Types & Classification
data P
data DT
data E

newtype TypCon t c = TypCon c 

class TMult t1 t2 t3 | t1 t2 -> t2 where
instance TMult P DT E

----------------------------------------------------------
-- | 3. Numeric Data types
type Val = Double -- or Ratio
type IState = Int
type BState = Bool 
data Sign = PSign | ZSign | NSign deriving Show

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

----------------------------------------------------------
-- | 4. Data Structures
type Vec = V.Vector
type UVec = UV.Vector

newtype EVal   d = EVal d deriving (Show)
newtype EList   d = EList [d] deriving (Show) 
newtype EList2  d = EList2 [[d]]  deriving (Show)
newtype EVec    d = EVec (Vec d)  deriving (Show)
newtype EVec2   d = EVec2 (Vec (Vec d))  deriving (Show)
newtype (UV.Unbox d) => EUVec  d =  EUVec (UVec d) deriving (Show)

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


-- Zip Classe
class EZipWith c1 c2 c3 d1 d2 d3 | d1 d2 -> d3 where 
  ezipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3 

instance EZipWith c1 c2 c3 d1 d2 d3  => EZipWith c2 c1 c3 d2 d1 d3

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
 
-- map 1d to 2d 
-- instance (EFunctor EVec2 EVec2 (d2 -> d3) d3) => EZipWith EVec EVec2 EVec2 d1 d2 d3  where
--  ezipWith f (EVec x) y = emap (f x) y  
  
-- With each other -- zip Needed
instance (UV.Unbox d1, UV.Unbox d2, UV.Unbox d3) => EZipWith EUVec EUVec EUVec d1 d2 d3  where
  ezipWith f (EUVec x) (EUVec y) = EUVec $ UV.zipWith f x y  
  
instance (UV.Unbox d1, UV.Unbox d2) => EZipWith EUVec EUVec EVec d1 d2 d3  where
  ezipWith f (EUVec x) (EUVec y) = EVec $ V.zipWith f (V.convert x) (V.convert y)  
  
instance (UV.Unbox d1) => EZipWith EUVec EVec EVec d1 d2 d3  where
  ezipWith f (EUVec x) (EVec y) = EVec $ V.zipWith f (V.convert x) y  

instance (UV.Unbox d2) => EZipWith EVec EUVec EVec d1 d2 d3  where
  ezipWith f (EVec x) (EUVec y) = EVec $ V.zipWith f x (V.convert y)  

instance (UV.Unbox d3) => EZipWith EVec EVec EUVec d1 d2 d3  where
  ezipWith f (EVec x) (EVec y) = EUVec $ UV.convert $ V.zipWith f x y  

instance (UV.Unbox d1,UV.Unbox d3) => EZipWith EUVec EVec EUVec d1 d2 d3  where
  ezipWith f (EUVec x) (EVec y) = EUVec $ UV.convert $ V.zipWith f (V.convert x) y  

instance (UV.Unbox d2,UV.Unbox d3) => EZipWith EVec EUVec EUVec d1 d2 d3  where
  ezipWith f (EVec x) (EUVec y) = EUVec $ UV.convert $ V.zipWith f x (V.convert y)  

-- boxed
instance EZipWith EVec EVec EVec d1 d2 d3  where
  ezipWith f (EVec x) (EVec y) = EVec $ V.zipWith f x y  

instance EZipWith EVec2 EVec2 EVec2 d1 d2 d3  where
  ezipWith f (EVec2 x) (EVec2 y) = EVec2 $ V.zipWith (V.zipWith f) x y  

instance EZipWith EList EList EList d1 d2 d3  where
  ezipWith f (EList x) (EList y) = EList $ zipWith f x y  

instance EZipWith EList2 EList2 EList2 d1 d2 d3  where
  ezipWith f (EList2 x) (EList2 y) = EList2 $ zipWith (zipWith f) x y  

-- 1dim to 2dim
instance EZipWith EList EList2 EList2 d1 d2 d3  where
  ezipWith f (EList x) (EList2 y) = EList2 $ map (zipWith f x) y  
  
instance EZipWith EVec EVec2 EVec2 d1 d2 d3  where
  ezipWith f (EVec x) (EVec2 y) = EVec2 $ V.map (V.zipWith f x) y  

---------------------------------------------------------------
-- Arith Funktionen  

class (EZipWith c1 c2 c3 d1 d2 d3, DMult d1 d2 d3) => CMult c1 d1 c2 d2 c3 d3 where
  (.*) :: c1 d1 -> c2 d2 -> c3 d3
  (./) :: c1 d1 -> c2 d2 -> c3 d3
  (.*) x y = ezipWith (.*.) x y
  (.*) x y = ezipWith (./.) x y
  
instance (EZipWith c1 c2 c3 d1 d2 d3, DMult d1 d2 d3) => CMult c1 d1 c2 d2 c3 d3 


class (EZipWith c1 c2 c3 d1 d2 d3, DSum d1 d2 d3) => CSum c1 d1 c2 d2 c3 d3 where
  (.+) :: c1 d1 -> c2 d2 -> c3 d3
  (.-) :: c1 d1 -> c2 d2 -> c3 d3
  (.+) x y = ezipWith (.+.) x y
  (.-) x y = ezipWith (.-.) x y

instance (EZipWith c1 c2 c3 d1 d2 d3, DSum d1 d2 d3) => CSum c1 d1 c2 d2 c3 d3

sign :: Val -> Sign 
sign x | x == 0 = NSign
sign x | x > 0 = PSign
sign x | x < 0 = NSign


-- class CMult c1 d1 c2 d2 c3 d3 => Mult 

s1 = EUVec $ UV.fromList [0..1] :: EUVec Val
s2 = EUVec $ UV.fromList [0..3] :: EUVec Val
v1 = EVec $ V.fromList [0..1] :: EVec Val
v2 = EVec $ V.fromList [0..1] :: EVec Val

s3 = emap sign s1 :: EVec Sign
v4 = ezipWith (.*.) v1 v2 :: EVec Val

v5 = v1 .* v2 :: EVec Val


main = do 
  putStrLn (show s2)