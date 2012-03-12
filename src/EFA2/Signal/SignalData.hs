{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

module EFA2.Signal.SignalData where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV


-----------------------------------------------------------------------------------
-- Easy Vector access and Calculations
  
type Val = Double
type UVec = UV.Vector Val  

diffVect :: UVec -> UVec
diffVect v = (UV.tail v) - (UV.init v)

dzipWith :: (Val -> Val -> Val ) -> UVec -> UVec
dzipWith f v = UV.zipWith f (UV.tail v) (UV.init v)

instance Num UVec where
  (+) v1 v2 = UV.zipWith (+) v1 v2
  (-) v1 v2 = UV.zipWith (-) v1 v2
  (*) v1 v2 = UV.zipWith (*) v1 v2
  abs v1  = UV.map abs v1
  signum v1  = UV.map signum v1
--  fromInteger v1 = UV.map fromInteger v1

instance Fractional UVec where
  (/) v1 v2 = UV.zipWith (/) v1 v2

-----------------------------------------------------------------------------------
-- Time Signal Samples

data TSignal = TSig UVec | PTSig UVec 

genDTime (TSig v) = DTSig (diffVect v)

integratePartial (PTSig v1) (TSig v2) =  ESig $ (dzipWith f v1) / diffVect v2 
  where f p1 p2 = 0.5 * (p1+p2)
    
  
-----------------------------------------------------------------------------------
-- Signal Indexing

data TSigIdx = TSigIdx Int deriving (Show)
data FSigIdx  = FSigIdx Int deriving (Show) 

class Index a where
  fromIdx :: a -> Int
  
class UVecBased a where
  getVect :: a -> UVec 

class (Index b, UVecBased a, Show b) => Indexible a b  | a -> b , b-> a where
  (!) :: a -> b -> Val 
  (!) v i = maybe (error $ "Error in Elem - index out of Range :" ++ show i) id ((getVect v) UV.!? (fromIdx i)) 


instance Index TSigIdx where
  fromIdx (TSigIdx x) = x
  
instance Index FSigIdx where
  fromIdx (FSigIdx x) = x

instance UVecBased FSignal where
  getVect (PSig v) = v
  getVect (NSig v) = v
  getVect (ESig v) = v
  getVect (DTSig v) = v
  getVect (XSig v) = v
  getVect (YSig v) = v
  getVect (MSig v) = v

instance UVecBased TSignal where
  getVect (PTSig v) = v
  getVect (TSig v) = v


instance Indexible FSignal FSigIdx where    
instance Indexible TSignal TSigIdx where    


-----------------------------------------------------------------------------------
-- Flow Signal
  
data FSignal = PSig (UVec) | NSig (UVec) | ESig (UVec) | DTSig (UVec) | XSig (UVec) | YSig (UVec) | MSig (UVec) deriving (Show, Eq, Ord)

instance Num FSignal where
  (*) (PSig v1) (DTSig v2) = ESig (v1*v2)   -- Energy = Power * DTime
  
  -- Multiply Energy by ratios from right
  (*) (ESig v1) (NSig v2) = ESig (v1*v2) -- Multiply with Efficiency  
  (*) (ESig v1) (XSig v2) = ESig (v1*v2) -- Multiply with Splitter
  (*) (ESig v1) (YSig v2) = ESig (v1*v2) -- Multiply with Devider
  (*) (ESig v1) (MSig v2) = ESig (v1*v2) -- Multiply with Mix-Signal
  
  -- Multiply Energy by ratios from right
  (*) (NSig v1) (ESig v2) = ESig (v1*v2) -- Multiply with Efficiency  
  (*) (XSig v1) (ESig v2) = ESig (v1*v2) -- Multiply with Splitter
  (*) (YSig v1) (ESig v2) = ESig (v1*v2) -- Multiply with Devider
  (*) (MSig v1) (ESig v2) = ESig (v1*v2) -- Multiply with Mix-Signal
  
  (*) a b = error ("Error in Num FSignal - case for * not implemented " ++ show a ++ " " ++ show b)  

instance Fractional FSignal where
  (/) (ESig v1) (DTSig v2) = PSig (v1/v2)   -- Energy = Power * DTime
  
  -- Devide EnergyFlow by Ratios
  (/) (ESig v1) (NSig v2) = ESig (v1/v2) -- Multiply with Efficiency  
  (/) (ESig v1) (XSig v2) = ESig (v1/v2) -- Multiply with Splitter
  (/) (ESig v1) (YSig v2) = ESig (v1/v2) -- Multiply with Devider
  (/) (ESig v1) (MSig v2) = ESig (v1/v2) -- Multiply with Mix-Signal
  
  


  