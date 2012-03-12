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

instance ValNum UVec where
  (.+) v1 val = UV.map (+val) v1
  (.-) v1 val = UV.map (+(negate val)) v1
  (.*) v1 val = UV.map (*val) v1
  (./) v1 val = UV.map (/val) v1

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
-- Time Signal -- (original Measurement data)

data TimeSignal = PTSig UVec deriving (Show, Eq) -- more signals can be added if needed like for flow signals
data Time = Time UVec deriving (Show, Eq)
  
genDTime (Time v) = DTSig (diffVect v)

integratePartial (PTSig v1) (Time v2) =  ESig $ (dzipWith f v1) / diffVect v2 
  where f p1 p2 = 0.5 * (p1+p2)
    
-- Class to allow Scaling and shifting using Double
class ValNum a where  
  (.+) :: a -> Val -> a
  (.-) :: a -> Val -> a  
  (.*) :: a -> Val -> a
  (./) :: a -> Val -> a
  
instance ValNum TimeSignal where  
  (.+) sig  v = smap (+v) sig
  (.-) sig  v = smap (+(negate v)) sig
  (.*) sig  v = smap (*v) sig
  (./) sig  v = smap (/v) sig
  
smap :: (Val -> Val) -> TimeSignal -> TimeSignal 
smap f (PTSig vect) = PTSig (UV.map f vect)   


instance Num TimeSignal where
  abs s  = smap abs s
  signum s = smap signum s


-----------------------------------------------------------------------------------
-- Flow Vals -- TODO

-----------------------------------------------------------------------------------
-- Flow Distributions - TODO

-----------------------------------------------------------------------------------
-- Flow Signal
  
data FlowSignal = PSig (UVec) | NSig (UVec) | ESig (UVec) | DTSig (UVec) | XSig (UVec) | YSig (UVec) | MSig (UVec) deriving (Show, Eq, Ord)

instance Num FlowSignal where
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

instance Fractional FlowSignal where
  (/) (ESig v1) (DTSig v2) = PSig (v1/v2)   -- Energy = Power * DTime
  
  -- Devide EnergyFlow by Ratios
  (/) (ESig v1) (NSig v2) = ESig (v1/v2) -- Multiply with Efficiency  
  (/) (ESig v1) (XSig v2) = ESig (v1/v2) -- Multiply with Splitter
  (/) (ESig v1) (YSig v2) = ESig (v1/v2) -- Multiply with Devider
  (/) (ESig v1) (MSig v2) = ESig (v1/v2) -- Multiply with Mix-Signal
  
  
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

instance UVecBased FlowSignal where
  getVect (PSig v) = v
  getVect (NSig v) = v
  getVect (ESig v) = v
  getVect (DTSig v) = v
  getVect (XSig v) = v
  getVect (YSig v) = v
  getVect (MSig v) = v

instance UVecBased TimeSignal where
  getVect (PTSig v) = v



instance Indexible FlowSignal FSigIdx where    
instance Indexible TimeSignal TSigIdx where    

  
  
  


  