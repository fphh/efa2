{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F



type UVec = UV.Vector Val
type Val = Double

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
-- Flow Signal
  
data FlowSignal = PSig (UVec) | NSig (UVec) | ESig (UVec) | DTSig (UVec) | XSig (UVec) | YSig (UVec) | MSig (UVec) deriving (Show, Eq, Ord)
data FlowSample = PSample | NSample | ESample | DTSample | XSample | YSample | MSample  deriving (Show, Eq, Ord)


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

s1 = PSig (UV.fromList [(0.5) , (0.3)]) 
s2 = DTSig (UV.fromList [(0.1) , (0.1)])


main = do
--  putStrLn $ show (p1.*n1)
  
  putStrLn $ show (s1*s2)

