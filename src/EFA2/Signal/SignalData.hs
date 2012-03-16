{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances  #-}

module EFA2.Signal.SignalData where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


----------------------------------------------------------------------------------
-- Basic Data Type

type Val = Double

----------------------------------------------------------------------------------
-- Vector Usage

type UVec a = (Show a, UV.Unbox a) => UV.Vector a

-- diffVect :: Eq a => UVec a -> UVec a
-- diffVect v = (UV.tail v) - (UV.init v)

dzipWith :: (a -> a -> a ) -> UVec a -> UVec a
dzipWith f v = UV.zipWith f (UV.tail v) (UV.init v)

-- instance (UV.Unbox a, Show a, Eq a, Num a) => Num (UV.Vector a) where
--   (+) v1 v2 = UV.zipWith (+) v1 v2
--   (-) v1 v2 = UV.zipWith (-) v1 v2
--   (*) v1 v2 = UV.zipWith (*) v1 v2
--   abs v1  = UV.map abs v1
--   signum v1  = UV.map signum v1
-- --  fromInteger v1 = UV.map fromInteger v1

-- instance (UV.Unbox a, Show a, Eq a, Fractional a) => Fractional (UV.Vector a) where
--   (/) v1 v2 = UV.zipWith (/) v1 v2

----------------------------------------------------------------------------
-- Typed Data Samples

newtype PSample = PSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype TSample = TSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- time step data
newtype DTSample = DTSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow data or storage content data -- TODO -- distringuish flow and storage of energy ?? (doubles all other sample types !!!)
newtype ESample = ESample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average power of energy flow
newtype PESample = PESample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average efficiency of energy flow
newtype NSample = NSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow mix data
newtype MSample = MSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow Split Share Data
newtype XSample = XSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow collection Share Data
newtype YSample = YSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

----------------------------------------------------------------------------
-- Data Access Funtions for Typed Samples

class  (Num a, Fractional a, UV.Unbox a) => Sample a where fromSample :: a -> Double; toSample :: Double -> a

instance Sample TSample where fromSample (TSample x) = x; toSample x = TSample x
instance Sample PSample where fromSample (PSample x) = x; toSample x = PSample x
instance Sample PESample where fromSample (PESample x) = x; toSample x = PESample x
instance Sample ESample where fromSample (ESample x) = x; toSample x = ESample x
instance Sample DTSample where fromSample (DTSample x) = x; toSample x = DTSample x
instance Sample NSample where fromSample (NSample x) = x; toSample x = NSample x
instance Sample XSample where fromSample (XSample x) = x; toSample x = XSample x
instance Sample YSample where fromSample (YSample x) = x; toSample x = YSample x
instance Sample MSample where fromSample (MSample x) = x; toSample x = MSample x
-- also for Double !!
instance Sample Double where  fromSample (x) = x; toSample x = x

----------------------------------------------------------------------------
-- Typed Arithmetics for Samples

class (Sample a, Sample b, Sample c) =>  SampleSum a b c | a b -> c where
  (.+) :: a -> b -> c
  (.+) x y = toSample (fromSample x + fromSample y)   
  
  (.-) :: a -> b -> c
  (.-) x y = toSample (fromSample x - fromSample y)   

instance  SampleSum PESample PESample PESample
instance  SampleSum PSample PSample PSample


class (Sample a, Sample b, Sample c) =>  SampleMult a b c | a b -> c where
  (.*) :: a -> b -> c
  (.*) x y = toSample (fromSample x * fromSample y)   
  
instance  SampleMult PESample NSample PESample
instance  SampleMult PESample XSample PESample
instance  SampleMult PESample YSample PESample
instance  SampleMult PESample MSample PESample

instance  SampleMult ESample NSample ESample
instance  SampleMult ESample XSample ESample
instance  SampleMult ESample YSample ESample
instance  SampleMult ESample MSample ESample

instance  SampleMult PESample DTSample ESample


class (Sample a, Sample b, Sample c) =>  SampleDiv a b c | a b -> c where
  (./) :: a -> b -> c
  (./) x y = toSample (fromSample x / fromSample y)   
  
instance  SampleDiv ESample DTSample PESample

----------------------------------------------------------------------------
-- Data Containers

data Signal a = Signal (UVec a) -- Time signal
data FSignal a = FSignal (UVec a) -- Flow Signal 
-- data FDistrib a = (Show a, UV.Unbox a) => FDistrib (UVec a) -- Flow Distribution
-- data MixFlow a = (Show a) => MixFlow (UVec a) -- Vector of Partial Flows
data Flow a = Flow (UVec a) -- Single Flow Value

----------------------------------------------------------------------------
-- Data Container Access

class Data d a where fromData :: d -> UVec a; toData :: UVec a -> d

instance (UV.Unbox a, Show a) => Data (Signal a) a where fromData (Signal x) = x; toData x = Signal x
instance (UV.Unbox a, Show a) => Data (FSignal a) a where fromData (FSignal x) = x; toData x = FSignal x
instance (UV.Unbox a, Show a) => Data (Flow a) a where fromData (Flow x) = x; toData x = Flow x

----------------------------------------------------------------------------
-- Data Container Arithmetic for given Container Combinations

-- Zip two signals ord distributions
class (SampleMult a b c) => SigProd a b c where
 (*.) :: a -> b ->  c
 
instance (SampleMult a b c) => SigProd a b c where
 (*.) (Signal v1)  (Signal v2) = (Signal (v1 * v2)) 


--------------------------------------------------------------------------------------------
-- Arithmetics Super Class

class (Data x a, Data y b, Data z c) => Sum x a y b z c where
 (~+) :: x -> y ->  z
 (~-) :: x -> y ->  z

class (Data x a, Data y b, Data z c) => Prod x a y b z c where
 (~*) :: x -> y ->  z

instance (SigProd a b c) => Prod (Signal a) a (Signal b) b (Signal c) c where (~*) x y = x .* y 


class (Data x a, Data y b, Data z c) => Div x a y b z c where
 (~/) :: x -> y ->  z


----------------------------------------------------------------------------
-- Integration
integrateSample :: PSample -> PSample -> DTSample -> ESample
integrateSample (PSample ps1) (PSample ps2) (DTSample dt) = toSample ((ps1+ps2)/2*dt)               

               






