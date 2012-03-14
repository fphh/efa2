{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances  #-}

module EFA2.Signal.SignalData where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


newtype SampleIdx = SampleIdx Int deriving (Eq, Ord, Num, Enum, Show)

-- allow direct usage of SampleIndex
(.!) :: (UV.Unbox a) => UV.Vector a -> SampleIdx -> a
(.!) vec (SampleIdx idx) = vec UV.! idx

class Index a where
  fromIdx :: a -> Int 
  toIdx :: Int -> a

instance Index SampleIdx where
  fromIdx (SampleIdx x) = x
  toIdx x = SampleIdx x


-----------------------------------------------------------------------------------
-- Time Signal Samples

type Signal a =  (Show a, UV.Unbox a) => (UV.Vector a) 
--   deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

                    -- TODO automatic diriving doesn't work with type synonym Signal 

newtype TSample = TSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)
           
newtype PSample = PSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- newtype PEta = PEta Double
--   deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)
           
------------------------------------------------------------------------------------           
-- Flow Samples -- TODO -- differentiate flowsignal, histogram, energyflow ?
-- flowsignal - precise power values
-- histogram classes or clusters -- small variation of power 
-- energy flow values -- wide variation of power values 


-- type FlowVect a = (Show a, UV.Unbox a) => UV.Vector a -- TODO automatic deriving doesn't work with type Synonym FlowVect 

----------------------------------------------------------------------------
-- Sample Types

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
-- Samples Based around Time Signals

class Sample a where
  fromSample :: a -> Double
  toSample :: Double -> a

instance Sample TSample where
  fromSample (TSample x) = x
  toSample x = TSample x

instance Sample PSample where
  fromSample (PSample x) = x
  toSample x = PSample x

instance Sample PESample where
  fromSample (PESample x) = x
  toSample x = PESample x

instance Sample ESample where
  fromSample (ESample x) = x
  toSample x = ESample x


instance Sample DTSample where
  fromSample (DTSample x) = x
  toSample x = DTSample x

instance Sample NSample where
  fromSample (NSample x) = x
  toSample x = NSample x
  
instance Sample XSample where
  fromSample (XSample x) = x
  toSample x = XSample x

instance Sample YSample where
  fromSample (YSample x) = x
  toSample x = YSample x

instance Sample MSample where
  fromSample (MSample x) = x
  toSample x = MSample x
  
instance Sample Double where
  fromSample (x) = x
  toSample x = x


----------------------------------------------------------------------------
-- Typed Arithmetics

-- class UserDefined a b where
--   unpack :: a
--   pack :: a


class (Sample a, Num a, Fractional a) =>  TNum a b c | a b -> c where
--  type ResType a b
  (.+.) :: a -> b -> c
  (.-.) :: a -> b -> c
  (.*.) :: a -> b -> c
  (./.) :: a -> b -> c
  

----------------------------------------------------------------------------
-- Each Type with Itself

instance  (Sample a,  Num a, Fractional a) => TNum a a a where
--  type ResType a a  = a
  (.+.) x y = toSample $ (fromSample x) + (fromSample y)
  (.-.) x y = toSample $ (fromSample x) + (fromSample y)

----------------------------------------------------------------------------
-- Mixed Arithmetics -- Sample & Double

instance (Sample a,  Num a, Fractional a) => TNum a Double a where
--  type ResType a Double = a
  (.+.) x y = toSample $ (fromSample x) + y
  (.-.) x y = toSample $ (fromSample x) + y
  (.*.) x y = toSample $ (fromSample x) + y
  (./.) x y = toSample $ (fromSample x) + y
  
instance (Sample a,  Num a, Fractional a) => TNum Double a a where
--  type ResType Double a = a
  (.+.) y x = toSample $ (fromSample x) + y
  (.-.) y x = toSample $ (fromSample x) + y
  (.*.) y x = toSample $ (fromSample x) + y
  (./.) y x = toSample $ (fromSample x) + y


----------------------------------------------------------------------------
-- Mixed Arithmetics -- One Type Dominant
  
instance  (Sample a,  Num a, Fractional a, Sample b,  Num b, Fractional b) => TNum a b a where
--  type ResType a b  = a
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) + (fromSample y)


----------------------------------------------------------------------------
-- Mixed Arithmetics -- One Type Dominant
  
-- instance  (Sample a,  Num a, Fractional a, Sample b,  Num b, Fractional b) => TNum a b c where
-- --  type ResType ESample XSample = ESample
--   (.*.) x y = toSample $ (fromSample x) + (fromSample y)
--   (./.) x y = toSample $ (fromSample x) + (fromSample y)

instance  TNum ESample YSample ESample where
--  type ResType ESample XSample = ESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) + (fromSample y)





class (Sample a, Sample b) => SampleMath a b where 
  (!*) :: a -> b -> a
  (!/) :: a -> b -> a
  
-- Calculate over efficiencies / devider- and collector and mix-ratios
instance SampleMath ESample NSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))
  
instance SampleMath ESample XSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))
 
instance SampleMath ESample YSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))

instance SampleMath ESample MSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))

-- Calculate over efficiencies / devider- and collector and mix-ratios
instance SampleMath PESample NSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))
  
instance SampleMath PESample XSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))
 
instance SampleMath PESample YSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))

instance SampleMath PESample MSample where  
  (!*) e x = toSample ((fromSample e) * (fromSample x))
  (!/) e x = toSample ((fromSample e) / (fromSample x))

----------------------------------------------------------------------------
-- Mixed Arithmetics -- With new Type c

class (Sample a, Sample b, Sample c) => SampleMathConv a b c where 
  (§*) :: a -> b -> c
  (§/) :: a -> b -> c

-- Power * TimeDuration = Energy
instance SampleMathConv PESample DTSample ESample where  
  (§*) e x = toSample ((fromSample e) * (fromSample x))
  
-- Power = Energy / Time Duration
instance SampleMathConv ESample DTSample PESample where  
  (§/) e x = toSample ((fromSample e) / (fromSample x))
 
-- Time / Delta Time
instance SampleMathConv TSample TSample DTSample where  
  (§/) t1 t2 = toSample ((fromSample t1) - (fromSample t2))


----------------------------------------------------------------------------
-- Integration
integrateSample :: PSample -> PSample -> DTSample -> ESample
integrateSample (PSample ps1) (PSample ps2) (DTSample dt) = toSample ((ps1+ps2)/2*dt)               

               






