{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs #-}

module EFA2.Signal.SignalData where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


newtype SampleIdx = SampleIdx Int deriving (Eq, Ord, Num, Enum, Show)

idxInc :: SampleIdx -> SampleIdx
idxInc (SampleIdx idx) = SampleIdx (idx+1) 

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

-- time step data
newtype DTSample = DTSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow data or storage content data -- TODO -- distringuish flow and storage of energy ?? (doubles all other sample types !!!)
newtype ESample = ESample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average power of energy flow
newtype FPSample = FPSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average efficiency of energy flow
newtype EEta = EEta Double
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



-----------------------------------------------------------------------------------
-- Class for Type-Safe Math Operations & Conversion Functions 

class (Sample a, Eta b) => SameUnit a b | a -> b, b -> a

-- instance SameUnit ESample EEta 
-- instance SameUnit ESample XSample
instance SameUnit EEta ESample 
instance SameUnit XSample ESample 

--instance SameUnit PSample PEta

-- instance SameUnit XSample ESample
-- --instance SameUnit YSample ESample
-- --instance SameUnit MSample ESample


class Sample a where
  fromSample :: a -> Double
  toSample :: Double -> a

instance Sample PSample where
  fromSample (PSample x) = x
  toSample x = PSample x

instance Sample ESample where
  fromSample (ESample x) = x
  toSample x = ESample x

instance Sample TSample where
  fromSample (TSample x) = x
  toSample x = TSample x

instance Sample DTSample where
  fromSample (DTSample x) = x
  toSample x = DTSample x



class Eta a where
  fromEta :: a -> Double
  toEta :: Double -> a

instance Eta EEta where
  fromEta (EEta x) = x
  toEta x = EEta x






