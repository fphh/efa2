{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies  #-}

module EFA2.Signal.SignalVersuch where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

{-
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

data Signal a = (Show a, UV.Unbox a, Sample a, Eq a) => Signal (UV.Vector a)

smap ::  (a-> b) -> Signal a -> Signal b
smap f Signal (v))  = Signal (UV.map f v))

sipWith :: (a->b->c) -> Signal a -> Signal b -> Signal c
sipWith f Signal (v1) Signal (v2) = UV.zipWith v1 v2 

(/*/) :: Signal a -> Signal b -> Signal c
(/*/) s1 s2 = sipWith (.*.) s1 s2

         
-- class SigNum a b where
--   (*.) :: (Signal a) -> (Signal b) -> (Signal (ResType a b)) 

-- instance Eq (Signal a) where
--   (==) (Signal v1) (Signal v2) = v1 == v2
--   (/=) (Signal v1) (Signal v2) = v1 /= v2
  
-- instance SigNum a b where 
--   (*.) (Signal v1) (Signal v2) = Signal (UV.zipWith (.*.) v1 v2) 
  
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
  
class (Sample a) => RatioSample a   
class (Sample a) => TimeSample a
class (Sample a) => FlowSample a

----------------------------------------------------------------------------
-- Typed Arithmetics Class

class (Sample a, Num a, Fractional a) =>  TNum a b where
  type ResType a b 
  (.+.) :: a -> b -> ResType a b 
  (.-.) :: a -> b -> ResType a b 
  (.*.) :: a -> b -> ResType a b 
  (./.) :: a -> b -> ResType a b 

----------------------------------------------------------------------------
-- Each Type with Itself -- Type Remains

instance  (Sample a,  Num a, Fractional a) => TNum  a a  where
  type ResType  a a  = a
  (.+.) x y = toSample $ (fromSample x) + (fromSample y)
  (.-.) x y = toSample $ (fromSample x) + (fromSample y)

----------------------------------------------------------------------------
-- Mixed Arithmetics -- Sample & Double -- Sample Type Remaims

instance (Sample a,  Num a, Fractional a) => TNum  a Double where
  type ResType a Double = a
  (.+.) x y = toSample $ (fromSample x) + y
  (.-.) x y = toSample $ (fromSample x) + y
  (.*.) x y = toSample $ (fromSample x) + y
  (./.) x y = toSample $ (fromSample x) + y
  
instance (Sample a,  Num a, Fractional a) => TNum  Double a where
  type ResType  Double a = a
  (.+.) y x = toSample $ (fromSample x) + y
  (.-.) y x = toSample $ (fromSample x) + y
  (.*.) y x = toSample $ (fromSample x) + y
  (./.) y x = toSample $ (fromSample x) + y


----------------------------------------------------------------------------
-- Flow Calculations over Ratio Samples

-- energy
instance  TNum  ESample NSample  where
  type ResType ESample NSample  = ESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  ESample XSample  where
  type ResType ESample XSample  = ESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  ESample YSample  where
  type ResType ESample YSample  = ESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  ESample MSample  where
  type ResType ESample MSample  = ESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)

-- Power 
instance  TNum  PESample NSample  where
  type ResType PESample NSample  = PESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  PESample XSample  where
  type ResType PESample XSample  = PESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  PESample YSample  where
  type ResType PESample YSample  = PESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


instance  TNum  PESample MSample  where
  type ResType PESample MSample  = PESample
  (.*.) x y = toSample $ (fromSample x) + (fromSample y)
  (./.) x y = toSample $ (fromSample x) / (fromSample y)


----------------------------------------------------------------------------
-- Power / Energy / Time

instance  TNum  ESample DTSample  where
  type ResType  ESample DTSample  = PESample
  (./.) x y = toSample $ (fromSample x) / (fromSample y)

instance  TNum  PESample DTSample  where
  type ResType  PESample DTSample  = ESample
  (.*.) x y = toSample $ (fromSample x) - (fromSample y)

instance  TNum  DTSample PESample  where
  type ResType  DTSample PESample  = ESample
  (.*.) x y = toSample $ (fromSample x) - (fromSample y)

----------------------------------------------------------------------------
-- Integration

genDTSample :: TSample -> TSample -> DTSample
genDTSample (TSample t1) (TSample t2) = DTSample (abs (t2-t1))


integrateSample :: PSample -> PSample -> DTSample -> ESample
integrateSample (PSample ps1) (PSample ps2) (DTSample dt) = toSample ((ps1+ps2)/2*dt)               

               
----------------------------------------------------------------------------
-- Signal Calculations

infixl 7  .*, ./
infixl 6  .+, .-

(.+) :: (UV.Unbox a, Num a, Sample a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.+) = UV.zipWith (+)

(.-) :: (UV.Unbox a, Num a, Sample a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.-) = UV.zipWith (-)

(.*) :: (UV.Unbox a, Num a, Sample a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.*) = UV.zipWith (*)

(./) :: (UV.Unbox a, Num a, Fractional a, Sample a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(./) = UV.zipWith (/)

delta :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> UV.Vector a
delta = (.-)

signm :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a
signm = UV.map (signum)


-}