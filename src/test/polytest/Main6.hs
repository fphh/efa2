{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances  #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


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

----------------------------------------------------------------------------
-- Samples Based around Time Signals

class Sample a where
  fromSample :: a -> Double
  toSample :: Double -> a

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
  
instance Sample Double where
  fromSample (x) = x
  toSample x = x

newtype PESignal = PESignal UVec deriving (Show,Num,Eq)
newtype DTSignal = DTSignal UVec deriving (Show,Num,Eq)
newtype ESignal = ESignal UVec deriving (Show,Num,Eq)

  

class (Sample a) => FlowVal a
instance FlowVal PESample
instance FlowVal ESample
instance FlowVal DTSample

class (Sample a) => Ratio a
instance Ratio NSample

class Signal a where
  fromSignal   :: a -> UVec 
  toSignal :: UVec -> a 
  
instance Signal PESignal where
  fromSignal (PESignal v) = v
  toSignal v = (PESignal v)

instance Signal ESignal where
  fromSignal (ESignal v) = v
  toSignal v = (ESignal v)

instance Signal DTSignal where
  fromSignal (DTSignal v) = v
  toSignal v = (DTSignal v)

-- Typed Arithmetics 2

type Val = Double
type UVec = UV.Vector Double
-- data Signal a = Signal (UVec a) deriving Show


-- smap f (Signal vec) = map f vec
-- sipWith f (Signal v1) (Signal v2)= Signal (zipWith f v1 v2)

instance Num UVec where
  (*) x y = UV.zipWith (*) x y 

class TNumMult a b c | a b -> c where
  (.*) :: a -> b -> c

  
instance TNumMult PESample DTSample ESample where  (.*) x y = toSample ((fromSample x) * (fromSample y)) 
instance TNumMult PESample NSample PESample where  (.*) x y = toSample ((fromSample x) * (fromSample y))
instance TNumMult PESignal DTSignal ESignal where  (.*) x y = toSignal ((fromSignal x) * (fromSignal y)) 




p1 = PESample (0.5) 
p2 = PESample (0.3)
n1 = NSample (0.8)
d = 2.0::Double
dt = DTSample (0.1)

s1 = PESignal (UV.fromList [(0.5) , (0.3)]) 
s2 = DTSignal (UV.fromList [(0.1) , (0.1)])


main = do
  putStrLn $ show (p1.*n1)
  
  putStrLn $ show (s1.*s2)



-- Funktioniert -- aber -- für jeden fall muss Formel definiert werden