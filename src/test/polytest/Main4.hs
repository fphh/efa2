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

class (Sample a) => FlowVal a
instance FlowVal PESample
instance FlowVal ESample
instance FlowVal DTSample

class (Sample a) => Ratio a
instance Ratio NSample



-- Typed Arithmetics 2

type Val = Double
type UVec a = [a]
data Signal a = Signal (UVec a) deriving Show


smap f (Signal vec) = map f vec
sipWith f (Signal v1) (Signal v2)= Signal (zipWith f v1 v2)


-- different Data Classes
class Sig a
-- class Sample a

-- top level no restrictions
class TNumMult a b c | a b -> c where
  (.*) :: a -> b -> c

-- next level with specific type restrictions -- but where to get c from ?
instance (Sample a, Sample b, Sample c) => TNumMult a b c where
  (.*) x  y = toSample ((fromSample x) * (fromSample y)) 
  
instance (Sig a, Sig b, Sig c) => TNumMult a b c where
  (.*) x y = sipWith (.*) x y




p1 = PESample (0.5) 
p2 = PESample (0.3)
n1 = NSample (0.8)
d = 2.0::Double
dt = DTSample (0.1)

s1 = Signal ([PESample (0.5) , PESample (0.3)]) 
s2 = Signal ([DTSample (0.1) , DTSample (0.1)])


main = do
  putStrLn $ show (p1.*n1)
  
  putStrLn $ show (s1.*s2)



-- Funktioniert nicht !! -- duplicate instance a,b,c
-- c kann nicht bestimmt werden