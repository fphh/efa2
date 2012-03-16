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
data Signal a = Signal (UVec a) 


smap f (Signal vec) = map f vec
sipWith f (Signal v1) (Signal v2)= Signal (zipWith f v1 v2)

-- class (Sample a, Sample b, Sample c) => SigMult a b c | a b -> c where
--  (.*.) :: Signal a -> Signal b -> Signal c
(.*.) ::  Signal a -> Signal b -> Signal c

-- class SignMult where
(.*.) s1 s2 = sipWith (.*) s1 s2

class (Sample a, Sample b, Sample c) => TNumMult a b c | a b -> c where
  (.*) :: a -> b -> c
  (.*) x  y = toSample ((fromSample x) * (fromSample y)) 
  
instance TNumMult PESample DTSample ESample
instance TNumMult PESample NSample PESample where
  (.*) x y = toSample ((fromSample x) + (fromSample y)) 


p1 = PESample (0.5) 
p2 = PESample (0.3)
n1 = NSample (0.8)
d = 2.0::Double
dt = DTSample (0.1)


main = do
  putStrLn $ show (p1.*n1)
  
  putStrLn $ show (p1.*dt)



-- ### Problem -- Sample inheritance works fine
-- but adding a layer on top requires the exact same case list which is rubbish !!!!

-- ghc --make Main.hs
-- [1 of 1] Compiling Main             ( Main.hs, Main.o )

-- Main.hs:77:23:
--     No instance for (TNumMult d e f)
--       arising from a use of `.*'
--     In the first argument of `sipWith', namely `(.*)'
--     In the expression: sipWith (.*) s1 s2
--     In an equation for `.*.': .*. s1 s2 = sipWith (.*) s1 s2

-- Compilation exited abnormally with code 1 at Wed Mar 14 15:27:34


-- The example works without type declaration, but class doesn't allow this