{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

-----------------------------------------------------------------------------------
-- Base Data Type

type Val = Double

-----------------------------------------------------------------------------------
-- Vector Convenience

type UVec a = UV.Vector a

-----------------------------------------------------------------------------------
-- Flow Signal
  
-- time
newtype TSample = TSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- time step data
newtype DTSample = DTSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow data or storage content data 
newtype ESample = ESample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average power of energy flow
newtype PSample = PSample Double
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


--------------------------------------------------------------------------------------------
-- Sample Access Class

class (Eq a, Ord a, Fractional a, Num a, Enum a, Show a, Real a, Floating a, RealFloat a, RealFrac a, UV.Unbox a) => Sample a where
  fromSample :: a -> Val
  toSample :: Val -> a
  
instance Sample PSample where fromSample (PSample x) = x; toSample x = PSample x                               
instance Sample TSample where fromSample (TSample x) = x; toSample x = TSample x
instance Sample PESample where fromSample (PESample x) = x; toSample x = PESample x
instance Sample ESample where fromSample (ESample x) = x; toSample x = ESample x
instance Sample DTSample where fromSample (DTSample x) = x; toSample x = DTSample x
instance Sample NSample where fromSample (NSample x) = x; toSample x = NSample x
instance Sample XSample where fromSample (XSample x) = x; toSample x = XSample x
instance Sample YSample where fromSample (YSample x) = x; toSample x = YSample x
instance Sample MSample where fromSample (MSample x) = x; toSample x = MSample x
-- also for Double !!
instance Sample Double where  fromSample (x) = x; toSample x = x

--------------------------------------------------------------------------------------------
-- Sample Calculation Class

class (Sample a, Sample b, Sample c) => SampleSum a b c  | a b -> c where
  (.+.) :: a -> b -> c
  (.+.) x y = toSample (fromSample x * fromSample y) 
  (.-.) :: a -> b -> c
  (.-.) x y = toSample (fromSample x * fromSample y) 
  
instance  SampleSum PESample PESample PESample
instance  SampleSum PSample PSample PSample


class (Sample a, Sample b, Sample c) => SampleProd a b c  | a b -> c where
  (.*.) :: a -> b -> c
  (.*.) x y = toSample (fromSample x * fromSample y) 

instance  SampleProd PESample NSample PESample
instance  SampleProd PESample XSample PESample
instance  SampleProd PESample YSample PESample
instance  SampleProd PESample MSample PESample

instance  SampleProd ESample NSample ESample
instance  SampleProd ESample XSample ESample
instance  SampleProd ESample YSample ESample
instance  SampleProd ESample MSample ESample

instance  SampleProd PESample DTSample ESample


class (Sample a, Sample b, Sample c) => SampleDiv a b c  | a b -> c where
  (./.) :: a -> b -> c
  (./.) x y = toSample (fromSample x * fromSample y) 


instance SampleDiv PSample TSample ESample 

--------------------------------------------------------------------------------------------
-- Data Containers 

data Signal a = Signal (UVec a) deriving (Show)
data Distrib a = Distrib (UVec a) deriving (Show)
data Flow a = Flow a deriving (Show)


--------------------------------------------------------------------------------------------
-- Data Container Access Class

class Data a b where
  toData :: a -> UVec b
  fromData :: UVec b -> a

instance Data (Signal b) b where
  toData (Signal x) = x
  fromData x = Signal x
  
instance Data (Distrib b) b where  
  toData (Distrib x) = x
  fromData x = Distrib x
  
--------------------------------------------------------------------------------------------
-- Generic Product Class with Instances

class Sum x y z  | x y -> z where  (.+) :: x -> y ->  z; (.-) :: x -> y ->  z 
instance (SampleSum a b c) => Sum (Signal a) (Signal b) (Signal c) where 
  (.+) (Signal x) (Signal y) = Signal (UV.zipWith (.+.) x y); (.-) (Signal x) (Signal y) = Signal (UV.zipWith (.-.) x y)

class Prod x y z  | x y -> z where  (.*) :: x -> y ->  z
instance (SampleProd a b c) => Prod (Signal a) (Signal b) (Signal c) where (.*) (Signal x) (Signal y) = Signal (UV.zipWith (.*.) x y) 

class Div x y z  | x y -> z where  (./) :: x -> y ->  z
instance (SampleDiv a b c) => Div (Signal a) (Signal b) (Signal c) where (./) (Signal x) (Signal y) =  Signal (UV.zipWith (./.) x y)



s1 = Signal (UV.fromList [PESample 0.5 , PESample 0.3]) 
s2 = Signal (UV.fromList [DTSample 0.1 , DTSample 0.1])
h1 = Signal (UV.fromList [PESample 0.5 , PESample 0.3])

main = do

  let s4 = (s1.*s2) :: Signal ESample
  putStrLn $ show (s4) 

