{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F



type UVec a = UV.Vector a
type Val = Double

-----------------------------------------------------------------------------------
-- Flow Signal
  
newtype PSample = PSample Double deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)
newtype ESample = ESample Double deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)
newtype TSample = TSample Double deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)


--------------------------------------------------------------------------------------------
-- Sample Class

class (Eq a, Ord a, Fractional a, Num a, Enum a, Show a, Real a, Floating a, RealFloat a, RealFrac a, UV.Unbox a) => Sample a where
  fromSample :: a -> Double 
  toSample :: Double -> a
  
instance Sample ESample where fromSample (ESample x) = x; toSample x = ESample x    
instance Sample TSample where fromSample (TSample x) = x; toSample x = TSample x 
instance Sample PSample where fromSample (PSample x) = x; toSample x = PSample x                               

--------------------------------------------------------------------------------------------
-- Sample Calculation Class

class (Sample a, Sample b, Sample c) => Types a b c  | a b -> c where
  (.*.) :: a -> b -> c
  (.*.) x y = toSample (fromSample x * fromSample y) 

instance Types PSample TSample ESample 


data Signal a = Signal (UVec a) deriving (Show)
data Hist a = Hist (UVec a) deriving (Show)


--------------------------------------------------------------------------------------------
-- Vector Class

class Vector a b where
  toVector :: a -> UVec b
  fromVector :: UVec b -> a

instance Vector (Signal b) b where
  toVector (Signal x) = x
  fromVector x = Signal x
  
instance Vector (Hist b) b where  
  toVector (Hist x) = x
  fromVector x = Hist x
  

--------------------------------------------------------------------------------------------
-- Generic Product Class with Instances

class (Vector x a, Vector y b, Vector z c) => Prod x a y b z c where
 (~*) :: x -> y ->  z


instance (SigProd a b c) => Prod (Signal a) a (Signal b) b (Signal c) c where (~*) x y = x .* y 
instance (HSigProd a b c) => Prod (Hist a) a (Signal b) b (Signal c) c where (~*) x y = x *~ y


--------------------------------------------------------------------------------------------
-- Implementation Classes

class (Types a b c) => SigProd a b c where
  (.*) :: Signal a -> Signal b -> Signal c

class (Types a b c) => HSigProd a b c where
  (*~) :: Hist a -> Signal b -> Signal c


instance (Types a b c) => SigProd a b c where
  (.*) (Signal v1)  (Signal v2) = (Signal (UV.zipWith (.*.) v1 v2)) 

instance (Types a b c) => HSigProd a b c where
  (*~) (Hist v1)  (Signal v2) = (Signal (UV.zipWith (.*.) v1 v2)) 


s1 = Signal (UV.fromList [PSample 0.5 , PSample 0.3]) 
s2 = Signal (UV.fromList [TSample 0.1 , TSample 0.1])
h1 = Signal (UV.fromList [PSample 0.5 , PSample 0.3])

main = do
  putStrLn $ show (s1~*s2)
  putStrLn $ show (h1~*s2)

