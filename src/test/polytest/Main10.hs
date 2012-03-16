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


class (Eq a, Ord a, Fractional a, Num a, Enum a, Show a, Real a, Floating a, RealFloat a, RealFrac a, UV.Unbox a) => Sample a where
  fromSample :: a -> Double 
  toSample :: Double -> a
  
instance Sample ESample where fromSample (ESample x) = x; toSample x = ESample x    
instance Sample TSample where fromSample (TSample x) = x; toSample x = TSample x 
instance Sample PSample where fromSample (PSample x) = x; toSample x = PSample x                               

class (Sample a, Sample b, Sample c) => Types a b c  | a b -> c where
  (.*.) :: a -> b -> c
  (.*.) x y = toSample (fromSample x * fromSample y) 
    
--instance Types a b c where
instance Types PSample TSample ESample 

data Signal a = Signal (UVec a) deriving (Show)

class (Types a b c) => SigProd a b c where
  (.*) :: Signal a -> Signal b -> Signal c
  (.*) (Signal v1)  (Signal v2) = (Signal (UV.zipWith (.*.) v1 v2)) 

instance SigProd PSample TSample ESample

s1 = Signal (UV.fromList [PSample 0.5 , PSample 0.3]) 
s2 = Signal (UV.fromList [TSample 0.1 , TSample 0.1])

main = do
  putStrLn $ show (s1.*s2)

