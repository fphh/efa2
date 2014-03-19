{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Abszissa where

import EFA.Utility(Caller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Map.Dimension as Dim 


-- | Datatype with monotonically rising values 
newtype Abszissa vec a = Abszissa (vec a) deriving Show 

length ::
  (SV.Storage vec a, SV.Length vec)=>
  Abszissa vec a -> Int
length (Abszissa vec) = SV.length vec

fromVec :: 
  (SV.Storage vec Bool, SV.Singleton vec, 
   SV.Zipper vec, SV.Storage vec a,Ord a) =>
  Caller -> vec a -> Abszissa vec a
fromVec caller vec = 
  if isMonoton then Abszissa vec   
  else error ("Error in Abszissa.generate called by " ++ 
       caller ++ " - vector of elements is not monotonically rising")   
    where isMonoton = SV.all (==True) $ SV.deltaMap (\ x1 x2 -> x2 > x1) vec
          
          
type Axes dim vec a = Dim.Data dim (Abszissa vec a)

type Axes dim vec a = Dim.Data dim (Abszissa vec a)