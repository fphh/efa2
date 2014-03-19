{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Axes where

import EFA.Utility(Caller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Map.Dimension as Dim 


-- | Datatype with monotonically rising values 
newtype Abszissa vec a = Abszissa (vec a) deriving Show

data Idx = Idx Int

newtype DimIdx dim = DimIdx (Dim.Data dim Int) deriving Show

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

create :: 
  (Dim.FromList dim,
   Ord a,
   SV.Zipper vec,
   SV.Storage vec a,
   SV.Storage vec Bool,
   SV.Singleton vec) =>
            Caller -> [vec a] -> Axes dim vec a
create caller xs = Dim.fromList (caller ++">genAxes")  
                    $ map (fromVec (caller ++ ">genAxes")) xs


findIndex :: 
  (SV.Storage vec a, SV.Find vec)=>
  (a -> Bool) -> Abszissa vec a -> Maybe Idx
findIndex f (Abszissa vec) = fmap Idx $ SV.findIndex f vec

{-
findIndices :: Axes dim vec a -> Dim.Data dim a -> Maybe (DimIdx dim)
findIndices (Dim.Data axes) (Dim.Data coordinates) = 
  let maybeList = zipWith (\ axis val -> findIndex (>val) axis) axes  coordinates 
      allJust = all (isJust) maybeList
  in if allJust then Just Dim.Data (catMaybes maybeList) else Nothing
-}