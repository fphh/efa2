{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Axes where

import EFA.Utility(Caller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Map.Dimension as Dim 


-- | Datatype with monotonically rising values 
newtype Axis vec a = Axis (vec a) deriving Show
newtype Data vec a = Data (vec a) deriving Show

data Idx = Idx Int

indexAdd :: Idx -> Int -> Idx
indexAdd (Idx idx) num = Idx $ (idx+num) 

newtype DimIdx dim = DimIdx (Dim.Data dim Int) deriving Show

len ::
  (SV.Storage vec a, SV.Length vec)=>
  Axis vec a -> Int
len (Axis vec) = SV.length vec

fromVec :: 
  (SV.Storage vec Bool, SV.Singleton vec, 
   SV.Zipper vec, SV.Storage vec a,Ord a) =>
  Caller -> vec a -> Axis vec a
fromVec caller vec = 
  if isMonoton then Axis vec   
  else error ("Error in Axis.generate called by " ++ 
       caller ++ " - vector of elements is not monotonically rising")   
    where isMonoton = SV.all (==True) $ SV.deltaMap (\ x1 x2 -> x2 > x1) vec
          
          
type Axes dim vec a = Dim.Data dim (Axis vec a)

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
  (a -> Bool) -> Axis vec a -> Maybe Idx
findIndex f (Axis vec) = fmap Idx $ SV.findIndex f vec


unsafeLookup :: 
  SV.UnsafeLookup vec a => 
  Axis vec a -> Idx -> a
unsafeLookup (Axis axis) (Idx idx) = SV.unsafeLookup axis idx 

unsafeLookupData :: 
  SV.UnsafeLookup vec a => 
  Data vec a -> Idx -> a
unsafeLookupData (Data vec) (Idx idx) = SV.unsafeLookup vec idx 


findRightInterpolationIndex :: 
  (SV.Storage vec a, SV.Find vec, Ord a, SV.Length vec) =>
  Axis vec a -> a -> Idx
findRightInterpolationIndex axis x = rightIndex
  where 
    idx = findIndex (>x) axis
    rightIndex = case idx of
      Just (Idx idx) -> if idx==0 then Idx 1 else Idx idx 
      Nothing   -> Idx $ (len axis)-1


getSupportPoints :: 
  (Ord a,
   SV.Storage vec a,
   SV.Length vec,
   SV.Find vec,
   SV.UnsafeLookup vec a) =>
  Axis vec a ->  a -> ((Idx,Idx),(a,a))
getSupportPoints axis x = ((leftIndex,rightIndex),
                                 (unsafeLookup axis leftIndex, unsafeLookup axis rightIndex))
  where rightIndex = findRightInterpolationIndex axis x
        leftIndex = indexAdd rightIndex (-1)

{-
interpolate :: 
  Caller -> 
  Interp.Method b ->
  Interp.ExtrapMethod b ->
  Axes.Axis vec a -> 
  Axes.Data vec b ->
  a ->   
  Interp.Val b
interpolate caller inM exM axis curve x =  
  let 
    (xPair,yPair) = Axes.getSupportPoints axis curve x
  in Interp.dim1 (caller++ ">interp1LinValid_new") inM exM xPair yPair x
-}