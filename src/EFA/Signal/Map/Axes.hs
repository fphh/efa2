{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Axes where

import EFA.Utility(Caller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Map.Dimension as Dim


-- | Datatype with monotonically rising values
newtype Axis vec a = Axis {getVec :: vec a} deriving (Show,Eq)
-- newtype Data vec a = Data (vec a) deriving Show

newtype Idx = Idx {getInt :: Int} deriving Show

indexAdd :: Idx -> Int -> Idx
indexAdd (Idx idx) num = Idx $ (idx+num)

type DimIdx dim = Dim.Data dim Idx

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
  (Dim.Dimensions dim,
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

generate::
  (SV.Walker vec,SV.Storage vec (Dim.Data dim a),
   SV.Singleton vec,
   SV.Storage vec [a],
   SV.Storage vec a,
   SV.Storage (Axis vec) (vec [a]),
   SV.FromList (Axis vec),
   SV.Walker (Axis vec),
   SV.Storage (Axis vec) a) =>
  Axes dim vec a ->
  vec (Dim.Data dim a)
generate axes = SV.map Dim.Data $ g axes
  where
    g (Dim.Data [Axis axis]) = SV.map (\x -> [x]) $ axis
    g (Dim.Data axes) =
      SV.concat $ SV.toList $ SV.map (\x -> SV.map (\xs -> x:xs) vector) axis
      where axis = head axes
            vector = g $ (Dim.Data $ tail axes)
