{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Ortho where

import EFA.Utility(Caller)

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Map.Dimension as Dim
import EFA.Signal.Map.Dimension(Dim2)
import qualified EFA.Signal.Map.Axes as Axes
import EFA.Signal.Map.Axes (Axes)

import EFA.Signal.Interp as Interp


import qualified Data.Vector as V 
import Data.Maybe(fromMaybe) 

data Ortho dim vec a b = Ortho {
  getAxes :: Axes dim vec a, 
  getVector :: vec b } deriving Show

newtype LinIdx = LinIdx {getInt:: Int} deriving Show

toLinear :: 
 (SV.Storage vec a, SV.Length vec)=>  
  Axes dim vec a -> Axes.DimIdx dim -> LinIdx
toLinear (Dim.Data xs) (Axes.DimIdx (Dim.Data indices)) = LinIdx $
  foldl (+) (0) $ zipWith (*) indices dimensionMultiplicators
  where dimensionMultiplicators = (init $ map Axes.len xs) ++ [1]
        
lookupLin ::SV.LookupMaybe vec b =>  Caller -> Ortho dim vec a b -> LinIdx -> b
lookupLin caller (Ortho _ vec) (LinIdx idx) = fromMaybe (error m) $ SV.lookupMaybe vec idx
  where m = "Error in LookupLinear called by " ++ caller ++
            " - linear Index out of Bounds: " ++ show idx  

lookup :: 
  (SV.LookupMaybe vec b, 
   SV.Storage vec a, 
   SV.Length vec) =>
  Caller -> Axes.DimIdx dim -> Ortho dim vec a b -> b 
lookup caller idx ortho@(Ortho axes _) = lookupLin (caller ++">lookup") ortho index
  where index = toLinear axes idx

checkVector :: 
  (SV.Storage vec b, SV.Length vec,SV.Storage vec a) =>
  Axes dim vec a -> vec b -> Bool
checkVector (Dim.Data axes) vec =  SV.length vec == numberElements
  where numberElements = foldl (*) (1) (fmap Axes.len axes)
    

create :: 
  (Ord a,
   SV.Zipper vec,
   SV.Storage vec Bool,
   SV.Storage vec a,
   SV.Singleton vec,
   SV.Storage vec b, 
   SV.Length vec,
   Dim.FromList dim) =>
  Caller -> [vec a] -> vec b -> Ortho dim vec a b
create caller xs vec = 
  let axes = Axes.create (caller++">genOrtho") xs
  in if checkVector axes vec
  then Ortho axes vec
       else error ("Error in Ortho.genOrtho called by " ++ 
            caller ++ " - Vector doesn't contain the right number of elements")

-- | Removes the first Dimension
getSubOrtho ::  
  (SV.Storage vec b, SV.Slice vec, 
   SV.Storage vec a, SV.Length vec) =>
  Caller ->
  Ortho (Dim.Succ dim) vec a b -> 
  Axes.Idx -> Ortho dim vec a b
getSubOrtho caller (Ortho axes vec) (Axes.Idx idx) = Ortho (Dim.dropFirst (caller++">getSubOrtho") axes) subVec
  where subVec = SV.slice (0) (l-1) vec
        startIdx = idx*l
        stopIdx = idx*l+(l-1) 
        l = Axes.len $ Dim.getFirst (caller++">getSubOrtho") axes
{-  
interpolate :: 
  Caller ->  
  ((a,a) -> (b,b) -> a -> Interp.Val b) -> 
  Ortho dim vec a b -> 
  (Dim.Data dim a) ->   
  Interp.Val b  
interpolate caller interpFunction ortho coordinates = Interp.combine3 y1 y2 y
  where 
    newCaller = (caller ++ ">Ortho.interpolate")
    axis = Dim.getFirst newCaller $ getAxes ortho
    subCoordinates = Dim.dropFirst newCaller coordinates
    x = Dim.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Axes.getSupportPoints axis x
    y1 = interpolate newCaller interpFunction (getSubOrtho newCaller ortho idx1) subCoordinates
    y2 = interpolate newCaller interpFunction (getSubOrtho newCaller ortho idx2) subCoordinates
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x 
    
 -} 
  