{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Ortho where

import EFA.Utility(Caller)

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Map.Dimension as Dim
import EFA.Signal.Map.Dimension(Dim2)
import qualified EFA.Signal.Map.Axes as Axes
import EFA.Signal.Map.Axes (Axes)


import qualified Data.Vector as V 
import Data.Maybe(fromMaybe) 

data Ortho dim vec a b = Ortho (Axes dim vec a) (vec b) deriving Show



newtype LinIdx = LinIdx {getInt:: Int} deriving Show

toLinear :: 
 (SV.Storage vec a, SV.Length vec)=>  
  Axes dim vec a -> Axes.DimIdx dim -> LinIdx
toLinear (Dim.Data xs) (Axes.DimIdx (Dim.Data indices)) = LinIdx $
  foldl (+) (0) $ zipWith (*) indices dimensionMultiplicators
  where dimensionMultiplicators = (init $ map Axes.length xs) ++ [1]
        
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
  where numberElements = foldl (*) (1) (fmap Axes.length axes)
    

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


