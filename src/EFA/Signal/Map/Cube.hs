{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Signal.Map.Cube where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Map.Dimension as Dim
import qualified EFA.Signal.Map.Coordinate as Coord
import qualified EFA.Signal.Map.Axis as Axis
import EFA.Signal.Map.Axis (Axis)
import EFA.Signal.Map.Coordinate (System)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data(Data(..),Nil,(:>))
import qualified Data.Map as Map  


import EFA.Signal.Interp as Interp

import qualified Prelude as P
import Prelude hiding (zipWith, map, foldl)
import Data.Maybe(fromMaybe) 

import EFA.Utility.Trace(mytrace)

modul :: ModuleName
modul = ModuleName "Cube"

nc :: FunctionName -> Caller
nc = genCaller modul

data Cube dim label vec a b = Cube {
  getAxes :: System dim label vec a, 
  getVector :: vec b } deriving Show
        
lookupLin ::
  (SV.LookupMaybe vec b,
  Show (vec b)) =>  
  Caller -> Cube dim label vec a b -> Coord.LinIdx -> b
lookupLin caller (Cube _ vec) (Coord.LinIdx idx) = fromMaybe e $ SV.lookupMaybe vec idx
  where e = merror caller modul "lookupLinear" 
            $ "linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec  

lookupLinUnsafe :: 
  SV.LookupUnsafe vec b => 
  Cube dim label vec a b -> Coord.LinIdx -> b
lookupLinUnsafe (Cube _ vec) (Coord.LinIdx idx) = SV.lookupUnsafe vec idx

lookUp :: 
  (SV.LookupMaybe vec b, 
   SV.Storage vec a,Show (vec b), 
   SV.Length vec) =>
  Caller -> Coord.DimIdx dim -> Cube dim label vec a b -> b 
lookUp caller idx ortho@(Cube axes _) = lookupLin (caller |> nc "lookUp") ortho index
  where index = Coord.toLinear axes idx

checkVector :: 
  (SV.Storage vec b, SV.Length vec,SV.Storage vec a) =>
  System dim label vec a -> vec b -> Bool
checkVector (Dim.Data axes) vec =  SV.length vec == numberElements
  where numberElements = P.foldl (*) (1) (fmap Axis.len axes)
    

create :: 
  (Ord a,
   SV.Zipper vec,
   SV.Storage vec Bool,
   SV.Storage vec a,
   SV.Singleton vec,
   SV.Storage vec b, 
   SV.Length vec,
   Dim.Dimensions dim
  ) =>
  Caller -> [(label,vec a)] -> vec b -> Cube dim label vec a b
create caller xs vec = 
  let axes = Coord.createSystem (caller |> (nc "genCube")) xs
  in if checkVector axes vec
  then Cube axes vec
       else merror caller modul "create" 
            "Vector doesn't contain the right number of elements"


generateWithCoordinates :: 
  (SV.Walker vec,
   SV.Storage vec b,
   SV.Storage vec (Dim.Data dim a),
   SV.Storage vec (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList vec) =>
   (Dim.Data dim a -> b) -> System dim label vec a -> Cube dim label vec a b
generateWithCoordinates f axes =  Cube axes $ SV.map f (Coord.vector axes)

map :: 
  (SV.Walker vec,
   SV.Storage vec c,
   SV.Storage vec b) => 
  (b -> c) -> Cube dim label vec a b -> Cube dim label vec a c
map f (Cube axes vec) = (Cube axes $ SV.map f vec)


mapWithCoordinates :: 
 (SV.Walker vec,
  SV.Storage vec c,SV.Storage vec (vec [a]), SV.FromList vec,
  SV.Zipper vec,
  SV.Storage vec b,
  SV.Storage vec (Dim.Data dim a),
  SV.Storage vec a,
  SV.Storage vec [a],
  SV.Singleton vec,
  SV.Storage vec (Dim.Data dim a, b)) =>
 (Dim.Data dim a -> b -> c) -> Cube dim label vec a b -> Cube dim label vec a c
mapWithCoordinates f (Cube axes vec)  = (Cube axes $ SV.map (\(x,y) -> f x y) $ 
                                  SV.zip (Coord.vector axes) vec)

foldl ::(SV.Walker vec, SV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Cube dim label vec a b -> acc
foldl f acc (Cube _ vec) = SV.foldl f acc vec 

foldlWithCoordinates :: 
  (SV.Walker vec,
   SV.Storage vec (vec [a]), 
   SV.FromList vec,
   SV.Zipper vec,
   SV.Storage vec b,
   SV.Storage vec (Dim.Data dim a),
   SV.Storage vec (Dim.Data dim a, b),
   SV.Walker (Axis label vec),
   SV.Storage (Axis label vec) a,
   SV.Storage (Axis label vec) (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList (Axis label vec)) =>
  (acc -> (Dim.Data dim a, b) -> acc) -> acc -> Cube dim label vec a b -> acc
foldlWithCoordinates f acc (Cube axes vec) = SV.foldl f acc $ SV.zip (Coord.vector axes) vec 

zipWith :: 
  (SV.Zipper vec,
   SV.Storage vec d,
   SV.Storage vec c,
   SV.Storage vec b, 
   Eq (System dim label vec a)) =>
  Caller -> 
  (b -> c -> d) -> 
  Cube dim label vec a b -> 
  Cube dim label vec a c -> 
  Cube dim label vec a d
zipWith caller f (Cube axes vec) (Cube axes1 vec1) = 
  if axes == axes1 then Cube axes $ SV.zipWith f vec vec1 
                   else merror caller modul "zipWith" "Axes differ"    

linearData :: Cube dim label vec a b -> vec b
linearData (Cube _ vec) = vec 

-- | Removes the first Dimension
getSubCube ::  
  (SV.Storage vec b, SV.Slice vec, 
   SV.Storage vec a, SV.Length vec) =>
  Caller ->
  Cube dim label vec a b -> 
  Axis.Idx -> Cube (Dim.SubDim dim) label vec a b
getSubCube caller (Cube axes vec) (Axis.Idx idx) = Cube (Dim.dropFirst (caller |> nc "getSubCube") axes) subVec
  where subVec = SV.slice startIdx l vec
        startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l
        l = mytrace 0 "getSubCube" "l" $ Axis.len $ Dim.getFirst (caller |> nc "getSubCube") axes

interpolate :: 
  (Ord a,Arith.Constant b,Num b,SV.LookupMaybe vec b,
   SV.LookupUnsafe vec a,Show (vec b),(Show (vec a)),
   SV.Storage vec a,Show label,
   SV.Length vec,
   SV.Find vec, 
   SV.Storage vec b, SV.Slice vec) =>
  Caller ->  
  ((a,a) -> (b,b) -> a -> Interp.Val b) -> 
  Cube dim label vec a b -> 
  (Dim.Data dim a) ->   
  Interp.Val b
interpolate caller interpFunction ortho coordinates = Interp.combine3 y1 y2 y
  where 
    newCaller = (caller |> (nc "interpolate"))
    axis = mytrace 0 "interpolate" "axis" $ Dim.getFirst newCaller $ 
           getAxes $ mytrace 0 "interpolate" "ortho" $ ortho
    subCoordinates = Dim.dropFirst newCaller coordinates
    x = Dim.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Axis.getSupportPoints axis x
    f idx = interpolate newCaller interpFunction (getSubCube newCaller ortho idx) subCoordinates
    (y1,y2) = if Dim.len coordinates >=2 then (f idx1, f idx2) 
              else (Interp.Inter $ lookUp newCaller (Dim.Data [idx1]) ortho,
                    Interp.Inter $ lookUp newCaller (Dim.Data [idx2]) ortho)
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x

dimension :: Dim.Dimensions dim => Cube dim label vec a b -> Int     
dimension (Cube axes _) = Dim.num axes
    
to2DSignal ::   
    (SV.Walker vec, 
     SV.Storage vec (vec b),
     SV.Storage vec b, 
     SV.Slice vec,     
     SV.Storage vec a, 
     SV.Length vec) =>
    Caller -> 
    Cube dim label vec a b -> 
    Sig.TC Sig.Signal t (Data (vec :> vec :> Nil) b)
to2DSignal caller (Cube axes vec) = Sig.TC $ Data $ SV.imap f $ Axis.getVec axis
      where 
        axis = Dim.getFirst (caller |> nc "nest") $ axes
        l = Axis.len axis
        f idx _ = SV.slice startIdx l vec
          where
            startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l

extract :: 
  (SV.Walker vec,
   SV.Storage vec a,
   SV.Storage vec Axis.Idx,SV.LookupUnsafe vec b,
   SV.FromList vec, 
   SV.Storage vec b, SV.Storage vec Coord.LinIdx, 
   SV.Storage vec (Dim.Data dim Axis.Idx),
   SV.Storage vec [Axis.Idx],
   SV.Storage vec (vec [Axis.Idx]),
   SV.Length vec,
   SV.Singleton vec) =>
  Caller -> 
  Cube dim label vec a b -> 
  Dim.Data dim2 (Dim.Idx) -> 
  Map.Map Dim.Idx Axis.Idx -> 
  Cube dim2 label vec a b
extract caller cube@(Cube axes _) dims2Keep dims2Drop = Cube newAxes newVec
  where newAxes = Coord.extractSubSystem newCaller axes dims2Keep
        newCaller =  caller |> nc "extractCube"
        indexVec = Coord.reductionIndexVector axes dims2Drop
        newVec = SV.map (lookupLinUnsafe cube) indexVec  
    
