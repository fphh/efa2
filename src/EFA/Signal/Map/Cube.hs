{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Cube where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Map.Dimension as Dim
import qualified EFA.Signal.Map.Coordinate as Coord
import EFA.Signal.Map.Coordinate (System)

import EFA.Signal.Interp as Interp

import qualified Prelude as P
import Prelude hiding (zipWith, map, foldl)
import Data.Maybe(fromMaybe) 

import EFA.Utility.Trace(mytrace)

modul :: ModuleName
modul = ModuleName "Cube"

nc :: FunctionName -> Caller
nc = genCaller modul

data Cube dim vec a b = Cube {
  getAxes :: System dim vec a, 
  getVector :: vec b } deriving Show

newtype LinIdx = LinIdx {getInt:: Int} deriving Show

toLinear :: 
 (SV.Storage vec a, SV.Length vec)=>  
  System dim vec a -> Coord.DimIdx dim -> LinIdx
toLinear (Dim.Data xs) (Dim.Data indices) = LinIdx $
  P.foldl (+) (0) $ P.zipWith (*) 
  (P.map Coord.getInt indices) dimensionMultiplicators
  where dimensionMultiplicators = (init $ P.map Coord.len xs) ++ [1]
        
lookupLin ::
  (SV.LookupMaybe vec b,
  Show (vec b)) =>  
  Caller -> Cube dim vec a b -> LinIdx -> b
lookupLin caller (Cube _ vec) (LinIdx idx) = fromMaybe e $ SV.lookupMaybe vec idx
  where e = merror caller modul "lookupLinear" 
            $ "linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec  

lookUp :: 
  (SV.LookupMaybe vec b, 
   SV.Storage vec a,Show (vec b), 
   SV.Length vec) =>
  Caller -> Coord.DimIdx dim -> Cube dim vec a b -> b 
lookUp caller idx ortho@(Cube axes _) = lookupLin (caller |> (nc "lookUp")) ortho index
  where index = toLinear axes idx

checkVector :: 
  (SV.Storage vec b, SV.Length vec,SV.Storage vec a) =>
  System dim vec a -> vec b -> Bool
checkVector (Dim.Data axes) vec =  SV.length vec == numberElements
  where numberElements = P.foldl (*) (1) (fmap Coord.len axes)
    

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
  Caller -> [vec a] -> vec b -> Cube dim vec a b
create caller xs vec = 
  let axes = Coord.createSystem (caller |> (nc "genCube")) xs
  in if checkVector axes vec
  then Cube axes vec
       else merror caller modul "create" 
            "Vector doesn't contain the right number of elements"

map :: 
  (SV.Walker vec,
   SV.Storage vec c,
   SV.Storage vec b) => 
  (b -> c) -> Cube dim vec a b -> Cube dim vec a c
map f (Cube axes vec) = (Cube axes $ SV.map f vec)


mapWithCoordinates :: 
 (SV.Walker vec,
  SV.Storage vec c,
  SV.Zipper vec,
  SV.Storage vec b,
  SV.Storage vec (Dim.Data dim a),
  SV.Walker (Coord.Axis vec),
  SV.Storage (Coord.Axis vec) a,
  SV.Storage (Coord.Axis vec) (vec [a]),
  SV.Storage vec a,
  SV.Storage vec [a],
  SV.Singleton vec,
  SV.FromList (Coord.Axis vec),
  SV.Storage vec (Dim.Data dim a, b)) =>
 (Dim.Data dim a -> b -> c) -> Cube dim vec a b -> Cube dim vec a c
mapWithCoordinates f (Cube axes vec)  = (Cube axes $ SV.map (\(x,y) -> f x y) $ 
                                  SV.zip (Coord.vector axes) vec)

foldl ::(SV.Walker vec, SV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Cube dim vec a b -> acc
foldl f acc (Cube _ vec) = SV.foldl f acc vec 

foldlWithCoordinates :: 
  (SV.Walker vec,
   SV.Zipper vec,
   SV.Storage vec b,
   SV.Storage vec (Dim.Data dim a),
   SV.Storage vec (Dim.Data dim a, b),
   SV.Walker (Coord.Axis vec),
   SV.Storage (Coord.Axis vec) a,
   SV.Storage (Coord.Axis vec) (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList (Coord.Axis vec)) =>
  (acc -> (Dim.Data dim a, b) -> acc) -> acc -> Cube dim vec a b -> acc
foldlWithCoordinates f acc (Cube axes vec) = SV.foldl f acc $ SV.zip (Coord.vector axes) vec 

zipWith :: 
  (SV.Zipper vec,
   SV.Storage vec d,
   SV.Storage vec c,
   SV.Storage vec b, 
   Eq (System dim vec a)) =>
  Caller -> 
  (b -> c -> d) -> 
  Cube dim vec a b -> 
  Cube dim vec a c -> 
  Cube dim vec a d
zipWith caller f (Cube axes vec) (Cube axes1 vec1) = 
  if axes == axes1 then Cube axes $ SV.zipWith f vec vec1 
                   else merror caller modul "zipWith" "Axes differ"    


-- | Removes the first Dimension
getSubCube ::  
  (SV.Storage vec b, SV.Slice vec, 
   SV.Storage vec a, SV.Length vec) =>
  Caller ->
  Cube dim vec a b -> 
  Coord.Idx -> Cube (Dim.SubDim dim) vec a b
getSubCube caller (Cube axes vec) (Coord.Idx idx) = Cube (Dim.dropFirst (caller |> (nc "getSubCube")) axes) subVec
  where subVec = SV.slice startIdx l vec
        startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l
        l = mytrace 0 "getSubCube" "l" $ Coord.len $ Dim.getFirst (caller |> (nc "getSubCube")) axes

interpolate :: 
  (Ord a,Arith.Constant b,Num b,SV.LookupMaybe vec b,
   SV.UnsafeLookup vec a,Show (vec b),(Show (vec a)),
   SV.Storage vec a,
   SV.Length vec,
   SV.Find vec, 
   SV.Storage vec b, SV.Slice vec) =>
  Caller ->  
  ((a,a) -> (b,b) -> a -> Interp.Val b) -> 
  Cube dim vec a b -> 
  (Dim.Data dim a) ->   
  Interp.Val b
interpolate caller interpFunction ortho coordinates = Interp.combine3 y1 y2 y
  where 
    newCaller = (caller |> (nc "interpolate"))
    axis = mytrace 0 "interpolate" "axis" $ Dim.getFirst newCaller $ 
           getAxes $ mytrace 0 "interpolate" "ortho" $ ortho
    subCoordinates = Dim.dropFirst newCaller coordinates
    x = Dim.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Coord.getSupportPoints axis x
    f idx = interpolate newCaller interpFunction (getSubCube newCaller ortho idx) subCoordinates
    (y1,y2) = if Dim.len coordinates >=2 then (f idx1, f idx2) 
              else (Interp.Inter $ lookUp newCaller (Dim.Data [idx1]) ortho,
                    Interp.Inter $ lookUp newCaller (Dim.Data [idx2]) ortho)
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x

    

