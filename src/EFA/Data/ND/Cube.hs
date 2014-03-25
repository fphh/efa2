{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Data.ND.Cube where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as SV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.ND.Cube.Grid as Grid
import qualified EFA.Data.Axis as Axis
import EFA.Data.ND.Cube.Grid (Grid)
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
  getAxes :: Grid dim label vec a, 
  getVector :: vec b } deriving Show
        
lookupLin ::
  (SV.LookupMaybe vec b,
  Show (vec b)) =>  
  Caller -> Cube dim label vec a b -> Grid.LinIdx -> b
lookupLin caller (Cube _ vec) (Grid.LinIdx idx) = fromMaybe e $ SV.lookupMaybe vec idx
  where e = merror caller modul "lookupLinear" 
            $ "linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec  

lookupLinUnsafe :: 
  SV.LookupUnsafe vec b => 
  Cube dim label vec a b -> Grid.LinIdx -> b
lookupLinUnsafe (Cube _ vec) (Grid.LinIdx idx) = SV.lookupUnsafe vec idx

lookUp :: 
  (SV.LookupMaybe vec b, 
   SV.Storage vec a,Show (vec b), 
   SV.Length vec) =>
  Caller -> Grid.DimIdx dim -> Cube dim label vec a b -> b 
lookUp caller idx ortho@(Cube axes _) = lookupLin (caller |> nc "lookUp") ortho index
  where index = Grid.toLinear axes idx

checkVector :: 
  (SV.Storage vec b, SV.Length vec,SV.Storage vec a) =>
  Grid dim label vec a -> vec b -> Bool
checkVector (ND.Data axes) vec =  SV.length vec == numberElements
  where numberElements = P.foldl (*) (1) (fmap Axis.len axes)
    

create :: 
  (Ord a,
   SV.Zipper vec,
   SV.Storage vec Bool,
   SV.Storage vec a,
   SV.Singleton vec,
   SV.Storage vec b, 
   SV.Length vec,
   ND.Dimensions dim
  ) =>
  Caller -> [(label,vec a)] -> vec b -> Cube dim label vec a b
create caller xs vec = 
  let axes = Grid.create (caller |> (nc "create")) xs
  in if checkVector axes vec
  then Cube axes vec
       else merror caller modul "create" 
            "Vector doesn't contain the right number of elements"


generateWithGrid :: 
  (SV.Walker vec,
   SV.Storage vec b,
   SV.Storage vec (ND.Data dim a),
   SV.Storage vec (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList vec) =>
   (ND.Data dim a -> b) -> Grid dim label vec a -> Cube dim label vec a b
generateWithGrid f axes =  Cube axes $ SV.map f (Grid.toVector axes)

map :: 
  (SV.Walker vec,
   SV.Storage vec c,
   SV.Storage vec b) => 
  (b -> c) -> Cube dim label vec a b -> Cube dim label vec a c
map f (Cube axes vec) = (Cube axes $ SV.map f vec)


mapWithGrid :: 
 (SV.Walker vec,
  SV.Storage vec c,SV.Storage vec (vec [a]), SV.FromList vec,
  SV.Zipper vec,
  SV.Storage vec b,
  SV.Storage vec (ND.Data dim a),
  SV.Storage vec a,
  SV.Storage vec [a],
  SV.Singleton vec,
  SV.Storage vec (ND.Data dim a, b)) =>
 (ND.Data dim a -> b -> c) -> Cube dim label vec a b -> Cube dim label vec a c
mapWithGrid f (Cube axes vec)  = (Cube axes $ SV.map (\(x,y) -> f x y) $ 
                                  SV.zip (Grid.toVector axes) vec)

foldl ::(SV.Walker vec, SV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Cube dim label vec a b -> acc
foldl f acc (Cube _ vec) = SV.foldl f acc vec 

foldlWithGrid :: 
  (SV.Walker vec,
   SV.Storage vec (vec [a]), 
   SV.FromList vec,
   SV.Zipper vec,
   SV.Storage vec b,
   SV.Storage vec (ND.Data dim a),
   SV.Storage vec (ND.Data dim a, b),
   SV.Walker (Axis.Strict label vec),
   SV.Storage (Axis.Strict label vec) a,
   SV.Storage (Axis.Strict label vec) (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList (Axis.Strict label vec)) =>
  (acc -> (ND.Data dim a, b) -> acc) -> acc -> Cube dim label vec a b -> acc
foldlWithGrid f acc (Cube axes vec) = SV.foldl f acc $ SV.zip (Grid.toVector axes) vec 

zipWith :: 
  (SV.Zipper vec,
   SV.Storage vec d,
   SV.Storage vec c,
   SV.Storage vec b, 
   Eq (Grid dim label vec a)) =>
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
  Axis.Idx -> Cube (ND.SubDim dim) label vec a b
getSubCube caller (Cube axes vec) (Axis.Idx idx) = Cube (ND.dropFirst (caller |> nc "getSubCube") axes) subVec
  where subVec = SV.slice startIdx l vec
        startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l
        l = mytrace 0 "getSubCube" "l" $ Axis.len $ ND.getFirst (caller |> nc "getSubCube") axes

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
  (ND.Data dim a) ->   
  Interp.Val b
interpolate caller interpFunction ortho coordinates = Interp.combine3 y1 y2 y
  where 
    newCaller = (caller |> (nc "interpolate"))
    axis = mytrace 0 "interpolate" "axis" $ ND.getFirst newCaller $ 
           getAxes $ mytrace 0 "interpolate" "ortho" $ ortho
    subCoordinates = ND.dropFirst newCaller coordinates
    x = ND.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Axis.getSupportPoints axis x
    f idx = interpolate newCaller interpFunction (getSubCube newCaller ortho idx) subCoordinates
    (y1,y2) = if ND.len coordinates >=2 then (f idx1, f idx2) 
              else (Interp.Inter $ lookUp newCaller (ND.Data [idx1]) ortho,
                    Interp.Inter $ lookUp newCaller (ND.Data [idx2]) ortho)
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x

dimension :: ND.Dimensions dim => Cube dim label vec a b -> Int     
dimension (Cube axes _) = ND.num axes
    
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
        axis = ND.getFirst (caller |> nc "nest") $ axes
        l = Axis.len axis
        f idx _ = SV.slice startIdx l vec
          where
            startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l

extract :: 
  (SV.Walker vec,
   SV.Storage vec a,
   SV.Storage vec Axis.Idx,SV.LookupUnsafe vec b,
   SV.FromList vec, 
   SV.Storage vec b, SV.Storage vec Grid.LinIdx, 
   SV.Storage vec (ND.Data dim Axis.Idx),
   SV.Storage vec [Axis.Idx],
   SV.Storage vec (vec [Axis.Idx]),
   SV.Length vec,
   SV.Singleton vec) =>
  Caller -> 
  Cube dim label vec a b -> 
  ND.Data dim2 (ND.Idx) -> 
  Map.Map ND.Idx Axis.Idx -> 
  Cube dim2 label vec a b
extract caller cube@(Cube axes _) dims2Keep dims2Drop = Cube newAxes newVec
  where newAxes = Grid.extract newCaller axes dims2Keep
        newCaller =  caller |> nc "extractCube"
        indexVec = Grid.reductionIndexVector axes dims2Drop
        newVec = SV.map (lookupLinUnsafe cube) indexVec  
    
