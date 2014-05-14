{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Data.ND.Cube.Map where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.ND.Cube.Grid as Grid
import qualified EFA.Data.Axis.Strict as Strict
import EFA.Data.ND.Cube.Grid (Grid)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data(Nil,(:>))
import qualified EFA.Signal.Data as SD
import qualified EFA.Reference.Base as Ref
import qualified Data.Map as Map


import EFA.Data.Interpolation as DataInterp

import qualified Prelude as P
import Prelude hiding (zipWith, map, foldl)
import Data.Maybe(fromMaybe)

import EFA.Utility.Trace(mytrace)

modul :: ModuleName
modul = ModuleName "Cube.Map"

nc :: FunctionName -> Caller
nc = genCaller modul

data Cube typ dim label vec a b = Cube {
  getGrid :: Grid typ dim label vec a,
  getData :: Data typ dim vec b } deriving (Show,Eq)

instance (Show label, Ref.ToData (vec a), Ref.ToData (vec b)) =>
         Ref.ToData (Cube typ dim label vec a b) where
  toData (Cube grid dat) = Ref.DoubleType "Cube" (Ref.toData grid) (Ref.toData dat)

data Data typ dim vec a = Data { getVector :: vec a} deriving (Show,Eq)

instance (Ref.ToData (vec a)) => Ref.ToData (Data typ dim vec a) where
  toData (Data vec) = Ref.SingleType "Cube.Data" $ Ref.toData vec

lookupLin ::
  (DV.LookupMaybe vec b,
  Show (vec b)) =>
  Caller -> Cube typ dim label vec a b -> Grid.LinIdx -> b
lookupLin caller (Cube _ (Data vec)) (Grid.LinIdx idx) =
  fromMaybe e $ DV.lookupMaybe vec idx
  where e = merror caller modul "lookupLinear"
            $ "linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec

lookupLinUnsafe ::
  DV.LookupUnsafe vec b =>
  Cube typ dim label vec a b -> Grid.LinIdx -> b
lookupLinUnsafe (Cube _ (Data vec)) (Grid.LinIdx idx) = DV.lookupUnsafe vec idx

lookUp ::
  (DV.LookupMaybe vec b,
   DV.Storage vec a,Show (vec b),
   DV.Length vec) =>
  Caller -> Grid.DimIdx dim -> Cube typ dim label vec a b -> b
lookUp caller idx cube@(Cube grid _) = lookupLin (caller |> nc "lookUp") cube index
  where index = Grid.toLinear grid idx

checkVector ::
  (DV.Storage vec b, DV.Length vec,DV.Storage vec a) =>
  Grid typ dim label vec a -> vec b -> Bool
checkVector (ND.Data grid) vec =  DV.length vec == numberElements
  where numberElements = P.foldl (*) (1) (fmap Strict.len grid)


create ::
  (Ord a,
   DV.Zipper vec,
   DV.Storage vec Bool,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Storage vec b,
   DV.Length vec,
   ND.Dimensions dim
  ) =>
  Caller -> [(label,vec a)] -> vec b -> Cube typ dim label vec a b
create caller xs vec =
  let grid = Grid.create (caller |> (nc "create")) xs
  in if checkVector grid vec
  then Cube grid (Data vec)
       else merror caller modul "create"
            "Vector doesn't contain the right number of elements"


generateWithGrid ::
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec (vec [a]),
   DV.Storage vec a,
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList vec) =>
   (ND.Data dim a -> b) -> Grid typ dim label vec a -> Cube typ dim label vec a b
generateWithGrid f grid =  Cube grid $ Data $ DV.map f (Grid.toVector grid)

map ::
  (DV.Walker vec,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c) -> Cube typ dim label vec a b -> Cube typ dim label vec a c
map f (Cube grid (Data vec)) = (Cube grid $ Data $ DV.map f vec)


mapWithGrid ::
 (DV.Walker vec,
  DV.Storage vec c,DV.Storage vec (vec [a]), DV.FromList vec,
  DV.Zipper vec,
  DV.Storage vec b,
  DV.Storage vec (ND.Data dim a),
  DV.Storage vec a,
  DV.Storage vec [a],
  DV.Singleton vec,
  DV.Storage vec (ND.Data dim a, b)) =>
 (ND.Data dim a -> b -> c) -> Cube typ dim label vec a b -> Cube typ dim label vec a c
mapWithGrid f (Cube grid (Data vec))  = (Cube grid $ Data $ DV.map (\(x,y) -> f x y) $
                                  DV.zip (Grid.toVector grid) $ vec)

foldl ::(DV.Walker vec, DV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Cube typ dim label vec a b -> acc
foldl f acc (Cube _ (Data vec)) = DV.foldl f acc vec

foldlWithGrid ::
  (DV.Walker vec,
   DV.Storage vec (vec [a]),
   DV.FromList vec,
   DV.Zipper vec,
   DV.Storage vec b,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec (ND.Data dim a, b),
   DV.Walker (Strict.Axis typ label vec),
   DV.Storage (Strict.Axis typ label vec) a,
   DV.Storage (Strict.Axis typ label vec) (vec [a]),
   DV.Storage vec a,
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList (Strict.Axis typ label vec)) =>
  (acc -> (ND.Data dim a, b) -> acc) -> acc -> Cube typ dim label vec a b -> acc
foldlWithGrid f acc (Cube grid (Data vec)) = DV.foldl f acc $ DV.zip (Grid.toVector grid) vec

zipWith ::
  (DV.Zipper vec,
   DV.Storage vec d,
   DV.Storage vec c,
   DV.Storage vec b,
   Eq (Grid typ dim label vec a)) =>
  Caller ->
  (b -> c -> d) ->
  Cube typ dim label vec a b ->
  Cube typ dim label vec a c ->
  Cube typ dim label vec a d
zipWith caller f (Cube grid (Data vec)) (Cube grid1 (Data vec1)) =
  if grid == grid1 then Cube grid $ Data $ DV.zipWith f vec vec1
                   else merror caller modul "zipWith" "Grid differ"

-- | Removes the first Dimension
getSubCube ::
  (DV.Storage vec b, DV.Slice vec,
   DV.Storage vec a, DV.Length vec) =>
  Caller ->
  Cube typ dim label vec a b ->
  Strict.Idx -> Cube typ (ND.SubDim dim) label vec a b
getSubCube caller (Cube grid (Data vec)) (Strict.Idx idx) = Cube (ND.dropFirst (caller |> nc "getSubCube") grid) $ Data subVec
  where subVec = DV.slice startIdx l vec
        startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l
        l = mytrace 0 "getSubCube" "l" $ P.foldl (*) 1 $ P.map Strict.len $ ND.toList $ ND.dropFirst (caller |> nc "getSubCube") grid

getSubCubes  :: 
  (DV.Storage vec a,DV.FromList vec,
   DV.Storage vec b,
   DV.Slice vec,
   DV.Length vec)=>
  Caller ->
  Cube typ dim label vec a b -> [(a,Cube typ (ND.SubDim dim) label vec a b)]
getSubCubes caller cube = P.zip vals $ P.map (getSubCube caller cube) $ indexes
  where indexes = P.map Strict.Idx [0..((Strict.len $ ND.getFirst (caller |> nc "getSubCubes") $ getGrid cube)-1)]
        vals = DV.toList $ Strict.getVec $ ND.getFirst (caller |> nc "getSubCubes") $ getGrid cube

interpolate ::
  (Ord a,Arith.Constant b,Num b,DV.LookupMaybe vec b,
   DV.LookupUnsafe vec a,Show (vec b),(Show (vec a)),
   DV.Storage vec a,Show label,
   DV.Length vec,
   DV.Find vec,
   DV.Storage vec b, DV.Slice vec) =>
  Caller ->
  ((a,a) -> (b,b) -> a -> DataInterp.Val b) ->
  Cube typ dim label vec a b ->
  (ND.Data dim a) ->
  DataInterp.Val b
interpolate caller interpFunction cube coordinates = DataInterp.combine3 y1 y2 y
  where
    newCaller = (caller |> (nc "interpolate"))
    axis = mytrace 0 "interpolate" "axis" $ ND.getFirst newCaller $
           getGrid $ mytrace 0 "interpolate" "cube" $ cube
    subCoordinates = ND.dropFirst newCaller coordinates
    x = ND.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Strict.getSupportPoints axis x
    f idx = interpolate newCaller interpFunction (getSubCube newCaller cube idx) subCoordinates
    (y1,y2) = if ND.len coordinates >=2 then (f idx1, f idx2)
              else (DataInterp.Inter $ lookUp newCaller (ND.Data [idx1]) cube,
                    DataInterp.Inter $ lookUp newCaller (ND.Data [idx2]) cube)
    y = interpFunction (x1,x2) (DataInterp.unpack y1,DataInterp.unpack y2) x

dimension :: ND.Dimensions dim => Cube typ dim label vec a b -> Int
dimension (Cube grid _) = ND.num grid

to2DSignal ::
    (DV.Walker vec,
     DV.Storage vec (vec b),
     DV.Storage vec b,
     DV.Slice vec,
     DV.Storage vec a,
     DV.Length vec) =>
    Caller ->
    Cube typ dim label vec a b ->
    Sig.TC Sig.Signal t (SD.Data (vec :> vec :> Nil) b)
to2DSignal caller (Cube grid (Data vec)) = Sig.TC $ SD.Data $ DV.imap f $ Strict.getVec axis
      where
        axis = ND.getFirst (caller |> nc "nest") $ grid
        l = Strict.len axis
        f idx _ = DV.slice startIdx l vec
          where
            startIdx = mytrace 0 "getSubCube" "startIdx" $ idx*l

extract ::
  (DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec Strict.Idx,DV.LookupUnsafe vec b,
   DV.FromList vec,
   DV.Storage vec b, DV.Storage vec Grid.LinIdx,
   DV.Storage vec (ND.Data dim Strict.Idx),
   DV.Storage vec [Strict.Idx],
   DV.Storage vec (vec [Strict.Idx]),
   DV.Length vec,
   DV.Singleton vec) =>
  Caller ->
  Cube typ dim label vec a b ->
  ND.Data dim2 (ND.Idx) ->
  Map.Map ND.Idx Strict.Idx ->
  Cube typ dim2 label vec a b
extract caller cube@(Cube grid _) dims2Keep dims2Drop = Cube newGrid (Data newVec)
  where newGrid = Grid.extract newCaller grid dims2Keep
        newCaller =  caller |> nc "extractCube"
        indexVec = Grid.reductionIndexVector grid dims2Drop
        newVec = DV.map (lookupLinUnsafe cube) indexVec


tupleVec :: 
  (DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec b,
   DV.Storage vec (vec [a]),
   DV.Storage vec (ND.Data dim a, b),
   DV.Singleton vec,
   DV.FromList vec) =>
  Cube typ dim label vec a b -> vec (ND.Data dim a, b)
tupleVec cube = getVector $ getData $ mapWithGrid (\ coordinate x -> (coordinate, x)) cube


valueRange :: 
  (Ord b, DV.Storage vec b, 
   DV.Singleton vec)=>
  Cube typ dim label vec a b -> (b,b)
valueRange cube = DV.minmax $ getVector $ getData cube
  
  