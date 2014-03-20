{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Ortho where

import EFA.Utility(Caller)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Map.Dimension as Dim
import EFA.Signal.Map.Dimension(Dim2)
import qualified EFA.Signal.Map.Axes as Axes
import EFA.Signal.Map.Axes (Axes)

import EFA.Signal.Interp as Interp

import qualified Prelude as P
import Prelude hiding (zipWith, map, foldl)
import qualified Data.Vector as V
import Data.Maybe(fromMaybe)

import EFA.Utility.Trace(mytrace)


data Ortho dim vec a b = Ortho {
  getAxes :: Axes dim vec a,
  getVector :: vec b } deriving Show

newtype LinIdx = LinIdx {getInt:: Int} deriving Show

toLinear ::
 (SV.Storage vec a, SV.Length vec)=>
  Axes dim vec a -> Axes.DimIdx dim -> LinIdx
toLinear (Dim.Data xs) (Dim.Data indices) = LinIdx $
  P.foldl (+) (0) $ P.zipWith (*)
  (P.map Axes.getInt indices) dimensionMultiplicators
  where dimensionMultiplicators = (init $ P.map Axes.len xs) ++ [1]

lookupLin ::
  (SV.LookupMaybe vec b,
  Show (vec b)) =>
  Caller -> Ortho dim vec a b -> LinIdx -> b
lookupLin caller (Ortho _ vec) (LinIdx idx) = fromMaybe (error m) $ SV.lookupMaybe vec idx
  where m = "Error in LookupLinear called by " ++ caller ++
            " - linear Index out of Bounds - Index : " ++ show idx ++"- Vector: "++ show vec

lookUp ::
  (SV.LookupMaybe vec b,
   SV.Storage vec a,Show (vec b),
   SV.Length vec) =>
  Caller -> Axes.DimIdx dim -> Ortho dim vec a b -> b
lookUp caller idx ortho@(Ortho axes _) = lookupLin (caller ++">lookUp") ortho index
  where index = toLinear axes idx

checkVector ::
  (SV.Storage vec b, SV.Length vec,SV.Storage vec a) =>
  Axes dim vec a -> vec b -> Bool
checkVector (Dim.Data axes) vec =  SV.length vec == numberElements
  where numberElements = P.foldl (*) (1) (fmap Axes.len axes)


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
  Caller -> [vec a] -> vec b -> Ortho dim vec a b
create caller xs vec =
  let axes = Axes.create (caller++">genOrtho") xs
  in if checkVector axes vec
  then Ortho axes vec
       else error ("Error in Ortho.genOrtho called by " ++
            caller ++ " - Vector doesn't contain the right number of elements")

map ::
  (SV.Walker vec,
   SV.Storage vec c,
   SV.Storage vec b) =>
  (b -> c) -> Ortho dim vec a b -> Ortho dim vec a c
map f (Ortho axes vec) = (Ortho axes $ SV.map f vec)


mapWithAxes ::
 (SV.Walker vec,
  SV.Storage vec c,
  SV.Zipper vec,
  SV.Storage vec b,
  SV.Storage vec (Dim.Data dim a),
  SV.Walker (Axes.Axis vec),
  SV.Storage (Axes.Axis vec) a,
  SV.Storage (Axes.Axis vec) (vec [a]),
  SV.Storage vec a,
  SV.Storage vec [a],
  SV.Singleton vec,
  SV.FromList (Axes.Axis vec),
  SV.Storage vec (Dim.Data dim a, b)) =>
 (Dim.Data dim a -> b -> c) -> Ortho dim vec a b -> Ortho dim vec a c
mapWithAxes f (Ortho axes vec)  = (Ortho axes $ SV.map (\(x,y) -> f x y) $
                                  SV.zip (Axes.generate axes) vec)

foldl ::(SV.Walker vec, SV.Storage vec b)=>
 (acc -> b -> acc) -> acc -> Ortho dim vec a b -> acc
foldl f acc (Ortho _ vec) = SV.foldl f acc vec

foldlWithAxes ::
  (SV.Walker vec,
   SV.Zipper vec,
   SV.Storage vec b,
   SV.Storage vec (Dim.Data dim a),
   SV.Storage vec (Dim.Data dim a, b),
   SV.Walker (Axes.Axis vec),
   SV.Storage (Axes.Axis vec) a,
   SV.Storage (Axes.Axis vec) (vec [a]),
   SV.Storage vec a,
   SV.Storage vec [a],
   SV.Singleton vec,
   SV.FromList (Axes.Axis vec)) =>
  (acc -> (Dim.Data dim a, b) -> acc) -> acc -> Ortho dim vec a b -> acc
foldlWithAxes f acc (Ortho axes vec) = SV.foldl f acc $ SV.zip (Axes.generate axes) vec

zipWith ::
  (SV.Zipper vec,
   SV.Storage vec d,
   SV.Storage vec c,
   SV.Storage vec b,
   Eq (Axes dim vec a)) =>
  Caller ->
  (b -> c -> d) ->
  Ortho dim vec a b ->
  Ortho dim vec a c ->
  Ortho dim vec a d
zipWith caller f (Ortho axes vec) (Ortho axes1 vec1) =
  if axes == axes1 then Ortho axes $ SV.zipWith f vec vec1
                   else error ("Error in Ortho.zipWith called by " ++ caller ++
                               "- Axes differ")


-- | Removes the first Dimension
getSubOrtho ::
  (SV.Storage vec b, SV.Slice vec,
   SV.Storage vec a, SV.Length vec) =>
  Caller ->
  Ortho dim vec a b ->
  Axes.Idx -> Ortho (Dim.SubDim dim) vec a b
getSubOrtho caller (Ortho axes vec) (Axes.Idx idx) = Ortho (Dim.dropFirst (caller++">getSubOrtho") axes) subVec
  where subVec = SV.slice startIdx l vec
        startIdx = mytrace 0 "getSubOrtho" "startIdx" $ idx*l
        l = mytrace 0 "getSubOrtho" "l" $ Axes.len $ Dim.getFirst (caller++">getSubOrtho") axes

interpolate ::
  (Ord a,Arith.Constant b,Num b,SV.LookupMaybe vec b,
   SV.UnsafeLookup vec a,Show (vec b),(Show (vec a)),
   SV.Storage vec a,
   SV.Length vec,
   SV.Find vec,
   SV.Storage vec b, SV.Slice vec) =>
  Caller ->
  ((a,a) -> (b,b) -> a -> Interp.Val b) ->
  Ortho dim vec a b ->
  (Dim.Data dim a) ->
  Interp.Val b
interpolate caller interpFunction ortho coordinates = Interp.combine3 y1 y2 y
  where
    newCaller = (caller ++ ">Ortho.interpolate")
    axis = mytrace 0 "interpolate" "axis" $ Dim.getFirst newCaller $
           getAxes $ mytrace 0 "interpolate" "ortho" $ ortho
    subCoordinates = Dim.dropFirst newCaller coordinates
    x = Dim.getFirst newCaller $ coordinates
    ((idx1,idx2),(x1,x2)) = Axes.getSupportPoints axis x
    f idx = interpolate newCaller interpFunction (getSubOrtho newCaller ortho idx) subCoordinates
    (y1,y2) = if Dim.len coordinates >=2 then (f idx1, f idx2)
              else (Interp.Inter $ lookUp newCaller (Dim.Data [idx1]) ortho,
                    Interp.Inter $ lookUp newCaller (Dim.Data [idx2]) ortho)
    y = interpFunction (x1,x2) (Interp.unpack y1,Interp.unpack y2) x



