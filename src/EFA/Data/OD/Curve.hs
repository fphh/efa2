module EFA.Data.OD.Curve where

import qualified EFA.Value as Value
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interpolation
import qualified EFA.Data.Vector as DV

import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple


import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

import qualified EFA.IO.TableParserTypes as ParseTable
import qualified EFA.Value.Type as Type

import qualified Data.List as List
import qualified Data.Map as Map

import Prelude hiding (map) 
import qualified Prelude as P

modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

-- TODO: generate OrdData -DataType-Wrapper for x which prevents swapping x and y in all interpolation and scaling stuff

data Curve inst label vec a b = Curve {getAxis :: Strict.Axis inst label vec a, 
                                       getData :: vec b} deriving Show

type Map key inst label vec a b = Map.Map key (Curve inst label vec a b)                               

curvesfromParseTableMap :: (DV.Storage vec a, DV.FromList vec) =>
  Caller ->
  ParseTable.Map a ->
  Map String inst String vec a a
curvesfromParseTableMap caller parseTable = Map.mapWithKey f parseTable
  where f name table = fromParseTable (caller |> nc "makeCollection") table Type.N name


fromParseTable :: (DV.Storage vec a, DV.FromList vec) =>
  Caller ->
  ParseTable.T a -> Type.Dynamic -> label ->
  Curve inst label vec a a

fromParseTable _ (ParseTable.T (_,2) tableData) typ label = 
  Curve (Strict.Axis label typ 
         ( DV.fromList $ head columns)) 
         ( DV.fromList $ last columns) 
    where columns = List.transpose tableData
fromParseTable caller (ParseTable.T (_,_) _) _ _ = 
  merror caller modul "fromParseTable" "Function only works for two columns"

lookupUnsafe :: (DV.LookupUnsafe vec b) => Curve inst label vec a b -> Strict.Idx -> b
lookupUnsafe curve (Strict.Idx idx) = DV.lookupUnsafe (getData curve) idx


interpolate :: 
  (Eq a, Show a, Arith.Product a, Arith.Constant a,Show label,
   Ord a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec) =>
  Caller -> Interpolation.Method a -> Interpolation.ExtrapMethod a -> 
  Curve inst label vec a a -> (Interpolation.Val a) -> (Interpolation.Val a)
interpolate caller inmethod exmethod curve x = case x of 
  Interpolation.Invalid xs -> Interpolation.Invalid xs
  _  -> result
  where
    result = Interpolation.dim1 (caller |> nc "interpolate") inmethod exmethod (show label) (x1, x2) (y1, y2) x
    label = Strict.getLabel $ getAxis curve
    ((idx1,idx2),(x1,x2)) = Strict.getSupportPoints (getAxis curve) (Interpolation.unpack x)
    y1 = lookupUnsafe curve idx1
    y2 = lookupUnsafe curve idx2
    
    
map :: 
  (DV.Walker vec,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c) -> Curve inst label vec a b -> Curve inst label vec a c
map f (Curve axis vec) = Curve axis $ DV.map f vec      

data ModifyOps a = 
  FlipX |
  FlipY |
  RecipY |
  Scale a a |
--  Offset a a |
  AddPntsL [(a,a)] |
  AddPntsR [(a,a)] 
--  AddAtZero b 
  deriving (Show) 

modify :: 
  (Arith.Product a, DV.Walker vec, DV.Storage vec a,
   DV.Singleton vec, DV.Reverse vec, DV.FromList vec) =>
  [ModifyOps a] -> 
  Curve inst label vec a a -> Curve inst label vec a a
modify xs curve = foldl (\crv op  -> modi op crv) curve xs 

modi :: 
  (Arith.Sum a,
   DV.Singleton vec, 
   DV.FromList vec,
   Arith.Product a,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Reverse vec) => 
  ModifyOps a -> Curve inst label vec a a -> Curve inst label vec a a      
modi FlipX  curve = flipX  curve
modi FlipY  curve = flipY  curve
modi RecipY  curve = recipY  curve
modi (Scale x y)  curve = scale x y  curve
modi (AddPntsL xs)  curve = addPntsL xs  curve
modi (AddPntsR xs)  curve = addPntsR xs  curve
-- modi (ModifLabel f) curve = modifyLabelWith f curve
-- TODO : mod (AddZero x y)  curve = Curve axis vec

flipX :: 
  (Arith.Sum a, DV.Walker vec, DV.Storage vec b, DV.Storage vec a,
   DV.Reverse vec) =>
  Curve inst label vec a b -> Curve inst label vec a b
flipX (Curve axis vec) = Curve (Strict.flip axis) (DV.reverse vec)

flipY ::  
  (Arith.Sum b, DV.Walker vec, DV.Storage vec b) =>
  Curve inst label vec a b -> Curve inst label vec a b

flipY (Curve axis vec) = Curve axis (DV.map Arith.negate vec)

recipY :: 
  (Arith.Product b, DV.Walker vec, DV.Storage vec b) =>
  Curve inst label vec a b -> Curve inst label vec a b
recipY (Curve axis vec) = Curve axis (DV.map Arith.recip vec)

scale :: 
  (Arith.Product b, Arith.Product a, DV.Walker vec, DV.Storage vec b,
   DV.Storage vec a) =>
  a -> b -> Curve inst label vec a b -> Curve inst label vec a b
scale x y (Curve axis vec) = Curve (Strict.scale x axis) (DV.map (Arith.~*y) vec)

addPntsL :: 
  (DV.Storage vec b, DV.Storage vec a, DV.Singleton vec,
   DV.FromList vec) =>
  [(a, b)] -> Curve inst label vec a b -> Curve inst label vec a b
addPntsL xs (Curve axis vec) = Curve (Strict.addLeft (P.map fst xs) axis) 
                               (DV.append (DV.fromList $ P.map snd xs) vec)

addPntsR ::
  (DV.Storage vec b, DV.Storage vec a, DV.Singleton vec,
   DV.FromList vec) =>
  [(a, b)] -> Curve inst label vec a b -> Curve inst label vec a b
addPntsR xs (Curve axis vec) = Curve (Strict.addRight axis $ P.map fst xs) 
                               (DV.append vec $ DV.fromList $ P.map snd xs)

combine ::  
  (Eq label, Ord a, DV.Storage vec b, DV.Storage vec a,
   DV.Singleton vec) =>
  Caller
  -> Curve inst label vec a b
  -> Curve inst label vec a b
  -> Curve inst label vec a b
combine caller (Curve axis vec) (Curve axis1 vec1) = 
  Curve (Strict.combine (caller |> nc "combine") axis axis1) (DV.append vec vec1)


modifyLabelWith :: 
  (label -> label1) ->  
  Curve inst label vec a b ->
  Curve inst label1 vec a b
modifyLabelWith f (Curve axis vec) = (Curve (Strict.modifyLabelWith f axis) vec)


getValueRange :: 
  (Ord b, DV.Storage vec b, DV.Singleton vec) => 
  Curve inst label vec a b -> Value.Range b
getValueRange (Curve _ vec) = Value.getValueRange vec
