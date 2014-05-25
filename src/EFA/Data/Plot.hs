{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Data.Plot where

--import qualified EFA.Data.ND.Cube.Map as CubeMap
--import qualified EFA.Data.ND as ND

--import qualified EFA.Signal.Sequence as Sequ
--import qualified EFA.Signal.Signal as S
--import qualified EFA.Signal.Data as D
import qualified EFA.Data.Vector as DV
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Colour as Colour

import qualified EFA.Value.Type as ValueType
import qualified EFA.Value as Value

--import EFA.Signal.Record (Record(Record))
--import EFA.Signal.Signal (TC,
                        --  toSigList,
--                          getDisplayType)
--import EFA.Signal.Data (Data,
                        --(:>), Nil,
--                        NestedList)

import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic (
  --Sum,
--  Product, (~*), Constant)

--import qualified EFA.Graph.Topology.Node as Node

--import EFA.Report.Typ
--          (TDisp,
        --   DisplayType(Typ_T),
--           getDisplayUnit, getDisplayTypName)
--import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

--import qualified EFA.Report.Format as Format
--import EFA.Report.FormatValue (FormatValue, formatValue)

--import EFA.Utility.Show (showNode)

import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Plot as Plt
--import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
--import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
--import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Graph as Graph
--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple

--import qualified Graphics.Gnuplot.LineSpecification as LineSpec
--import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
--import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

--import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Axis as Axis

--import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Foldable as Foldable
--import qualified Data.List.Key as Key
--import Data.Map (Map)
import Control.Functor.HT (void)
-- import Data.Foldable (foldMap)
--import Data.Monoid (mconcat)

--import EFA.Utility.Trace(mytrace)
import qualified EFA.Value.Type as Type
import Prelude hiding (sequence)

import EFA.Utility(Caller,
               --    merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
--import Data.List as List

modul :: ModuleName
modul = ModuleName "Data.Plot"

nc :: FunctionName -> Caller
nc = genCaller modul

data AxisInfo label a = AxisInfo [Maybe label] (Value.Range a) (Tics  a) [ValueType.Dynamic] deriving Show
data Tics a = NoTics | Tics [a]  deriving Show

fromAxis ::
  (Arith.Constant a, DV.Storage vec a, Axis.GetInfo axis vec a,
   DV.FromList vec) =>
  (axis:: * -> * -> (* -> *) -> * -> *) typ label vec a -> AxisInfo label a
fromAxis axis =
  AxisInfo [Just $ Axis.getLabel axis]
            (fmap (Type.toDisplayUnit' typ) $ Axis.getRange axis)
            (Tics $ map (Type.toDisplayUnit' typ) $ DV.toList $ Axis.getVector axis)
            [typ]
  where
  typ = Axis.getType axis

fromRange ::
  (Ord a, Arith.Constant a,
   DV.Storage vec a, DV.Singleton vec,
   Type.GetDynamicType a) =>
  vec a -> AxisInfo label a
fromRange dataVec =
  AxisInfo
  [Nothing]
  (fmap (Type.toDisplayUnit' typ) range)
  NoTics
  [typ]
     where range = (\(x,y) -> Value.Range x y) $ DV.minmax dataVec
           typ = ValueType.getDynamicType range


-- TODO: Tics better with set datatype ?
combineTics :: Ord a => Tics a -> Tics  a -> Tics a
combineTics NoTics NoTics = NoTics
combineTics (Tics xs) NoTics = Tics xs
combineTics NoTics (Tics xs) = Tics xs
combineTics (Tics xs) (Tics xs1) = Tics $ List.sort $ xs ++ xs1


combineList :: Ord a => [AxisInfo label a] -> AxisInfo label a
combineList (x:xs) = foldl combine x xs

combine :: Ord a =>
  AxisInfo label a ->
  AxisInfo label a ->
  AxisInfo label a
combine (AxisInfo label range tic typ) (AxisInfo label1 range1 tic1 typ1) =
          (AxisInfo (label++label1)
           (Value.combineRange range range1)
           (combineTics tic tic1)
           (typ++typ1))
makeAxisLabel :: Show label => AxisInfo label a -> String
makeAxisLabel (AxisInfo labels _ _ types) =
  if all (== head labelList) labelList
  then head labelList
  else  List.intercalate "," labelList
  where f (Just l) t = (show l) ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
        f (Nothing) t = "-" ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
        labelList = zipWith f labels types

makeAxisLabelWithIds  :: Show id => [Maybe id] -> AxisInfo label a -> String
makeAxisLabelWithIds xs (AxisInfo _ _ _ types) =
  if all (== head labelList) labelList
  then head labelList
  else  List.intercalate "," labelList
    where f (Just l) t = (show l) ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
          f (Nothing) t = "-" ++ " [" ++ (Type.showUnit $ Type.getDisplayUnit t) ++ "] "
          labelList = zipWith f xs types

-- | Generic IO Commands ---------------------------------------------------------------
run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal frameAttr plt =
   void $ Plot.plotSync terminal $ Frame.cons frameAttr plt

data PlotInfo id a = PlotInfo (Maybe id) (Maybe a)


allInOneIO :: 
  (Graph.C graph, Terminal.C terminal) =>
  terminal -> 
  ([a] -> Opts.T graph) -> 
  ((Int, a) -> Plt.T graph) 
  -> [a] 
  -> IO()
allInOneIO terminal setFrameStyle makeGraph xs =
  run terminal (setFrameStyle xs) $ (Foldable.fold $ map makeGraph $ zip [0..] xs)


{-
eachIO :: (Terminal.C terminal, Atom.C a, Atom.C b)=>
  terminal ->
  ([PlotData id label a b] ->  Opts.T (Graph3D.T a a b)) ->
  (Int -> PlotData id label a b -> (LineSpec.T -> LineSpec.T)) ->
  [PlotData id label a b] ->
  IO()
eachIO terminal makeFrameStyle setGraphStyle xs =
  mapM_ (DataPlot.run terminal (makeFrameStyle xs)) $ map g $ zip [0..] xs
  where g (idx,plotData@(PlotData _ _ plot)) = fmap (Graph3D.lineSpec $ setGraphStyle idx plotData $ LineSpec.deflt) plot
-}

{-
data PlotData2 id label a b plot =
  PlotData2 (PlotInfo id (PlotInfoContent id label a b plot)) (RangeInfo id label a b plot) plot
  
type family PlotInfoContent id label a b plot   
type family RangeInfo id label a b plot   
  
type family PlotType z

class ToPlotData id z plot where
  toPlotData2 :: Caller -> Maybe id -> z -> (PlotType z) --[PlotData2 id2 label a b plot]
-}

data PlotData2 id z  =
  PlotData2 (PlotInfo id (PlotInfoContent z)) (RangeInfo z) (PlotType z)
  
type family PlotInfoContent z
type family RangeInfo z
type family PlotType z

class ToPlotData id zin z where
  toPlotData2 :: Caller -> Maybe id -> zin -> [PlotData2 id z]