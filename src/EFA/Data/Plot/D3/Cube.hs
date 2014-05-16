{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Data.Plot.D3.Cube {-(
   run,
   signal,
   signalFrameAttr,
   heatmap, xyzrange3d, cbrange, xyzlabel, xyzlabelnode, depthorder,
   paletteGH, paletteGray, paletteHSV, missing, contour,
   Signal,
   Value,
   Labeled, label,
   xy,
   xyBasic,
   xyStyle,
   xyFrameAttr,
   XY,
   surface,
   Surface,
   record,
   recordFrameAttr,
   recordList,
   sequence,
   optKeyOutside,
   stack,
   stackFrameAttr,
   stacks,
   stacksFrameAttr,
   getData,
   genAxLabel
   ) -}
       where

import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND as ND
import qualified EFA.Value.Type as Type

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Data.Vector as DV
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Colour as Colour

import EFA.Signal.Record (Record(Record))
import EFA.Signal.Signal (TC, toSigList, getDisplayType)
import EFA.Signal.Data (Data, (:>), Nil, NestedList)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic (Sum, Product, (~*), Constant)

import qualified EFA.Data.Plot.D3 as PlotD3
import qualified EFA.Data.Plot as DataPlot
import qualified EFA.Value as Value

import qualified EFA.Graph.Topology.Node as Node

import EFA.Report.Typ
          (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import EFA.Utility.Show (showNode)

import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Plot as Plt
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified EFA.Data.Axis.Strict as Strict

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Data.List.Key as Key
import Data.Map (Map)
import Control.Functor.HT (void)
import Data.Foldable (foldMap)
import Data.Monoid (mconcat)

import EFA.Utility.Trace(mytrace)

import Prelude hiding (sequence)

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Plot.Cube"

nc :: FunctionName -> Caller
nc = genCaller modul


surfaceLineSpec :: LineSpec.T
surfaceLineSpec =
   LineSpec.pointSize 0.1 $
   LineSpec.pointType 7 $
   LineSpec.lineWidth 1 $
   LineSpec.deflt

frameAttr ::
--   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   (Graph.C graph) => String -> tcX -> tcY -> Opts.T graph
frameAttr ti x y =
   Opts.title ti $
   Opts.add (Opt.custom "hidden3d" "") ["back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"] $
--   Opts.xLabel (genAxLabel x) $
--   Opts.yLabel (genAxLabel y) $
   Opts.grid True $
   Opts.deflt
   
blankFrameAttr ti =    
   Opts.title ti $
   Opts.add (Opt.custom "hidden3d" "") ["back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"] $
   Opts.grid True $
   Opts.deflt
  

-- TODO:: CUBE Dim2 == Dim3 korrigieren

getD3RangeInfo :: 
  (Show label, Ord a,
   Type.GetDynamicType a,
   Value.ToDouble a,
   DV.Storage vec a,
   DV.FromList vec, 
   Ord b,
   Type.GetDynamicType b,
   Value.ToDouble b,
   DV.Storage vec b,
   DV.Singleton vec) => 
  CubeMap.Cube typ ND.Dim2 label vec a b ->
  PlotD3.D3RangeInfo label
getD3RangeInfo cube@(CubeMap.Cube grid dat) = PlotD3.D3RangeInfo 
                                            (head axesInfo)
                                            (head $ tail axesInfo)
                                            rangeInfo
  where
    axesInfo = map DataPlot.fromAxis $ ND.toList grid
    rangeInfo = DataPlot.fromRange $ (CubeMap.getVector $ CubeMap.getData cube)


basic ::
  (Atom.C b, Show a, Show b,Show label,Type.GetDynamicType a,
                      Value.ToDouble a,
   Atom.C a, Ord b,Ord a,
   Tuple.C b, Tuple.C a, 
   Type.GetDynamicType b, Value.ToDouble b,
   DV.Storage vec (ND.Data ND.Dim1 a, b),
   DV.Storage vec (ND.Data ND.Dim1 a),DV.Slice vec, 
   DV.Length vec,
   DV.FromList vec,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (vec [a]),
   DV.Storage vec b,
   DV.Storage vec (ND.Data ND.Dim2 a),
   DV.Storage vec a,
   DV.Storage vec [a],
   DV.Singleton vec)=>
   Caller ->
   PlotD3.CutInfo id label ->
   CubeMap.Cube typ ND.Dim2 label vec a b ->
   PlotD3.PlotData id label a b
basic caller cut cube@(CubeMap.Cube grid _) = PlotD3.PlotData cut (getD3RangeInfo cube) plot
   where
     subCubes = CubeMap.getSubCubes (caller |> nc "getSubCube") cube
     plot = Plot3D.mesh $ map (\((_,x),subCube) -> map (\(ND.Data [y],z) -> (x,y,z)) $ 
                                               DV.toList $ CubeMap.tupleVec subCube) subCubes


-- TODO: Erweiterung auf ND
-- TODO: Labels generieren
     
class Surface dim label vec a b where
  toPlotData :: Caller ->
             Maybe id ->
             CubeMap.Cube typ dim label vec a b ->
             [PlotD3.PlotData id label a b]

instance 
  (Show b, Type.GetDynamicType a, 
   Value.ToDouble a, Ord a,
   Show a,Ord b, Show label,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (ND.Data ND.Dim2 a),
   DV.Storage vec b,
   DV.Storage vec (vec [a]),
   DV.Storage vec (ND.Data ND.Dim1 a),
   DV.Storage vec (ND.Data ND.Dim1 a, b),
   DV.Slice vec,Type.GetDynamicType b, Value.ToDouble b,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec,
   Tuple.C a,
   Tuple.C b,
   Atom.C a,
   Atom.C b) =>
      Surface ND.Dim2 label vec a b where
   toPlotData caller id cube = [basic caller (PlotD3.NoCut id) cube]
       
instance 
  (Ord b, Ord a,
   Show label,
   Show b,
   Show a,
   Type.GetDynamicType b,
   Type.GetDynamicType a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (ND.Data ND.Dim2 a),
   DV.Storage vec b,
   DV.Storage vec (vec [a]),
   DV.Storage vec (ND.Data ND.Dim1 a),
   DV.Storage vec (ND.Data ND.Dim1 a, b),
   DV.Slice vec,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec,
   Value.ToDouble b,
   Value.ToDouble a,
   Atom.C a,
   Atom.C b,
   Tuple.C a,
   Tuple.C b)=>
        Surface ND.Dim3 label vec a b where
   toPlotData caller ident cube = map f $ CubeMap.getSubCubes caller cube 
     where f ((label,x) , subCube) = basic caller (PlotD3.Cut ident [(label, Value.toDouble x, Type.getDynamicType x)]) subCube


