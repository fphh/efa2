{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Data.Plot {-(
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
modul = ModuleName "Data.Plot"

nc :: FunctionName -> Caller
nc = genCaller modul


-- | Generic IO Commands ---------------------------------------------------------------
run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal frameAttr plt =
   void $ Plot.plotSync terminal $ Frame.cons frameAttr plt

data RangeInfo a = NoData | PlotRange (a,a) | PlotAxis [a]
type AxisInfo a = (Maybe String, RangeInfo a)
data CutInfo dim label a = NoCut | Cut (ND.Data dim (label,a))

data PlotInfo a b c = NoInfo | PlotInfo (AxisInfo a) (AxisInfo b) (RangeInfo c)

-- TODO -- check if show on Label is right or is FormatValue correct ?
getAxisInfo ::
  (Show label, 
   DV.Storage vec a, 
   DV.FromList vec) =>  
  Strict.Axis typ label vec a -> AxisInfo a
getAxisInfo axis = 
  (Just $ show $ Strict.getLabel axis, 
   PlotAxis $ DV.toList $ Strict.getVec axis)


{-
-- | Example how to generate frame attributes

frameAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
framAttr ti x =
   Opts.title ti $
   Opts.xLabel "Signal Index []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt
-}

-- TODO check if AxisLabel is reusable 


-- | Class to generate Axis Labels

class Atom.C (Value tc) => AxisLabel tc where
   type Value tc :: *
   genAxLabel :: tc -> String

instance (TDisp t, Atom.C (D.Value c)) => AxisLabel (TC s t c) where
   type Value (TC s t c) = D.Value c
   genAxLabel x =
      let dispType = getDisplayType x
      in  getDisplayTypName dispType ++
             " [" ++ (show $ getDisplayUnit dispType) ++ "]"

instance (AxisLabel tc) => AxisLabel [tc] where
   type Value [tc] = Value tc
   genAxLabel x = genAxLabel $ head x


-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

getData ::
   (TDisp typ, D.FromList c, D.Map c, D.Storage c a, Constant a) =>
   TC s typ (Data c a) -> NestedList c a
getData x = S.toList $ S.map (~* Arith.fromRational s) x
   where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x


-- | Function to simplify linecolor setting

lineColour :: Colour.Name -> LineSpec.T -> LineSpec.T
lineColour = LineSpec.lineColor . ColourSpec.name . Colour.unpackName


-- | Plotting Surfaces -------------------------------------------------------------------------


surfaceLineSpec :: LineSpec.T
surfaceLineSpec =
   LineSpec.pointSize 0.1 $
   LineSpec.pointType 7 $
   LineSpec.lineWidth 1 $
   LineSpec.deflt

surfFrameAttr ::
   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   String -> tcX -> tcY -> Opts.T graph
surfFrameAttr ti x y =
   Opts.title ti $
   Opts.add (Opt.custom "hidden3d" "") ["back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"] $
   Opts.xLabel (genAxLabel x) $
   Opts.yLabel (genAxLabel y) $
   Opts.grid True $
   Opts.deflt
   
blankFrameAttr ti =    
   Opts.title ti $
   Opts.add (Opt.custom "hidden3d" "") ["back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"] $
   Opts.grid True $
   Opts.deflt
  

surfaceBasic ::
  (Atom.C b, Show a, Show b,Show label,
   Atom.C a, Ord b,
   Tuple.C b, Tuple.C a, 
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
   CubeMap.Cube typ ND.Dim2 label vec a b ->
   (PlotInfo a a b, Plot3D.T a a b)
surfaceBasic caller cube@(CubeMap.Cube grid _)= 
  (plotInfo, Plot3D.mesh $ map formatData $ subCubes)
   where
     plotInfo = PlotInfo (head axesInfo) (head $ tail axesInfo) (PlotRange $ CubeMap.valueRange cube)
     subCubes = CubeMap.getSubCubes (caller |> nc "getSubCube") cube
     formatData (x,subCube) = map (\ (dimData,z) -> (x,ND.unsafeLookup dimData $ ND.Idx 0, 
                                        z)) $ DV.toList $ CubeMap.tupleVec subCube
     axesInfo = map getAxisInfo $ ND.toList grid


-- TODO: Erweiterung auf ND
-- TODO: Labels generieren
     
class Surface dim label vec a b where
  surface :: Caller ->
             (LineSpec.T -> LineSpec.T) ->
             CubeMap.Cube typ dim label vec a b ->
             [(CutInfo dim label a, PlotInfo a a b,Plot3D.T a a b)]

instance 
  (Show b,
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
   DV.Slice vec,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec,
   Tuple.C a,
   Tuple.C b,
   Atom.C a,
   Atom.C b) =>
      Surface ND.Dim2 label vec a b where
   surface caller opts cube = [(NoCut,plotInfo,plot)]
     where (plotInfo, plot) = surfaceBasic caller cube
       

{-
instance 
  (Show b,Format.Format [Char], FormatValue a,
   Show a,Ord b, Show label,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec [a],
   DV.Storage vec (ND.Data ND.Dim2 a),
   DV.Storage vec (vec [a]),
   DV.Storage vec (ND.Data ND.Dim1 a),
   DV.Storage vec (ND.Data ND.Dim1 a, b),
   DV.Singleton vec,
   Tuple.C a,
   Tuple.C b,
   Atom.C a,
   Atom.C b, 
   DV.Storage vec a,
   DV.Storage vec b,
   DV.Slice vec,
   DV.Length vec,
   DV.FromList vec)=>
       Surface ND.Dim3 label vec a b where
   surface caller opts cube = map f $ CubeMap.getSubCubes caller cube 
     where f (xVal , subCube) = (DimInfo (Dim.Data , surfaceBasic caller subCube)
           where g PlotInfo 

-}