{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Plot.D2.Curve where

--import qualified EFA.Value as Value
import qualified EFA.Value.Type as Type

import qualified EFA.Data.Vector as DV
import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Plot as DataPlot
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.OD.Curve as Curve

import qualified EFA.Data.Axis.Strict as Strict
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Data.Map as Map 

instance 
  (Ord a,
   Arith.Constant a,
   Type.GetDynamicType a,
   DV.Storage vec a,
   DV.Length vec,
   DV.FromList vec, 
   Ord b,
   Arith.Constant b,
   Type.GetDynamicType b,
   DV.Storage vec b,
   DV.Singleton vec) =>
  PlotD2.GetRangeInfo Curve.Curve label vec a b where
  getRangeInfo curve = PlotD2.RangeInfo axRange valRange
    where axRange = DataPlot.fromAxis $ Curve.getAxis curve
          valRange = DataPlot.fromRange $ Curve.getData curve

basic :: 
  (Ord b,Tuple.C a, Tuple.C b, Type.ToDisplayUnit b,
   Arith.Constant b,(DV.Walker vec),Atom.C b, Atom.C a,
   Type.GetDynamicType b,
   DV.Storage vec b,
   DV.Singleton vec, 
   Ord a,
   Arith.Constant a,
   Type.GetDynamicType a,
   DV.Storage vec a,
   DV.Length vec,
   DV.FromList vec) =>
  Maybe id ->
  Curve.Curve inst label vec a b -> 
  PlotD2.PlotData id info label a b 
basic ident curve = PlotD2.PlotData info range  (Plot2D.list Graph2D.lines $ zip xdata ydata)
  where info = DataPlot.PlotInfo ident Nothing
        range = PlotD2.getRangeInfo curve
        axis = Curve.getAxis curve
        xdata = DV.toList $ DV.map (Type.toDisplayUnit' (Strict.getType axis)) $ Strict.getVec axis
        ydata = DV.toList $ DV.map Type.toDisplayUnit $ Curve.getData curve                         
              

toPlotData :: 
  (Ord a,
   Ord b,
   Arith.Constant a,
   Arith.Constant b,
   Atom.C a,
   Atom.C b,
   Tuple.C b,
   Tuple.C a,
   Type.ToDisplayUnit b,
   Type.GetDynamicType a,
   Type.GetDynamicType b,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec b,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  Maybe id ->
  Curve.Curve inst label vec a b ->
  [PlotD2.PlotData id info label a b]
toPlotData ident curve = [basic ident curve]

toPlotDataMap ::
  (Ord b,
   Ord a,
   Arith.Constant b,
   Arith.Constant a,
   Atom.C b,
   Atom.C a,
   Tuple.C a,
   Tuple.C b,
   Type.ToDisplayUnit b,
   Type.GetDynamicType b,
   Type.GetDynamicType a,
   DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec)=>
 (PlotD2.ToPlotData Curve.Curve info label vec a b) =>
  Map.Map key (Curve.Curve inst label vec a b) ->
  [PlotD2.PlotData key info label a b]
toPlotDataMap curveMap = concatMap snd $ Map.toList $ Map.mapWithKey (\key x -> toPlotData (Just key) x) curveMap