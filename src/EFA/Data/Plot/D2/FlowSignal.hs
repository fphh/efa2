{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Plot.D2.FlowSignal where

--import qualified EFA.Value as Value
import qualified EFA.Value.Type as Type

import qualified EFA.Data.Vector as DV
import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Plot as DataPlot
import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Data.OD.Curve as Curve
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

import qualified EFA.Data.Axis.Strict as Strict
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Data.Map as Map 

{-
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
  PlotD2.GetRangeInfo SignalFlow.Signal label vec a b where
-}

getRangeInfo::
  (Ord a,
   Ord b,
   Arith.Constant a,
   Arith.Constant b,
   Type.GetDynamicType a,
   Type.GetDynamicType b,
   DV.Storage vec a,
   DV.Storage vec b,
   DV.Singleton vec,
   DV.Length vec,
   DV.Walker vec,
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
 SignalFlow.Signal inst label vec a b ->
 PlotD2.RangeInfo label a b 
getRangeInfo signal= PlotD2.RangeInfo axRange valRange
    where axRange = DataPlot.fromAxis $ Strict.map SignalFlow.getMidTime $ SignalFlow.getTime signal
          valRange = DataPlot.fromRange $  SignalFlow.getVector $ SignalFlow.getData signal

basic :: 
  (Ord b,Tuple.C a, Tuple.C b, Type.ToDisplayUnit b,
   Arith.Constant b,(DV.Walker vec),Atom.C b, Atom.C a,
   DV.Storage vec (SignalFlow.TimeStep a),
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
  SignalFlow.Signal inst label vec a b -> 
  PlotD2.PlotData id info label a b 
basic ident signal = PlotD2.PlotData info range  (Plot2D.list Graph2D.lines $ zip xdata ydata)
  where info = DataPlot.PlotInfo ident Nothing
        range = getRangeInfo signal
        time = SignalFlow.getTime signal
        xdata = DV.toList $ DV.map (Type.toDisplayUnit' (Strict.getType time)) $ 
                Strict.getVec $ Strict.map SignalFlow.getMidTime time
        ydata = DV.toList $ DV.map Type.toDisplayUnit $ SignalFlow.getVector $ SignalFlow.getData signal                         

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
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
  Maybe id ->
  SignalFlow.Signal inst label vec a b ->
  [PlotD2.PlotData id info label a b] 
toPlotData ident signal = [basic ident signal]

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
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
  Map.Map key (SignalFlow.Signal inst label vec a b) ->
  [PlotD2.PlotData key info label a b]
toPlotDataMap signalMap = concatMap snd $ Map.toList $ Map.mapWithKey (\key x -> toPlotData (Just key) x) signalMap


plotHRecord::
  (Ord a,
   Ord b,
   Arith.Constant a,
   Arith.Constant b,
   Tuple.C b,
   Tuple.C a,
   Atom.C a,
   Atom.C b,
   Type.ToDisplayUnit b,
   Type.GetDynamicType a,
   Type.GetDynamicType b,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec b,
   DV.Singleton vec,
   DV.Length vec,
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
  SignalFlow.HRecord key inst label vec a b ->
   [PlotD2.PlotData key info label a b]
plotHRecord record = map (\(key,sig) -> basic (Just key) sig) $ SignalFlow.hRecordToList record


plotSignalMap ::
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
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
  Map.Map id (SignalFlow.Signal inst label vec a b) ->
  [PlotD2.PlotData id info label a b]
plotSignalMap m = map (\(key,sig) -> basic (Just key) sig) $ Map.toList m