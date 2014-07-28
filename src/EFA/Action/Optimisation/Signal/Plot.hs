{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Signal.Plot where

--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Graph as Graph
--import qualified EFA.Flow.Topology as FlowTopo
--import qualified EFA.Action.Flow.Balance as Balance
--import qualified EFA.Flow.Topology.Index as XIdx
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified EFA.Data.Plot.D2.FlowSignal as PlotFSignal
import qualified Graphics.Gnuplot.Frame as Frame
--import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Flow.SequenceState.Index as Idx
--import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.ND as ND
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
--import qualified EFA.Action.Flow.Check as ActFlowCheck
--import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Value.State as ValueState
--import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified EFA.Action.Optimisation.Signal as OptSignal
--import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
--import qualified EFA.Data.Interpolation as Interp
--import qualified EFA.Data.ND.Cube.Map as CubeMap

import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import qualified EFA.Value.Type as Type

import qualified EFA.Data.Plot.D2.FlowSignal as SignalFlowPlot
import qualified EFA.Data.Plot.D2 as PlotD2

import qualified Data.Map as Map
--import qualified Control.Monad as Monad
--import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
              --     merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Signal.Plot"

nc :: FunctionName -> Caller
nc = genCaller modul


plotDemandCycle ::
  (Ord node,
   Ord a,Show node,
   Ord b,
   Arith.Constant a,
   Arith.Constant b,
   Tuple.C b,
   Tuple.C a,
   Atom.C b,
   Atom.C a,
   Type.ToDisplayUnit b,
   Type.GetDynamicType a,
   Type.GetDynamicType b,
   DV.Walker vec,
   DV.Storage vec (ND.Data dim b),DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec a,
   DV.Storage vec b,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  OptSignal.DemandCycle node inst dim vec a b ->
  [DemandAndControl.DemandVar node] ->
  Frame.T (Graph2D.T a b)
plotDemandCycle demandVars cyc = plotDemandCycleMap $ OptSignal.convertToDemandCycleMap demandVars cyc 

plotDemandCycleMap ::
  (Ord b,Show node,
   Ord a,
   Arith.Constant b,
   Arith.Constant a,
   Tuple.C a,
   Tuple.C b,
   Atom.C a,
   Atom.C b,
   Type.ToDisplayUnit b,
   Type.GetDynamicType b,
   Type.GetDynamicType a,
   DV.Walker vec,
   DV.Storage vec b,DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  OptSignal.DemandCycleMap node inst vec a b ->
  Frame.T (Graph2D.T a b)
plotDemandCycleMap cyc = 
  PlotD2.allInOne (PlotD2.labledFrame "DemandCycle")  (\ _ _ -> id) $ 
  concat $ Map.elems $ Map.mapWithKey (\ident sig ->  SignalFlowPlot.toPlotData (Just ident) sig) cyc
  
  
  
plotOptimalSignals ::
  (Ord b,
   Ord a,
   Show id,
   Show label,
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
   DV.Length vec,DV.Storage vec (SignalFlow.TimeStep a),
   DV.FromList vec) =>
  String ->
  Map.Map id (SignalFlow.Signal inst label vec a b) ->
  Frame.T (Graph2D.T a b)
plotOptimalSignals title signalMap  = 
  PlotD2.allInOne (PlotD2.labledFrame title)
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) $     
  PlotFSignal.plotSignalMap signalMap
  where
    legend = Map.fromList $ zip [0..] $ Map.keys signalMap