{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Action.Optimisation.Cube.Sweep.Plot where


import qualified EFA.Action.Optimisation.Cube.Sweep.Access as SweepAccess
import EFA.Utility(Caller,
                   merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
--import qualified EFA.Graph as Graph 
import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
--import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.Flow.Optimality as FlowOpt
--import qualified EFA.Action.Flow.Balance as FlowBal
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
--import qualified EFA.Data.Axis.Strict as Strict
--import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
-- import qualified EFA.Flow.Topology as FlowTopo
--import qualified EFA.Data.Interpolation as Interp
--import qualified EFA.Application.Utility as AppUt
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Data.OD.Signal.Flow as SignalFlow
--import EFA.Application.Utility (quantityTopology)
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import EFA.Application.Optimisation.Params (Name)
--import qualified EFA.Application.Optimisation.Params as Params
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame as Frame
--import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Flow.Topology.Index as XIdx
--import qualified EFA.Flow.Topology.Variable as Variable
--import EFA.Flow.Topology.Absolute ( (.=), 
--                                    (=.=) )

--import qualified EFA.Flow.Absolute as EqAbs
--import qualified EFA.Flow.Absolute as EqAbs

import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.RecordIndex as RecIdx
--import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
--import qualified EFA.Flow.Topology.Record as TopoRecord

--import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))

--import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
--import qualified Data.Foldable as Fold
-- import Data.Map as (Map)
--import Data.Monoid((<>))
--import qualified EFA.Data.OrdData as OrdData
--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Data.Plot.D3.Cube as PlotCube
import qualified EFA.Data.Plot.D3 as PlotD3

--import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

--import qualified EFA.Action.Utility as ActUt
--import qualified EFA.Action.Optimisation.Cube.Solve as CubeSolve

import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative

--import qualified EFA.Flow.Topology as FlowTopo

-- TODO: Modul so verallgemeinern, dass mit verschiedenen Datentypen gesweept werden kann
modul :: ModuleName
modul = ModuleName "DoubleSweep"

nc :: FunctionName -> Caller
nc = genCaller modul

plotVariation :: 
  (Eq node,
   Ord node,
   DV.Storage demVec b,
   DV.LookupUnsafe srchVec b,
   Show node,
   Eq (srchVec a),
   PlotCube.ToPlotData CubeMap.Cube srchDim (DemandAndControl.Var node) srchVec a b,
   DV.Walker demVec,
   DV.Storage demVec (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a b),
   DV.Storage demVec (Collection.Collection (DemandAndControl.Var node) 
                      (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a b)),
   DV.LookupUnsafe demVec (Collection.Collection (DemandAndControl.Var node) 
                           (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a b)),
   Eq (demVec a),
   PlotCube.ToPlotData
   CubeMap.Cube demDim (DemandAndControl.Var node) demVec a b) =>
  Caller ->
  CubeSweep.Variation node inst demDim srchDim demVec srchVec a b ->
  ([PlotD3.PlotData (DemandAndControl.Var node) (DemandAndControl.Var node) a b],
   [PlotD3.PlotData (DemandAndControl.Var node) (DemandAndControl.Var node) a b])
plotVariation caller sweep = 
  (PlotCube.plotCollection newCaller demandCubes, PlotCube.plotCollection newCaller searchCubes)
  where 
    newCaller = (caller |> nc "plotVariation")
    cubeCollection = SweepAccess.collectionCubeToCubeCollection sweep
    demandCubes = Collection.map (CubeMap.map (flip CubeMap.lookupLinUnsafe (CubeGrid.LinIdx 0))) cubeCollection
    searchCubes = CubeMap.lookupLinUnsafe sweep (CubeGrid.LinIdx 0)
    
plotDemandSweepValue ::
  (Ord c,
   Ord a,
   Show label,
   Atom.C c,
   Atom.C a,
   DV.Walker vec,
   DV.Storage vec2 a1,
   DV.Storage vec (CubeMap.Data inst1 dim1 vec1 b),
   DV.Storage vec b,
   DV.Storage vec c,
   DV.LookupUnsafe vec1 b,
   DV.Length vec2,
   ND.Dimensions dim2,
   PlotCube.ToPlotData CubeMap.Cube dim label vec a c) =>
  Caller ->
  String ->
  CubeGrid.Grid inst2 dim2 label1 vec2 a1 ->
  (b -> c) ->
  CubeGrid.ExtractInfo dim2 ->
  CubeMap.Cube inst dim label vec a (CubeMap.Data inst1 dim1 vec1 b) ->
  Frame.T (Graph3D.T a a c)
plotDemandSweepValue caller title searchGrid faccess  extractData sweepCube = 
  PlotD3.allInOne (PlotD3.labledFrame title) (\ _ plotData -> LineSpec.title (show $ PlotD3.getId $ plotData )) $ 
  concatMap f $ SweepAccess.extractSearchData (caller |> nc "plotEvalSweepStackValue") searchGrid  sweepCube extractData
  where 
    f (dimIdx,cube) = PlotCube.toPlotData caller (Just dimIdx) $ CubeMap.map faccess cube

plotStoragePowers :: 
 (Show node,
  Ord node,
  Atom.C b,
  Atom.C a,
  Ord b,
  Ord a,
  DV.Walker demVec,
  DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec b)),
  DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec b),
  DV.Storage srchVec a,
  DV.Storage demVec b,
  DV.LookupUnsafe srchVec b,
  DV.Length srchVec,
  ND.Dimensions srchDim,
  PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a b) =>
  Caller ->
  String ->
  CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a ->
  CubeGrid.ExtractInfo srchDim ->
  [node] ->
  CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a b ->
  --CubeMap.Cube inst dim label vec a (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data inst1 dim1 vec1 b)) ->
  Frame.T (Graph3D.T a a b)
plotStoragePowers caller title searchGrid extractData nodeList sweepCube = 
  PlotD3.allInOne (PlotD3.labledFrame title) (\ _ plotData -> LineSpec.title (show $ PlotD3.getId $ plotData )) $ 
  concatMap (\(k,swpCube) -> concatMap (f k) $ SweepAccess.extractSearchData 
               (caller |> nc "plotEvalSweepStackValue") searchGrid  swpCube extractData) cubeList
  where 
    f k (dimIdx,cube) = PlotCube.toPlotData caller (Just (k,dimIdx)) cube
    cubeList = zip nodeList $ map (\k -> CubeMap.map (Maybe.fromJust . TopoQty.sumOut . Maybe.fromJust . Maybe.fromMaybe (err k) . Map.lookup k . 
                                                      FlowOpt.unStorageMap . FlowTopoOpt.getStorageMap) 
                                           sweepCube) nodeList
    err k = merror caller modul "plotStoragePowers" $ "key not found " ++ show k 
    

plotSweepFlowValues ::  
  (Show key, DV.Walker demVec,Ord b, Ord a, Atom.C b, Atom.C a,
   DV.Storage srchVec a,Show node,
   DV.Storage demVec b,
   DV.LookupUnsafe srchVec b,
   DV.Length srchVec,
   ND.Dimensions srchDim,
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a b,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec b), 
   DV.Storage demVec (TopoQty.Section node (CubeMap.Data (Sweep.Search inst) srchDim srchVec b))) =>
  Caller ->
  String ->
  CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a ->
  (key -> TopoQty.Section node (CubeMap.Data (Sweep.Search inst) srchDim srchVec b) -> Maybe (CubeMap.Data (Sweep.Search inst) srchDim srchVec b)) ->
  CubeGrid.ExtractInfo srchDim ->
  [key] ->
  CubeSweep.Flow node inst demDim srchDim demVec srchVec a b ->
  Frame.T (Graph3D.T a a b)
plotSweepFlowValues caller title searchGrid faccess  extractData keyList sweepCube = 
  PlotD3.allInOne (PlotD3.labledFrame title) (\ _ plotData -> LineSpec.title (show $ PlotD3.getId $ plotData )) $ 
  concatMap (\(k,swpCube) -> concatMap (f k) $ SweepAccess.extractSearchData 
               (caller |> nc "plotSweepValues") searchGrid  swpCube extractData) cubeList
  where 
    f k (dimIdx,cube) = PlotCube.toPlotData caller (Just (k,dimIdx)) cube
    cubeList = zip keyList $ map (\k -> CubeMap.map (Maybe.fromMaybe (err k) . faccess k) sweepCube) keyList
    err k = merror caller modul "plotSweepValues" $ "key not found " ++ show k 
  


plotStates ::
  (DV.Walker vec,Ord a, Show node, Atom.C a,
   DV.Walker srchVec,
   DV.Storage vec (Maybe Idx.AbsoluteState),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure b),
   DV.Storage srchVec a,
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure b),
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityMeasure b)),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a (ActFlowCheck.EdgeFlowStatus,
                                                                                               FlowOpt.OptimalityMeasure b)),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                    FlowOpt.OptimalityMeasure b))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityMeasure b),
   DV.LookupUnsafe vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                          FlowOpt.OptimalityMeasure b)),
   DV.Length srchVec,
   DV.FromList srchVec,ND.Dimensions srchDim,
   Ord b,Arith.Constant b,
  DV.Storage vec (Interp.Val (Interp.Val a)),
  PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val (Interp.Val a)),
  DV.Storage vec (Interp.Val b),
  PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val b)) =>
  Caller ->
  String ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  CubeSweep.OptimalityMeasure node inst dim srchDim vec srchVec a b ->
  Frame.T (Graph3D.T a a (Interp.Val b))
plotStates caller title searchGrid sweepCube = 
  plotDemandSweepValue (caller |> nc "plotStates") title searchGrid 
  (Maybe.maybe (Interp.Invalid ["plotStates"]) Interp.Inter . fmap (Arith.fromInteger . fromIntegral . Idx.unAbsoluteState) . 
   ActFlowCheck.getState . fst)  
  (CubeGrid.All) sweepCube
  
  
plotOptimalOptimalityValuePerState ::
  (Arith.Sum b, Atom.C a,Ord b, Ord a, Show node, Arith.Constant b,
   DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec (Interp.Val b),
   DV.Storage vec (Maybe (CubeGrid.LinIdx,
                          (ActFlowCheck.EdgeFlowStatus,
                           FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Storage vec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues (Interp.Val b))),

   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues b))),
   DV.Storage vec (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus,
                                     FlowOpt.OptimalityValues b)),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val b),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a b) =>
  Caller ->
  String ->
  ((CubeGrid.LinIdx, 
    (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val b))) -> Interp.Val b) ->
  CubeSweep.OptimalChoicePerState node inst dim vec a (Interp.Val b) ->
  Frame.T (Graph3D.T a a (Interp.Val b))
plotOptimalOptimalityValuePerState caller title faccess optPerState = 
  PlotD3.allInOne (PlotD3.labledFrame title) 
      (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) $ 
  concatMap f stateCubes
  where  
    f (_,cube) = PlotCube.toPlotData caller (Just "OpimalObjectivePerState") $ 
                  CubeMap.map (Maybe.maybe (Interp.Invalid ["plotOptimalObjectivePerState"]) faccess) cube
    stateCubes = ValueState.toList $ SweepAccess.stateCubeToStateCubes optPerState
    legend = Map.fromList $ zip [0..] $ map fst stateCubes
 
