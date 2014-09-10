{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Process where

import qualified EFA.Action.EnergyFlowAnalysis as EFA
import qualified EFA.Action.Simulation as Simulation
import qualified EFA.Action.Optimisation.Loop as Loop
import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Data.OD.Curve as Curve
import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Data.OD.Signal.Chop as DataChop

--import qualified EFA.Utility.Trace as UtTrace

import qualified EFA.Signal.Data as Data

import EFA.Utility(Caller,
                  --merror,
                  (|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Action.Flow.Topology.State as TopoState

--import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.Flow.Optimality as FlowOpt
--import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
import qualified EFA.Action.Flow.Balance as FlowBal
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Strict
--import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology as FlowTopoPlain
--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
--import qualified EFA.Flow.Topology as FlowTopo
--import qualified EFA.Data.Interpolation as Interp
--import qualified EFA.Application.Utility as AppUt
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Data.OD.Signal.Flow as SignalFlow
--import EFA.Application.Utility (quantityTopology)
--import qualified EFA.Application.Optimisation.Sweep as Sweep
--import EFA.Application.Optimisation.Params (Name)
--import qualified EFA.Application.Optimisation.Params as Params

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

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology.Quantity as TopoQty

import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))

--import qualified  UniqueLogic.ST.TF.System as ULSystem
import qualified EFA.Action.Utility as ActUt
import qualified Data.Map as Map
--import qualified Data.Foldable as Fold

--import Data.Monoid((<>))

--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Value.Type as Type
-- import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
--import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative
-- import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology as FlowTopo
import qualified Data.Tuple.HT as TupleHT
--import qualified Data.List as List
--import qualified Data.Set as Set

modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Process"

nc :: FunctionName -> Caller
nc = genCaller modul
                               
data InputSet = 
  InputSet
  {accessEtaCurvesFile :: String,
   accessDemandCycleFile :: String}
  
data System node = 
  System
  {accessLabledEdgeList :: ActUt.LabeledEdgeList node,
   accessTopology :: Topo.Topology node, 
   accessLabledTopology :: Topo.LabeledTopology node,
   accessStateAnalysis :: [(Idx.AbsoluteState,Topo.FlowTopology node)],
   accessStates :: [Idx.AbsoluteState]
  } deriving Show

data SystemData inst node etaVec a = 
  SystemData
  {accessRawEfficiencyCurves :: Curve.Map String inst String etaVec a a,
   accessEtaAssignMap :: EtaFunctions.EtaAssignMap node a,
   accessFunctionMap :: EtaFunctions.FunctionMap node (Interp.Val a),
   accessFunctionPlotAxis :: Strict.Axis inst String etaVec a 
  } 
   
data TestSet node inst demDim sigVec a = 
  TestSet
  {accessDemandCycle ::  OptSignal.DemandCycle node inst demDim sigVec a a,
   accessInitialSoc :: Balance.Balance node a} 
      
-- TODO: Insert Data-Type to host a multitude of search-Cubes 
-- for now cubes are concatinated at a certain point in the calculation, making search-cube display impossible  
data OptiSet node inst demDim srchDim demVec srchVec sigVec a = 
  OptiSet 
  {accessDemandVariation :: [(DemandAndControl.Var node,Type.Dynamic,demVec a)],
   accessSearchVariations :: [[(DemandAndControl.Var node,Type.Dynamic,srchVec a)]],  
   accessDemandVars :: [DemandAndControl.DemandVar node],
   accessControlVars :: [[DemandAndControl.ControlVar node]],  
   accessDemandGrid ::  CubeGrid.Grid (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a,
   accessSearchGrids :: [CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a],
   accessVariations :: [CubeSweep.Variation node inst demDim srchDim demVec srchVec a (Interp.Val a)],
   accessSupportSignal :: OptSignal.SupportSignal node inst demDim sigVec a a,
   accessAllowedStates ::  [Idx.AbsoluteState]
     } 

data SweepResults node inst demDim srchDim demVec srchVec a = 
  SweepResults 
  {accessSweepFlowResult :: CubeSweep.FlowResult node inst demDim srchDim demVec srchVec a (Interp.Val a),
   accessSweepFlow :: CubeSweep.Flow node inst demDim srchDim demVec srchVec a (Interp.Val a),
   accessSweepFlowStatus :: CubeSweep.FlowStatus node inst demDim srchDim demVec srchVec a,
   accessSweepEndNodePowers:: CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a (Interp.Val a)
  }

data SweepEvaluation node inst demDim srchDim demVec srchVec a = 
  SweepEvaluation {accessSweepOptimality :: 
                             CubeSweep.OptimalityMeasure node inst demDim srchDim demVec srchVec a (Interp.Val a)}


data OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a = OptimisationPerState {
  accessObjectiveFunctionValues :: CubeSweep.ObjectiveFunctionValues node inst demDim srchDim demVec srchVec a (Interp.Val a),
  accessOptimalChoicePerState :: CubeSweep.OptimalChoicePerState node inst demDim demVec a (Interp.Val a),
  accessOptimalFlowPerState :: CubeSweep.OptimalFlowPerState node inst demDim demVec a (Interp.Val a), 
  accessOptimalSignalsPerState:: OptSignal.OptimalityPerStateSignal node inst sigVec a (Interp.Val a), 
  accessOptimalControlSignalsPerState :: OptSignal.OptimalControlSignalsPerState node inst sigVec a (Interp.Val a),
  accessOptimalStoragePowersPerState :: OptSignal.OptimalStoragePowersPerState node inst sigVec a (Interp.Val a)}


data OptimalOperation node inst vec a = OptimalOperation {
  accessOptimalStateChoice :: OptSignal.OptimalStateChoice node inst vec a (Interp.Val a),
  accessOptimalControlSignals :: OptSignal.OptimalControlSignals node inst vec a (Interp.Val a),
  accessOptimalStoragePowers :: OptSignal.OptimalStoragePowers node inst vec a (Interp.Val a),
  accessBalance :: Balance.Balance node (Maybe (Interp.Val a))}


data SimulationAndAnalysis node inst sigVec a = 
  SimulationAndAnalysis
  {accessSimulation :: Simulation.Simulation node inst sigVec a,
   accessAnalysis :: EFA.EnergyFlowAnalysis node inst sigVec a}   

data OutputSet = OutputSet {
  accessOutPutFolder :: String}
                 
data ContolSet = ContolSet {                 
  accessMakePlot :: Bool}
  

buildSystem :: 
  (Node.C node,Show node) => 
  ActUt.LabeledEdgeList node -> 
  TopoState.EdgeConditions node -> 
  System node
buildSystem edgeList edgeCond = System edgeList topoPlain labeledTopology stateAnalysis stateList
  where labeledTopology = ActUt.topologyFromLabeledEdges edgeList
        topoPlain = Topo.plainFromLabeled labeledTopology
        stateAnalysis = TopoState.filterFlowStates edgeCond $ TopoState.stateAnalysisAbsolute topoPlain
        stateList = map fst stateAnalysis
  
buildTestSet :: 
  OptSignal.DemandCycle node inst demDim sigVec a a -> 
  Balance.Balance node a -> 
  TestSet node inst demDim sigVec a
buildTestSet demandCycle initialBalance = TestSet demandCycle initialBalance


buildSystemData :: 
  (Ord a,Ord node, 
   DV.Zipper etaVec,
   Show a,
   Arith.Constant a,
   DV.Walker etaVec,
   DV.Storage etaVec a,
   DV.Singleton etaVec,
   DV.Reverse etaVec,
   DV.LookupUnsafe etaVec a,
   DV.Length etaVec,
   DV.FromList etaVec,
   DV.Find etaVec) =>
  Curve.Map String inst String etaVec a a ->
  EtaFunctions.EtaAssignMap node a ->
  Strict.Axis inst String etaVec a ->
  SystemData inst node etaVec a
buildSystemData rawCurves etaAssignMap etaFunctionShowAxis = SystemData rawCurves etaAssignMap etaFunctions etaFunctionShowAxis
  where
  etaFunctions = EtaFunctions.makeEtaFunctions (nc "buildSystemData") etaAssignMap rawCurves
    

buildOptiSet ::
  (DV.Zipper demVec,
   DV.Storage demVec Bool,
   DV.Storage demVec a,
   DV.Singleton demVec,
   ND.Dimensions demDim, 
   Ord a,
   DV.Zipper srchVec,
   DV.Storage srchVec Bool,
   DV.Storage srchVec a,
   DV.Singleton srchVec,
   ND.Dimensions srchDim, 
   Eq (srchVec a),
   Ord node,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage demVec (Collection.Collection (DemandAndControl.Var node) 
                      (CubeMap.Cube (Sweep.Search inst) srchDim 
                       (DemandAndControl.Var node) srchVec a (Interp.Val a))), 
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (srchVec [a]),
   DV.Storage srchVec [a],
   DV.Storage srchVec (ND.Data srchDim a),
   DV.Storage demVec (demVec [a]),
   DV.Storage demVec [a],
   DV.Storage demVec (ND.Data demDim a),
   DV.FromList demVec,
   DV.FromList srchVec, 
   DV.Walker sigVec,
   DV.Storage
   sigVec (ND.Data demDim (Strict.SupportingPoints (Strict.Idx, a))),
   DV.Storage sigVec (ND.Data demDim a),
   DV.LookupUnsafe demVec a,
   DV.Length demVec,
   DV.Find demVec) =>
  Caller ->
  [(DemandAndControl.Var node,Type.Dynamic,demVec a)] ->
  [[(DemandAndControl.Var node,Type.Dynamic,srchVec a)]] ->
  OptSignal.DemandCycle node inst demDim sigVec a a ->
  [Idx.AbsoluteState]->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a
buildOptiSet caller demandVariation searchVariations demandCycle allowedStates = 
  OptiSet demandVariation searchVariations demandVars controlVars 
   demandGrid searchGrids 
   sweepVariations supportSignal
   allowedStates
  where
    newCaller = caller |>  nc "buildOptiSet"
    demandGrid = CubeGrid.create newCaller demandVariation
    searchGrids = map (CubeGrid.create newCaller) searchVariations
    sweepVariations = map (CubeSweep.generateVariation newCaller demandGrid) searchGrids
    supportSignal = OptSignal.getSupportPoints newCaller demandGrid demandCycle 
    demandVars  = map (DemandAndControl.toDemandVar . TupleHT.fst3) demandVariation
    controlVars = map (map (DemandAndControl.toControlVar . TupleHT.fst3)) searchVariations


-- | Only has to be calculated once, unless efficiency curves change
makeSweep ::
  (Ord a,Eq (demVec a), DV.Zipper demVec,
   Arith.Constant a,Arith.NaNTestable a,
   Node.C node,
   DV.Zipper srchVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage demVec (FlowOpt.EndNodeEnergies node ((CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage demVec ((CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (TopoQty.Section node (Maybe 
                                            (TopoQty.Flow (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))))),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.Storage srchVec ActFlowCheck.Validity,
   DV.Storage demVec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage srchVec a,
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec Bool,
   DV.Storage demVec (Collection.Collection (DemandAndControl.Var node) 
                      (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a (Interp.Val a))),
   DV.Singleton srchVec,
   DV.Length srchVec,
   DV.Storage demVec (TopoQty.Section node (Maybe (TopoQty.Flow (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))))), 
   DV.Storage demVec (TopoQty.Section node 
                      (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))) =>
  Caller -> 
  System node ->
  SystemData inst node etaVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  SweepResults node inst demDim srchDim demVec srchVec a
makeSweep caller system systemData optiSet = SweepResults energyFlowResult energyFlow flowStatus endNodePowers 
  where 
    newCaller = caller |> nc "makeSweep"
    topology = accessTopology system
    etaFunctions = accessFunctionMap systemData
    energyFlowCubes = map (CubeSweep.solve topology etaFunctions) (accessVariations optiSet)
    energyFlowResult = CubeSweep.joinFlowCubes newCaller energyFlowCubes
    energyFlow = CubeMap.map (TopoQty.mapSection $ ActUt.checkDetermined "makeSweep") energyFlowResult
    flowStatus = CubeSweep.getFlowStatus newCaller energyFlow
    endNodePowers = CubeSweep.getEndNodeFlows newCaller energyFlow

evaluateSweep ::
  (Eq (demVec a),
   Ord node,
   Ord a,
   Show node,
   Show (srchVec a),
   Arith.Constant a,
   DV.Zipper demVec,
   DV.Zipper srchVec,
   DV.Walker srchVec,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a,
                       FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce a),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec a,
   DV.Storage srchVec (a, a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a),
                       FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,(a,a)),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Singleton srchVec,
   DV.Length srchVec) =>
  Caller ->
  (Caller -> 
   evalParam ->
   FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)) ->
   CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus ->
   CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                     FlowOpt.OptimalityMeasure (Interp.Val a))) ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  evalParam ->
  SweepEvaluation node inst demDim srchDim demVec srchVec a
evaluateSweep caller evalFunction sweepResults evalParam = 
  SweepEvaluation $  CubeMap.zipWith (caller |> nc "evaluateSweep") 
  (evalFunction (caller |> nc "evaluateSweep") evalParam) endNodePowers status
  where endNodePowers = accessSweepEndNodePowers sweepResults
        status = accessSweepFlowStatus sweepResults
 
optimisationPerState ::
  (Eq (demVec a),
   Ord a,
   Show (demVec (ValueState.Map (Interp.Val a))),
   Show (demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (demVec (ValueState.Map (Maybe (Interp.Val a)))),
   Show a,
   Show (sigVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (demVec (ValueState.Map (CubeGrid.LinIdx,
                                  (ActFlowCheck.EdgeFlowStatus,
                                   FlowOpt.OptimalityValues (Interp.Val a))))),
   Show node,
   Arith.Constant a,
   Node.C node,
   DV.Zipper sigVec,
   DV.Zipper demVec,
   DV.Zipper srchVec,
   DV.Walker demVec,
   DV.Walker srchVec,
   DV.Storage demVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ND.Data demDim (Strict.SupportingPoints (Strict.Idx,
                                                               a))),
   DV.Storage sigVec (ND.Data demDim a),
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec a,
   DV.Storage sigVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage sigVec a,
   DV.Storage demVec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Interp.Val a))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Result.Result (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                       (ActFlowCheck.EdgeFlowStatus,
                                        FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                        (ActFlowCheck.EdgeFlowStatus,
                         FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                        (ActFlowCheck.EdgeFlowStatus,
                         (Interp.Val a,
                          Interp.Val a))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                          (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                                           (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                                            FlowOpt.Loss2Optimise (Interp.Val a)))))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a),
                        FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                        FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce (Interp.Val a)),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                          FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                          FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                        FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (Interp.Val a,
                        Interp.Val a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                        (Interp.Val a,
                         Interp.Val a)),
   DV.Slice demVec,
   DV.Slice sigVec,
   DV.Singleton srchVec,
   DV.LookupUnsafe srchVec (Interp.Val a),   
   DV.LookupMaybe demVec (ValueState.Map (Interp.Val a)),
   DV.LookupMaybe demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (CubeGrid.LinIdx,
                                           (ActFlowCheck.EdgeFlowStatus,
                                            FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe sigVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                         (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                          FlowOpt.Loss2Optimise (Interp.Val a))))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (FlowOpt.StorageFlow (Interp.Val a))))),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Length demVec,
   DV.Length sigVec,
   DV.Length srchVec) =>
  Caller ->
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  SweepEvaluation node inst demDim srchDim demVec srchVec a ->
  [node] ->
  FlowBal.Forcing node (Interp.Val a) ->
  [DemandAndControl.ControlVar node] ->
  OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a

optimisationPerState caller testSet optiSet sweepResults sweepEvaluationResults storageList balanceForcingMap controlVars = 
  OptimisationPerState
  objectiveFunctionValues    
  optimisationResultPerState
  optimalFlowCube
  optimalStateSignals
  optimalControlSignalsPerState 
  optimalStoragePowersPerState
  where 
    newCaller = caller |> caller |> nc "optimisationPerState"
    demandCycle = accessDemandCycle testSet
    supportSignal = accessSupportSignal optiSet
    flowResult = accessSweepFlowResult sweepResults
    endNodePowers = accessSweepEndNodePowers sweepResults
    optimalityMeasure = accessSweepOptimality sweepEvaluationResults
    states = accessAllowedStates optiSet
    objectiveFunctionValues = CubeSweep.objectiveFunctionValues newCaller balanceForcingMap endNodePowers optimalityMeasure
    optimisationResultPerState = CubeSweep.findMaximumEtaPerState  newCaller states objectiveFunctionValues                          
    optimalFlowCube = CubeSweep.unresultOptimalFlowPerStateCube newCaller $ 
                        CubeSweep.getOptimalFlowPerStateCube newCaller optimisationResultPerState flowResult
    optimalStateSignals = OptSignal.optimalStateSignals newCaller optimisationResultPerState supportSignal demandCycle
    optimalStoragePowersPerState = OptSignal.interpolateStoragePowersPerState newCaller Interp.Linear 
                              optimalFlowCube supportSignal demandCycle storageList                                   
    optimalControlSignalsPerState = OptSignal.interpolateControlSignalsPerState newCaller Interp.Linear 
                                       optimalFlowCube supportSignal demandCycle controlVars
optimalOperation ::
  (Ord a,Show a,
 Arith.Constant a,
 DV.Zipper vec,
 DV.Walker vec,
 DV.Storage vec (a,
 Maybe (Interp.Val a)),
 DV.Storage vec [Maybe (Interp.Val a)],
 DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
 DV.Storage vec a,
 DV.Storage vec [Interp.Val a],
 DV.Storage vec [a],
 DV.Storage vec ([Maybe Idx.AbsoluteState],
                 Maybe (Interp.Val a)),
 DV.Storage vec (Interp.Val a),
 Show (vec (Interp.Val a)),
 Show (vec (SignalFlow.TimeStep a)),
 DV.Storage vec (Maybe (Interp.Val a)),
 DV.Storage vec (ValueState.Map (Interp.Val a)),
 DV.Storage vec (SignalFlow.TimeStep a, Maybe (Interp.Val a)),
 DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
 DV.Singleton vec,
 Show (vec (ValueState.Map (Interp.Val a))),
 DV.Storage vec (SignalFlow.TimeStep a),
 DV.Storage vec [SignalFlow.TimeStep a],
 DV.Storage vec Int,
 DV.LookupUnsafe vec (SignalFlow.TimeStep a),
 DV.Find vec,
 DV.FromList vec) =>
 Caller -> 
 OptSignal.StateForcing ->
 OptimisationPerState node inst demDim srchDim demVec srchVec vec a ->
 OptimalOperation node inst vec a
optimalOperation caller stateForcing optimisationPerStateResults =
  OptimalOperation optimalStateSignal optimalControlSignals
     optimalStorageSignals balance
  where
    newCaller = caller |> nc "optimalOperation"
    optimalStateSignals = accessOptimalSignalsPerState optimisationPerStateResults
    optimalStoragePowersPerState = accessOptimalStoragePowersPerState optimisationPerStateResults
    optimalControlSignalsPerState = accessOptimalControlSignalsPerState optimisationPerStateResults
    
    optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta newCaller stateForcing optimalStateSignals
    optimalControlSignals = OptSignal.generateOptimalControl optimalStateSignal optimalControlSignalsPerState                       
    optimalStorageSignals = OptSignal.generateOptimalStorageSignals optimalStateSignal optimalStoragePowersPerState
    balance = OptSignal.getBalance optimalStorageSignals 
  
  
 
-- | simulate and provide EFA with new LifeCycle-Efficiencies
simulateAndAnalyse :: 
  (Arith.ZeroTestable
   (SignalFlow.Data inst sigVec (Interp.Val a)),
   Eq (sigVec (Interp.Val a)),
   SV.Storage sigVec (Interp.Val a),
   Arith.ZeroTestable (Interp.Val a),
   Arith.Sum (SignalFlow.Data inst sigVec a),
   Arith.Product (SignalFlow.Data inst sigVec (Interp.Val a)),
   Arith.Constant a,
   Node.C node,
   DV.Walker sigVec,
   DV.Storage sigVec a,
   DV.Storage sigVec (Interp.Val a),
   DV.Length sigVec,
   DV.FromList sigVec, 
   Ord a,
   DV.Slice sigVec,
   DV.Len (sigVec (Interp.Val a)), 
   Eq (sigVec a),
   Show (sigVec a),
   Show a,
   Show node,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Singleton sigVec,
   SV.Convert [] sigVec,
   Show (sigVec (SignalFlow.TimeStep a)),
   Show (sigVec (Interp.Val a)),
   SV.Convert sigVec [],
   DV.Storage sigVec (ND.Data dim a),DV.Singleton sigVec,
   DV.Storage sigVec (SignalFlow.TimeStep a),
   Arith.ZeroTestable a,
   DV.Zipper sigVec,
   DV.Storage sigVec Int,
   Show (sigVec Int),
   DV.Storage sigVec ([Maybe Idx.AbsoluteState], Maybe (Interp.Val a)),
   DV.Storage sigVec (sigVec a)) =>
  Caller -> 
  System node ->
  EFA.EFAParams node (Interp.Val a) ->
  SystemData inst node etaVec a ->
  [DemandAndControl.DemandVar node] ->
  OptimalOperation node inst sigVec a -> 
  OptSignal.DemandCycle node inst dim sigVec a a ->
  SimulationAndAnalysis  node inst sigVec a
simulateAndAnalyse caller system efaParams systemData demandVars optOperation demandCycle = SimulationAndAnalysis sim efa
  where 
    topology = accessTopology system
    etaFunctions = accessFunctionMap systemData
    optimalStateSignal = accessOptimalStateChoice optOperation
    optimalControlSignals = accessOptimalControlSignals optOperation
    eps = EFA.accessZeroDetectionEps efaParams
    given = --UtTrace.simTrace "Given" $ 
            OptSignal.makeGivenRecord (caller |> nc "simulateAndAnalyse") optimalStateSignal
            (OptSignal.convertToDemandCycleMap demandCycle demandVars) optimalControlSignals
    sim = Simulation.simulation caller topology etaFunctions given
    sequenceFlowRecord = -- UtTrace.simTrace "SfRecord" $
                         DataChop.chopHRecord (caller |> nc "simulateAndAnalyse") eps
                         ( Simulation.accessPowerRecord sim)
    sequenceFlowRecordOld = -- UtTrace.simTrace "SfRecordOld" $ 
                            DataChop.convertToOld sequenceFlowRecord
    efa = EFA.energyFlowAnalysisOld topology efaParams sequenceFlowRecordOld
  

loop ::
  (Eq (demVec a),
   Eq (sigVec (Interp.Val a)),
   Eq (sigVec a),
   Ord a,
   Show (srchVec a),
   Show (demVec (ValueState.Map (Interp.Val a))),
   Show (demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (demVec (ValueState.Map (Maybe (Interp.Val a)))),
   Show (sigVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (demVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (sigVec a),
   Show node,
   Show a,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec (Interp.Val a),
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Singleton sigVec,
   SV.Convert [] sigVec,
   SV.Convert sigVec [],
   Arith.ZeroTestable a,
   Arith.Constant a,
   Node.C node,
   DV.Zipper demVec,
   DV.Zipper srchVec,
   DV.Zipper sigVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Walker sigVec,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a,
                       FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce a),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec a,
   DV.Storage srchVec (a,
                       a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a),
                       FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       (a,
                        a)),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ND.Data demDim (Strict.SupportingPoints (Strict.Idx,
                                                               a))),
   DV.Storage sigVec (ND.Data demDim a),
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec a,
   DV.Storage sigVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage sigVec a,
   DV.Storage demVec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Interp.Val a))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Result.Result (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                       (ActFlowCheck.EdgeFlowStatus,
                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                       (ActFlowCheck.EdgeFlowStatus,
                        (Interp.Val a,
                         Interp.Val a))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                                        (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                                         FlowOpt.Loss2Optimise (Interp.Val a)))))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage srchVec (FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce (Interp.Val a)),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage srchVec (Interp.Val a,
                       Interp.Val a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       (Interp.Val a,
                        Interp.Val a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                         (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                          FlowOpt.Loss2Optimise (Interp.Val a))))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (a,
                      Maybe (Interp.Val a)),
   DV.Storage sigVec [Maybe (Interp.Val a)],
   DV.Storage sigVec [Interp.Val a],
   DV.Storage sigVec [a],
   DV.Storage sigVec ([Maybe Idx.AbsoluteState],
                      Maybe (Interp.Val a)),
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec (Maybe (Interp.Val a)),
   DV.Storage sigVec (Interp.Val a,
                      Interp.Val a),
   DV.Slice demVec,
   DV.Slice sigVec,
   DV.Singleton srchVec,
   DV.Singleton sigVec,
   DV.LookupUnsafe srchVec (Interp.Val a),
   DV.LookupMaybe demVec (ValueState.Map (Interp.Val a)),
   DV.LookupMaybe demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe sigVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Length srchVec,
   DV.Length demVec,
   DV.Length sigVec,
   Show evalParam,
   DV.Len (sigVec (Interp.Val a)),
   DV.Storage sigVec (SignalFlow.TimeStep a,Maybe (Interp.Val a)),
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec [SignalFlow.TimeStep a],   
   DV.FromList sigVec,Show (sigVec Int), 
   Show (sigVec (SignalFlow.TimeStep a)),
   Show (sigVec (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (FlowOpt.StorageFlow (Interp.Val a))))),
   DV.Storage sigVec Int,
   DV.Storage sigVec (sigVec a),
   Show (sigVec (ValueState.Map (Interp.Val a))),
   DV.LookupUnsafe sigVec (SignalFlow.TimeStep a),
   DV.Find sigVec,
   Show (TopoQty.Flow (Result.Result (Data.Data Data.Nil (Interp.Val a))))) =>
  Caller ->
  System node ->
  SystemData inst node etaVec a ->
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  EFA.EFAParams node (Interp.Val a) ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  [node] ->
  Loop.EtaLoopParams node (Interp.Val a) evalParam ->
  Loop.BalanceLoopParams node (Interp.Val a) ->
  (Caller ->
   evalParam ->
   FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)) ->
   CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus ->
   CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                     FlowOpt.OptimalityMeasure (Interp.Val a))) ->
  (Res node inst demDim srchDim demVec srchVec sigVec a  -> evalParam -> (Interp.Val a,evalParam)) ->
  OptSignal.StateForcing ->
  [Loop.EtaLoopItem node (Interp.Val a) evalParam 
   (SweepEvaluation node inst demDim srchDim demVec srchVec a) 
   (Res node inst demDim srchDim demVec srchVec sigVec a)]


loop caller system systemData testSet optiSet efaParams sweepResults storageList etaParams 
  balParams evalFunction updateEvalParam stateForcing = 
  Loop.etaLoop caller storageList etaParams balParams sweepEvalFunction sysFunction getBalance updateEvalParam
  where
    newCaller = caller |> nc "loop"
    getBalance = Balance.unMaybeBalance newCaller . accessBalance . accOptOperation
    sysFunction = systemFunction newCaller system systemData testSet optiSet efaParams storageList sweepResults stateForcing
    sweepEvalFunction = evaluateSweep newCaller evalFunction sweepResults
    
systemFunction ::
  (Eq (sigVec a),
   Eq (sigVec (Interp.Val a)),
   Eq (demVec a),
   Ord a,
   Show (sigVec a),
   Show (demVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (sigVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show a,
   Show (demVec (ValueState.Map (Maybe (Interp.Val a)))),
   Show (demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (demVec (ValueState.Map (Interp.Val a))),
   Show (srchVec a),
   Show node,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec a,
   SV.Storage sigVec Bool,
   SV.Storage sigVec (Interp.Val a),
   SV.Singleton sigVec,
   SV.Convert sigVec [],
   SV.Convert [] sigVec,
   Arith.ZeroTestable a,
   Arith.Constant a,
   Node.C node,
   DV.Zipper sigVec,
   DV.Zipper srchVec,
   DV.Zipper demVec,
   DV.Walker sigVec,
   DV.Walker demVec,
   DV.Walker srchVec,
   DV.Storage sigVec (Interp.Val a,
                      Interp.Val a),
   DV.Storage sigVec (Maybe (Interp.Val a)),
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec ([Maybe Idx.AbsoluteState],
                      Maybe (Interp.Val a)),
   DV.Storage sigVec [a],
   DV.Storage sigVec [Interp.Val a],
   DV.Storage sigVec [Maybe (Interp.Val a)],
   DV.Storage sigVec (a,
                      Maybe (Interp.Val a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                         (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                          FlowOpt.Loss2Optimise (Interp.Val a))))),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       (Interp.Val a,
                        Interp.Val a)),
   DV.Storage srchVec (Interp.Val a,
                       Interp.Val a),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                                        (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                                         FlowOpt.Loss2Optimise (Interp.Val a)))))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                       (ActFlowCheck.EdgeFlowStatus,
                        (Interp.Val a,
                         Interp.Val a))),
   DV.Storage srchVec (CubeGrid.LinIdx,
                       (ActFlowCheck.EdgeFlowStatus,
                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Result.Result (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (TopoQty.Section node (Interp.Val a))),
   DV.Storage demVec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage sigVec a,
   DV.Storage sigVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec a,
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (ND.Data demDim a),
   DV.Storage sigVec (ND.Data demDim (Strict.SupportingPoints (Strict.Idx,
                                                               a))),
   DV.Storage sigVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       (a,
                        a)),
   DV.Storage srchVec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise (Interp.Val a),
                       FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage srchVec (a,
                       a),
   DV.Storage srchVec a,
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (FlowOpt.Eta2Optimise a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce a),
   DV.Storage srchVec (FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a,
                       FlowOpt.Loss2Optimise a),
   DV.Storage demVec (FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Slice sigVec,
   DV.Slice demVec,
   DV.Singleton sigVec,
   DV.Singleton srchVec,
   DV.LookupUnsafe srchVec (Interp.Val a),
   DV.LookupMaybe sigVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe demVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (Interp.Val a)),
   DV.Length sigVec,
   DV.Length demVec,
   DV.Length srchVec,
   DV.Len (sigVec (Interp.Val a)),
   DV.Storage sigVec [SignalFlow.TimeStep a],
   DV.Storage sigVec (SignalFlow.TimeStep a), 
   Show (sigVec (Interp.Val a)),
   Show (sigVec (SignalFlow.TimeStep a)),
   Show (sigVec (ValueState.Map (Interp.Val a))),
   DV.Storage sigVec (SignalFlow.TimeStep a, Maybe (Interp.Val a)),
   DV.FromList sigVec, Show (sigVec Int),
   DV.LookupUnsafe sigVec (SignalFlow.TimeStep a),
   DV.Find sigVec,
   DV.Storage sigVec (sigVec a),
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (FlowOpt.StorageFlow (Interp.Val a))))),
   DV.Storage sigVec Int) =>
  Caller ->
  System node ->
  SystemData inst node etaVec a ->
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  EFA.EFAParams node (Interp.Val a) ->
  [node] ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  OptSignal.StateForcing ->
  SweepEvaluation node inst demDim srchDim demVec srchVec a -> 
  FlowBal.Forcing node (Interp.Val a) ->

  Res node inst demDim srchDim demVec srchVec sigVec a

systemFunction caller system systemData testSet optiSet efaParams 
  storageList sweepResults stateForcing sweepEvaluationResults balanceForcingMap = 
      Res
      optimisationPerStateResults
      optOperation 
      simEfa
      where
        newCaller = caller |> nc "systemFunction"
        demandCycle = accessDemandCycle testSet
        demandVars = accessDemandVars optiSet
        controlVars = accessControlVars optiSet
        

        -- TODO - warning control and sweep vars are not distinguished yet -> controlvar = head sweepVar
        optimisationPerStateResults = optimisationPerState newCaller testSet optiSet sweepResults sweepEvaluationResults 
                                  storageList balanceForcingMap (head controlVars)
        optOperation = optimalOperation newCaller stateForcing optimisationPerStateResults
        
        simEfa = simulateAndAnalyse newCaller system efaParams systemData demandVars optOperation demandCycle

data Res node inst demDim srchDim demVec srchVec sigVec a = Res
  {accOptPerState :: OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a,
   accOptOperation :: OptimalOperation node inst sigVec a, 
   accSimEfa :: SimulationAndAnalysis node inst sigVec a}