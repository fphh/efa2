{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Process where

import qualified EFA.Action.EenergyFlowAnalysis as EFA
import qualified EFA.Action.Simulation as Simulation
import qualified EFA.Action.Optimisation.Loop as Loop
import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Data.OD.Curve as Curve
import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Data.OD.Signal.Chop as DataChop

import EFA.Utility(Caller,
                  --merror,
                  (|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
import qualified EFA.Action.Flow.Balance as FlowBal
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Strict
--import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
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

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.Process"

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
   accessLabledTopology :: Topo.LabeledTopology node
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
      
data OptiSet node inst demDim srchDim demVec srchVec sigVec a = 
  OptiSet 
  {accessDemandVariation :: [(DemandAndControl.Var node,Type.Dynamic,demVec a)],
   accessSearchVariation :: [(DemandAndControl.Var node,Type.Dynamic,srchVec a)],  
   accessDemandVars :: [DemandAndControl.DemandVar node],
   accessControlVars :: [DemandAndControl.ControlVar node],  
   accessDemandGrid ::  CubeGrid.Grid (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a,
   accessSearchGrid ::  CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a,
   accessVariation :: CubeSweep.Variation node inst demDim srchDim demVec srchVec a (Interp.Val a),
   accessSupportSignal :: OptSignal.SupportSignal node inst demDim sigVec a a
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
  accessOptimalStateSignals :: OptSignal.OptimalityPerStateSignal node inst sigVec a (Interp.Val a), 
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
  

buildSystem :: (Node.C node) => ActUt.LabeledEdgeList node -> System node
buildSystem  edgeList = System edgeList topoPlain labeledTopology
  where labeledTopology = ActUt.topologyFromLabeledEdges edgeList
        topoPlain = Topo.plainFromLabeled labeledTopology
  
buildTestSet :: 
  OptSignal.DemandCycle node inst demDim sigVec a a -> 
  Balance.Balance node a -> 
  TestSet node inst demDim sigVec a
buildTestSet demandCycle initialBalance = TestSet demandCycle initialBalance


buildSystemData :: 
  (Ord a,
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

  [(DemandAndControl.Var node,Type.Dynamic,demVec a)] ->
  [(DemandAndControl.Var node,Type.Dynamic,srchVec a)] ->
  OptSignal.DemandCycle node inst demDim sigVec a a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a
buildOptiSet demandVariation searchVariation demandCycle = 
  OptiSet demandVariation searchVariation demandVars controlVars 
   demandGrid searchGrid 
   sweepVariation supportSignal
  where
    demandGrid = CubeGrid.create (nc "Main") demandVariation
    searchGrid = CubeGrid.create (nc "Main") searchVariation
    sweepVariation = CubeSweep.generateVariation (nc "Main") demandGrid searchGrid
    supportSignal = OptSignal.getSupportPoints (nc "Main") demandGrid demandCycle 
    demandVars  = map (DemandAndControl.toDemandVar . TupleHT.fst3) demandVariation
    controlVars = map (DemandAndControl.toControlVar . TupleHT.fst3) searchVariation


-- | Only has to be calculated once, unless efficiency curves change
makeSweep ::
  (Ord a,
   Arith.Constant a,Arith.NaNTestable a,
   Node.C node,
   DV.Zipper srchVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node ((CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
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
   System node ->
  SystemData inst node etaVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  SweepResults node inst demDim srchDim demVec srchVec a
makeSweep system systemData optiSet = SweepResults energyFlowResult energyFlow flowStatus endNodePowers 
  where 
    topology = accessTopology system
    etaFunctions = accessFunctionMap systemData
    energyFlowResult = CubeSweep.solve topology etaFunctions (accessVariation optiSet)
    energyFlow = CubeMap.map (TopoQty.mapSection $ ActUt.checkDetermined "makeSweep") energyFlowResult
    flowStatus = CubeSweep.getFlowStatus (nc "main") energyFlow
    endNodePowers = CubeSweep.getEndNodeFlows energyFlow

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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
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
  FlowOpt.LifeCycleMap node (Interp.Val a) ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  SweepEvaluation node inst demDim srchDim demVec srchVec a
evaluateSweep caller lifeCycleMap sweepResults = 
  SweepEvaluation $ CubeSweep.calculateOptimalityMeasure caller lifeCycleMap endNodePowers status
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
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (Maybe (Interp.Val a))))),
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Length demVec,
   DV.Length sigVec,
   DV.Length srchVec) =>
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  SweepEvaluation node inst demDim srchDim demVec srchVec a ->
  [node] ->
  FlowBal.Forcing node (Interp.Val a) ->
  [DemandAndControl.ControlVar node] ->
  OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a

optimisationPerState testSet optiSet sweepResults sweepEvaluationResults storageList balanceForcingMap controlVars = 
  OptimisationPerState
  objectiveFunctionValues    
  optimisationResultPerState
  optimalFlowCube
  optimalStateSignals
  optimalControlSignalsPerState 
  optimalStoragePowersPerState
  where 
    demandCycle = accessDemandCycle testSet
    supportSignal = accessSupportSignal optiSet
    flowResult = accessSweepFlowResult sweepResults
    endNodePowers = accessSweepEndNodePowers sweepResults
    optimalityMeasure = accessSweepOptimality sweepEvaluationResults
    objectiveFunctionValues = CubeSweep.objectiveFunctionValues (nc "main") balanceForcingMap endNodePowers optimalityMeasure
    optimisationResultPerState = CubeSweep.findMaximumEtaPerState  (nc "main") objectiveFunctionValues                          
    optimalFlowCube = CubeSweep.unresultOptimalFlowPerStateCube (nc "main") $ 
                        CubeSweep.getOptimalFlowPerStateCube (nc "main") optimisationResultPerState flowResult
    optimalStateSignals = OptSignal.optimalStateSignals (nc "main") optimisationResultPerState supportSignal demandCycle
    optimalStoragePowersPerState = OptSignal.interpolateStoragePowersPerState (nc "main") Interp.Linear 
                              optimalFlowCube supportSignal demandCycle storageList                                   
    optimalControlSignalsPerState = OptSignal.interpolateControlSignalsPerState (nc "main") Interp.Linear 
                                       optimalFlowCube supportSignal demandCycle controlVars
optimalOperation ::
  (Ord a,
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
 DV.Storage vec (Maybe (Interp.Val a)),
 DV.Storage vec (ValueState.Map (Interp.Val a)),
 DV.Storage vec (SignalFlow.TimeStep a, Maybe (Interp.Val a)),
 DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
 DV.Singleton vec,
 DV.Storage vec (SignalFlow.TimeStep a),
 DV.Storage vec [SignalFlow.TimeStep a],
 DV.FromList vec) =>
 OptimisationPerState node inst demDim srchDim demVec srchVec vec a ->
 OptimalOperation node inst vec a
optimalOperation optimisationPerStateResults =
  OptimalOperation optimalStateSignal optimalControlSignals
     optimalStorageSignals balance
  where
    optimalStateSignals = accessOptimalStateSignals optimisationPerStateResults
    optimalStoragePowersPerState = accessOptimalStoragePowersPerState optimisationPerStateResults
    optimalControlSignalsPerState = accessOptimalControlSignalsPerState optimisationPerStateResults
    
    optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOff optimalStateSignals
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
   SV.Convert sigVec [],
   DV.Storage sigVec (ND.Data dim a),
   DV.Storage sigVec (SignalFlow.TimeStep a),
   Arith.ZeroTestable a) =>
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
    optimalControlSignals = accessOptimalControlSignals optOperation
    given = OptSignal.makeGivenRecord (caller |> nc "simulateAndAnalyse") 
            (OptSignal.convertToDemandCycleMap demandCycle demandVars) optimalControlSignals
    sim = Simulation.simulation caller topology etaFunctions given
    sequenceFlowRecord = DataChop.chopHRecord (caller |> nc "simulateAndAnalyse") (Simulation.accessPowerRecord sim)
    sequenceFlowRecordOld = DataChop.convertToOld sequenceFlowRecord
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
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
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (Maybe (Interp.Val a))))),
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
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
   DV.Len (sigVec (Interp.Val a)),
   DV.Storage sigVec (SignalFlow.TimeStep a,Maybe (Interp.Val a)),
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec [SignalFlow.TimeStep a],   
   DV.FromList sigVec) =>
  Caller ->
  System node ->
  SystemData inst node etaVec a ->
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  EFA.EFAParams node (Interp.Val a) ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  FlowOpt.LifeCycleMap node (Interp.Val a) ->
  [node] ->
  Loop.EtaLoopParams (Interp.Val a) ->
  Loop.BalanceLoopParams node (Interp.Val a) ->
  [Loop.EtaLoopItem node (Interp.Val a) (Res node inst demDim srchDim demVec srchVec sigVec a)]

loop caller system systemData testSet optiSet efaParams sweepResults initialLifeCycleMap storageList etaParams balParams = 
  Loop.etaLoop caller storageList etaParams balParams sysF getBalance initialLifeCycleMap updateLifeCycleMap
  where
    newCaller = caller |> nc "loop"
    topo = accessTopology system
    method = Loop.accLifeCycleMethod etaParams
    globalEtas = Loop.accGlobalEtas etaParams
    
    getBalance = Balance.unMaybeBalance newCaller . accessBalance . accOptOperation
    updateLifeCycleMap = StateFlowOpt.updateOneStorageLifeCycleEfficiencies 
                                                  newCaller topo method globalEtas . 
                                                  EFA.accessStateFlowGraph . accessAnalysis . accSimEfa
                                                  
    sysF = systemFunction newCaller system systemData testSet optiSet efaParams storageList sweepResults
 
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
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
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (Maybe (Interp.Val a))))),
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
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
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
   DV.Storage sigVec (SignalFlow.TimeStep a, Maybe (Interp.Val a)),
   DV.FromList sigVec) =>
  Caller ->
  System node ->
  SystemData inst node etaVec a ->
  TestSet node inst demDim sigVec a ->
  OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  EFA.EFAParams node (Interp.Val a) ->
  [node] ->
  SweepResults node inst demDim srchDim demVec srchVec a ->
  FlowOpt.LifeCycleMap node  (Interp.Val a) ->
  FlowBal.Forcing node (Interp.Val a) ->
  Res node inst demDim srchDim demVec srchVec sigVec a

systemFunction caller system systemData testSet optiSet efaParams storageList sweepResults lifeCycleMap balanceForcingMap = 
      Res
      sweepEvaluationResults
      optimisationPerStateResults
      optOperation 
      simEfa
      where
        demandCycle = accessDemandCycle testSet
        demandVars = accessDemandVars optiSet
        controlVars = accessControlVars optiSet
        
        sweepEvaluationResults = evaluateSweep caller lifeCycleMap 
                                 sweepResults
        
        optimisationPerStateResults = optimisationPerState testSet optiSet sweepResults sweepEvaluationResults 
                                  storageList balanceForcingMap controlVars
        optOperation = optimalOperation optimisationPerStateResults
        
        simEfa = simulateAndAnalyse caller system efaParams systemData demandVars optOperation demandCycle
 



data Res node inst demDim srchDim demVec srchVec sigVec a = Res
  {accSweepEval :: SweepEvaluation node inst demDim srchDim demVec srchVec a,
   accOptPerState :: OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a,
   accOptOperation :: OptimalOperation node inst sigVec a, 
   accSimEfa :: SimulationAndAnalysis node inst sigVec a 
     }