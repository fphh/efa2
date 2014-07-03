{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Process where

import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Action.Flow.Balance as Balance

import EFA.Utility(Caller,
                  -- merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.Flow.Optimality as FlowOpt
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

import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Value.Type as Type
-- import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep

--import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative

--import qualified EFA.Flow.Topology as FlowTopo

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.Process"

nc :: FunctionName -> Caller
nc = genCaller modul
                               
-- TODO :: Insert Control and Demand Variables into label !! -> Variation labels will differ !
data SweepAndDemandCycleData inst demDim srchDim label demVec srchVec sigVec a b = 
  SweepAndDemandCycleData 
  {accessDemandVariation :: [(label,Type.Dynamic,demVec b)],
   accessSearchVariation :: [(label,Type.Dynamic,srchVec b)],  
   accessDemandGrid ::  CubeGrid.Grid inst demDim label demVec b,
   accessSearchGrid ::  CubeGrid.Grid inst srchDim label srchVec b,
   accessVariation :: CubeSweep.Variation inst demDim srchDim label demVec srchVec a b,
   accessDemandCycle ::  OptSignal.DemandCycle inst demDim label sigVec a b,
   accessSupportSignal :: OptSignal.SupportSignal inst demDim label sigVec a b} 

data SweepResults node inst demandDim searchDim label demandVec searchVec a b = 
  SweepResults 
  {accessSweepFlow :: CubeSweep.FlowResult node inst demandDim searchDim label demandVec searchVec a b,
   accessSweepFlowStatus :: CubeSweep.FlowStatus inst demandDim searchDim label demandVec searchVec a,
   accessSweepEndNodePowers:: CubeSweep.EndNodeFlows node inst demandDim searchDim label demandVec searchVec a b
  }

data SweepEvaluationResults node inst demandDim searchDim label demandVec searchVec a b = 
  SweepEvaluationResults {accessSweepOptimality :: 
                             CubeSweep.OptimalityMeasure node inst demandDim searchDim label demandVec searchVec a b}


data OptimisationPerStateResults node inst dim label vec a b = OptimisationPerStateResults {
  accessOptimalChoicePerState :: CubeSweep.OptimalChoicePerState inst dim label vec a b,
  accessOptimalFlowPerState :: CubeSweep.OptimalFlowPerState node inst dim label vec a b, 
  accessOptimalControlSignalsPerState :: OptSignal.OptimalControlSignalsPerState node inst label vec a b,
  accessOptimalStoragePowersPerState :: OptSignal.OptimalStoragePowersPerState node inst label vec a b}


data OptimalOperation node inst label vec a b = OptimalOperation {
  accessOptimalStateChoice :: OptSignal.OptimalStateChoice inst label vec a b,
  accessOptimalControlSignals :: OptSignal.OptimalControlSignals node inst label vec a b,
  accessOptimalStorageSignals :: OptSignal.OptimalStoragePowers node inst label vec a b,
  accessBalance :: Balance.Balance node (Maybe (Interp.Val b))}



prepare ::
  (Eq (vec1 a),
   Ord label1,
   Ord a,
   DV.Zipper vec2,
   DV.Zipper vec1,
   DV.Walker vec,
   DV.Walker vec1,
   DV.Walker vec2,
   DV.Storage vec (ND.Data dim (Strict.SupportingPoints (Strict.Idx, a))),
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec2 (Collection.Collection label1 (CubeMap.Cube (Sweep.Search inst) dim1 label1 vec1 a (Interp.Val a))),
   DV.Storage vec1 (Interp.Val a),
   DV.Storage vec1 (vec1 [a]),
   DV.Storage vec1 [a],
   DV.Storage vec1 (ND.Data dim1 a),
   DV.Storage vec2 (vec2 [a]),
   DV.Storage vec2 [a],
   DV.Storage vec2 (ND.Data dim a),
   DV.Storage vec2 Bool,
   DV.Storage vec2 a,
   DV.Storage vec1 Bool,
   DV.Storage vec1 a,
   DV.Singleton vec2,
   DV.Singleton vec1,
   DV.LookupUnsafe vec2 a,
   DV.Length vec2,
   DV.FromList vec2,
   DV.FromList vec1,
   DV.Find vec2,
   ND.Dimensions dim,
   ND.Dimensions dim1) =>
  [(label1,Type.Dynamic,vec2 a)] ->
  [(label1,Type.Dynamic,vec1 a)] ->
  SignalFlow.Signal inst1 label vec a (ND.Data dim a) ->
  (CubeGrid.Grid inst2 dim label1 vec2 a,
   CubeGrid.Grid inst3 dim1 label1 vec1 a,
   CubeSweep.Variation inst dim dim1 label1 vec2 vec1 a (Interp.Val a),
   SignalFlow.Signal inst1 label vec a (ND.Data dim (Strict.SupportingPoints (Strict.Idx,a))))
prepare demandVariation searchVariation demandCycle = (demandGrid,searchGrid,sweepVariation,supportSignal)
  where
    demandGrid = CubeGrid.create (nc "Main") demandVariation
    searchGrid = CubeGrid.create (nc "Main") searchVariation
    sweepVariation = CubeSweep.generateVariation (nc "Main") demandGrid searchGrid
    supportSignal = OptSignal.getSupportPoints (nc "Main") demandGrid demandCycle 


-- | Only has to be calculated once, unless efficiency curves change
sweep ::
  (Ord a,
   Arith.Constant a,
   Node.C node,
   DV.Zipper vec1,
   DV.Walker vec1,
   DV.Walker vec,
   DV.Storage vec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
   DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec1 (Maybe Idx.AbsoluteState),
   DV.Storage vec1 ActFlowCheck.Validity,
   DV.Storage vec (TopoQty.Section node (Maybe (TopoQty.Flow (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)))))),   
   DV.Storage vec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Storage vec1 a,
   DV.Storage vec1 (Interp.Val a),
   DV.Storage vec1 Bool,
   DV.Storage vec (Collection.Collection (TopoIdx.Position node) (CubeMap.Cube (Sweep.Search inst) dim1 (TopoIdx.Position node) vec1 a (Interp.Val a))),
   DV.Singleton vec1,
   DV.Length vec1) =>
  Topo.Topology node ->
  EtaFunctions.FunctionMap node a ->
  CubeSweep.Variation inst dim dim1 (TopoIdx.Position node) vec vec1 a (Interp.Val a) ->
  (CubeSweep.FlowResult node inst dim dim1 (TopoIdx.Position node) vec vec1 a (Interp.Val a),
   CubeSweep.FlowStatus inst dim dim1 (TopoIdx.Position node) vec vec1 a,
   CubeSweep.EndNodeFlows node inst dim dim1 (TopoIdx.Position node) vec vec1 a (Interp.Val a))
sweep topology etaFunctions given = (energyFlow,flowStatus,endNodePowers) 
  where 
    energyFlow = CubeSweep.solve topology etaFunctions given 
    flowStatus = CubeSweep.getFlowStatus (nc "main") energyFlow
    endNodePowers = CubeSweep.getEndNodeFlows energyFlow

evaluateSweep ::
  (Eq (vec a),
   Eq label,
   Ord node,
   Ord b,
   Show node,
   Arith.Constant b,
   DV.Zipper vec,
   DV.Zipper vec1,
   DV.Walker vec1,
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                               (FlowOpt.TotalBalanceForce b,
                                                                                (FlowOpt.Eta2Optimise b,
                                                                                 FlowOpt.Loss2Optimise b))))),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
   DV.Storage vec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 b))),
   DV.Storage vec1 (FlowOpt.Eta2Optimise b,
                    FlowOpt.Loss2Optimise b),
   DV.Storage vec1 (FlowOpt.Loss2Optimise b),
   DV.Storage vec1 (FlowOpt.OptimalityMeasure b),
   DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityMeasure b),
   DV.Storage vec1 (FlowOpt.TotalBalanceForce b),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                               FlowOpt.OptimalityMeasure b))),
   DV.Storage vec1 (FlowOpt.Eta2Optimise b),
   DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec1 b,
   DV.Storage vec1 (b,
                    b),
   DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                    (b,
                     b)),
   DV.Singleton vec1,
   DV.Length vec1) =>
  Caller ->
  FlowOpt.LifeCycleMap node b ->
  
  CubeMap.Cube inst dim label vec a (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 b))) ->
  CubeMap.Cube inst dim label vec a (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 ActFlowCheck.EdgeFlowStatus)) ->
  CubeMap.Cube inst dim label vec a (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                                 FlowOpt.OptimalityMeasure b)))
evaluateSweep caller lifeCycleMap endNodePowers status = 
  CubeSweep.calculateOptimalityMeasure caller lifeCycleMap endNodePowers status
 

optimisationPerState ::
   (Eq (vec a),
    Eq label,
    Ord a,
    Ord node,
    Show (vec (ValueState.Map (Interp.Val a))),
    Show label,
    Show a,
    Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
    Show (vec (ValueState.Map (Maybe (Interp.Val a)))),
    Show node,
    Arith.Constant a,
    Node.C node1,
    DV.Zipper vec3,
    DV.Zipper vec,
    DV.Zipper vec1,
    DV.Walker vec,
    DV.Walker vec1,
    DV.Storage vec (ValueState.Map (Interp.Val a)),
    DV.Storage vec3 (ValueState.Map (Interp.Val a)),
    DV.Storage vec (ValueState.Map (Map.Map node1 (Maybe (Maybe (Interp.Val a))))),
    DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
    DV.Storage vec a,
    DV.Storage vec3 (ND.Data dim (Strict.SupportingPoints (Strict.Idx,
                                                           a))),
    DV.Storage vec3 (ND.Data dim a),
    DV.Storage vec3 (ValueState.Map (Maybe (Interp.Val a))),
    DV.Storage vec (TopoQty.Section node1 (Result.Result (CubeMap.Data (Sweep.Search inst) dim2 vec2 (Interp.Val a)))),
    DV.Storage vec (ValueState.Map (TopoQty.Section node1 (Interp.Val a))),
    DV.Storage vec (ValueState.Map (TopoQty.Section node1 (Result.Result (Interp.Val a)))),
    DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                    (ActFlowCheck.EdgeFlowStatus,
                                     FlowOpt.OptimalityValues (Interp.Val a)))),
    DV.Storage vec1 (CubeGrid.LinIdx,
                     (ActFlowCheck.EdgeFlowStatus,
                      FlowOpt.OptimalityValues (Interp.Val a))),
    DV.Storage vec1 (CubeGrid.LinIdx,
                     (ActFlowCheck.EdgeFlowStatus,
                      (Interp.Val a,
                       Interp.Val a))),
    DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                (FlowOpt.TotalBalanceForce (Interp.Val a),
                                                                                 (FlowOpt.Eta2Optimise (Interp.Val a),
                                                                                  FlowOpt.Loss2Optimise (Interp.Val a)))))),
    DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
    DV.Storage vec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (Interp.Val a)))),
    DV.Storage vec1 (FlowOpt.Eta2Optimise (Interp.Val a),
                     FlowOpt.Loss2Optimise (Interp.Val a)),
    DV.Storage vec1 (FlowOpt.Loss2Optimise (Interp.Val a)),
    DV.Storage vec1 (FlowOpt.OptimalityValues (Interp.Val a)),
    DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                     FlowOpt.OptimalityValues (Interp.Val a)),
    DV.Storage vec1 (FlowOpt.TotalBalanceForce (Interp.Val a)),
    DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                FlowOpt.OptimalityValues (Interp.Val a)))),
    DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                FlowOpt.OptimalityMeasure (Interp.Val a)))),
    DV.Storage vec1 (FlowOpt.Eta2Optimise (Interp.Val a)),
    DV.Storage vec1 (FlowOpt.OptimalityMeasure (Interp.Val a)),
    DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                     FlowOpt.OptimalityMeasure (Interp.Val a)),
    DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
    DV.Storage vec1 (Interp.Val a),
    DV.Storage vec1 (Interp.Val a,
                     Interp.Val a),
    DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                     (Interp.Val a,
                      Interp.Val a)),
    DV.Slice vec,
    DV.Singleton vec1,
    DV.LookupUnsafe vec2 (Interp.Val a),
    DV.LookupMaybe vec (ValueState.Map (Interp.Val a)),
    DV.LookupMaybe vec (ValueState.Map (Maybe (Interp.Val a))),
    DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
    DV.Length vec,
    DV.Length vec1) =>
   CubeMap.Cube (Sweep.Demand inst) dim label vec a (TopoQty.Section node1 (Result.Result (CubeMap.Data (Sweep.Search inst) dim2 vec2 (Interp.Val a)))) ->
   CubeMap.Cube (Sweep.Demand inst) dim label vec a (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (Interp.Val a)))) ->
   CubeMap.Cube (Sweep.Demand inst) dim label vec a (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                                                 FlowOpt.OptimalityMeasure (Interp.Val a)))) ->
   SignalFlow.Signal inst2 label1 vec3 a1 (ND.Data dim a) ->
   SignalFlow.Signal inst2 label1 vec3 a1 (ND.Data dim (Strict.SupportingPoints (Strict.Idx,
                                                                                 a))) ->
   [node1] ->
   FlowBal.Forcing node (Interp.Val a) ->
   [DemandAndControl.ControlVar node1] ->
   (CubeMap.Cube (Sweep.Demand inst) dim label vec a (Result.Result (CubeMap.Data (Sweep.Search inst1) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
                                                                                                                  FlowOpt.OptimalityValues (Interp.Val a)))),
    CubeMap.Cube (Sweep.Demand inst) dim label vec a (ValueState.Map (CubeGrid.LinIdx,
                                                                      (ActFlowCheck.EdgeFlowStatus,
                                                                       FlowOpt.OptimalityValues (Interp.Val a)))),
    CubeMap.Cube (Sweep.Demand inst) dim label vec a (ValueState.Map (TopoQty.Section node1 (Interp.Val a))),
    Map.Map node1 (SignalFlow.Signal inst2 label1 vec3 a1 (ValueState.Map (Maybe (Interp.Val a)))),
    Map.Map (DemandAndControl.ControlVar node1) (SignalFlow.Signal inst2 label1 vec3 a1 (ValueState.Map (Interp.Val a))))
optimisationPerState sweepCube endNodePowers optimalityMeasure demandCycle supportSignal storageList balanceForcingMap controlVars = 
  (objectiveFunctionValues,optimisationResultPerState,optimalFlowCube,optimalStoragePowersPerState,optimalControlSignalsPerState)
  where 
    objectiveFunctionValues = CubeSweep.objectiveFunctionValues (nc "main") balanceForcingMap endNodePowers optimalityMeasure
    optimisationResultPerState = CubeSweep.findMaximumEtaPerState  (nc "main") objectiveFunctionValues                          
                                        
    optimalFlowCube = CubeSweep.unresultOptimalFlowPerStateCube (nc "main") $ 
                        CubeSweep.getOptimalFlowPerStateCube (nc "main") optimisationResultPerState sweepCube
    
    optimalStoragePowersPerState = OptSignal.interpolateStoragePowersPerState (nc "main") Interp.Linear 
                              optimalFlowCube supportSignal demandCycle storageList                                   
    optimalControlSignalsPerState = OptSignal.interpolateControlSignalsPerState (nc "main") Interp.Linear 
                                       optimalFlowCube supportSignal demandCycle controlVars

-- | Has to be recalculated each time, the Balance-Forcing changes  
optimalOperation ::
  (Ord b,
   Show b,
   Show label1,
   Show (vec1 (ValueState.Map (CubeGrid.LinIdx,
                               (ActFlowCheck.EdgeFlowStatus,
                                FlowOpt.OptimalityValues (Interp.Val b))))),
   Arith.Constant a,
   Arith.Constant b,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (a,Maybe (Interp.Val a)),
   DV.Storage vec (Maybe (Interp.Val a)),
   DV.Storage vec [Maybe (Interp.Val a)],
   DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage vec a,
   DV.Storage vec [Interp.Val b],
   DV.Storage vec [a],
   DV.Storage vec ([Maybe Idx.AbsoluteState],Maybe (Interp.Val b)),
   DV.Storage vec (Interp.Val b),
   DV.Storage vec (Maybe (Interp.Val b)),
   DV.Storage vec (ValueState.Map (Interp.Val b)),
   DV.Storage vec (ND.Data dim (Strict.SupportingPoints (Strict.Idx,b))),
   DV.Storage vec (ND.Data dim b),
   DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))),
   DV.Storage vec1 b,
   DV.Storage vec1 (ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,
                                     FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Slice vec1,
   DV.Singleton vec,
   DV.LookupMaybe vec1 (ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,
                                         FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Length vec1,
   DV.FromList vec) =>
  CubeMap.Cube inst1 dim label1 vec1 b (ValueState.Map (CubeGrid.LinIdx,
                                                        (ActFlowCheck.EdgeFlowStatus,
                                                         FlowOpt.OptimalityValues (Interp.Val b)))) ->
  SignalFlow.Signal inst label vec a (ND.Data dim b) ->
  SignalFlow.Signal inst label vec a (ND.Data dim (Strict.SupportingPoints (Strict.Idx,b))) ->
  Map.Map node1 (SignalFlow.Signal inst label vec a (ValueState.Map (Maybe (Interp.Val a)))) ->
  Map.Map (DemandAndControl.ControlVar node) (SignalFlow.Signal inst label vec a (ValueState.Map (Interp.Val b))) ->
  (SignalFlow.Signal inst label vec a (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))),
   SignalFlow.Signal inst label vec a ([Maybe Idx.AbsoluteState],Maybe (Interp.Val b)),
   Map.Map (DemandAndControl.ControlVar node) (SignalFlow.Signal inst label vec a (Interp.Val b)),
   Map.Map node1 (SignalFlow.Signal inst label vec a (Maybe (Interp.Val a))),
   FlowBal.Balance node1 (Maybe (Interp.Val a)))
optimalOperation  optimisationResultPerState demandCycle supportSignal optimalStoragePowersPerState optimalControlSignalsPerState =
    (optimalStateSignals, 
     optimalStateSignal,
     optimalControlSignals,
     optimalStorageSignals,
     balance)
  where
    optimalStateSignals = OptSignal.optimalStateSignals (nc "main") optimisationResultPerState supportSignal demandCycle
    optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOff optimalStateSignals
    optimalControlSignals = OptSignal.generateOptimalControl optimalStateSignal optimalControlSignalsPerState                       
    optimalStorageSignals = OptSignal.generateOptimalStorageSignals optimalStateSignal optimalStoragePowersPerState
    balance = OptSignal.getBalance optimalStorageSignals 
  
  
{-  
-- | simulate and provide EFA with new LifeCycle-Efficiencies
simulateAndAnalyse ::   
-}
  


