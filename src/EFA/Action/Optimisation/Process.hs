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
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative

--import qualified EFA.Flow.Topology as FlowTopo

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.Process"

nc :: FunctionName -> Caller
nc = genCaller modul
                               

--data SystemDescription node inst =  SystemDescription {accessTopology :: }
-- data SystemInputData node inst = 
-- data OptimisationSettings = OptimisationSettings {}
-- data OutputSettings = 

data SweepAndDemandCycleData node inst demDim srchDim demVec srchVec sigVec a = 
  SweepAndDemandCycleData 
  {accessDemandVariation :: [(DemandAndControl.Var node,Type.Dynamic,demVec a)],
   accessSearchVariation :: [(DemandAndControl.Var node,Type.Dynamic,srchVec a)],  
   accessDemandGrid ::  CubeGrid.Grid inst demDim (DemandAndControl.Var node) demVec a,
   accessSearchGrid ::  CubeGrid.Grid inst srchDim (DemandAndControl.Var node) srchVec a,
   accessVariation :: CubeSweep.Variation node inst demDim srchDim demVec srchVec a (Interp.Val a),
   accessDemandCycle ::  OptSignal.DemandCycle node inst demDim sigVec a a,
   accessSupportSignal :: OptSignal.SupportSignal node inst demDim sigVec a a} 

data SweepResults node inst demDim srchDim demVec srchVec a = 
  SweepResults 
  {accessSweepFlow :: CubeSweep.FlowResult node inst demDim srchDim demVec srchVec a (Interp.Val a),
   accessSweepFlowStatus :: CubeSweep.FlowStatus node inst demDim srchDim demVec srchVec a,
   accessSweepEndNodePowers:: CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a (Interp.Val a)
  }

data SweepEvaluationResults node inst demDim srchDim demVec srchVec a = 
  SweepEvaluationResults {accessSweepOptimality :: 
                             CubeSweep.OptimalityMeasure node inst demDim srchDim demVec srchVec a a}


data OptimisationPerStateResults node inst demDim srchDim demVec srchVec sigVec a = OptimisationPerStateResults {
  accessObjectiveFunctionValues :: CubeSweep.ObjectiveFunctionValues node inst demDim srchDim demVec srchVec a (Interp.Val a),
  accessOptimalChoicePerState :: CubeSweep.OptimalChoicePerState node inst demDim demVec a (Interp.Val a),
  accessOptimalFlowPerState :: CubeSweep.OptimalFlowPerState node inst demDim demVec a (Interp.Val a), 
  accessOptimalStateSignals :: OptSignal.OptimalityPerStateSignal node inst sigVec a (Interp.Val a), 
  accessOptimalControlSignalsPerState :: OptSignal.OptimalControlSignalsPerState node inst sigVec a (Interp.Val a),
  accessOptimalStoragePowersPerState :: OptSignal.OptimalStoragePowersPerState node inst sigVec a (Interp.Val a)}


data OptimalOperation node inst vec a = OptimalOperation {
  accessOptimalStateChoice :: OptSignal.OptimalStateChoice node inst vec a (Interp.Val a),
  accessOptimalControlSignals :: OptSignal.OptimalControlSignals node inst vec a (Interp.Val a),
  accessOptimalStorageSignals :: OptSignal.OptimalStoragePowers node inst vec a (Interp.Val a),
  accessBalance :: Balance.Balance node (Maybe (Interp.Val a))}



prepare ::
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
  SweepAndDemandCycleData node inst demDim srchDim demVec srchVec sigVec a
prepare demandVariation searchVariation demandCycle = 
  SweepAndDemandCycleData demandVariation searchVariation demandGrid searchGrid 
   sweepVariation demandCycle supportSignal
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
   DV.Zipper srchVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (TopoQty.Section node (Maybe (TopoQty.Flow (Result.Result 
                                                                 (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))))),
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
   DV.Length srchVec) =>
  Topo.Topology node ->
  EtaFunctions.FunctionMap node a ->
  CubeSweep.Variation node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  SweepResults node inst demDim srchDim demVec srchVec a
sweep topology etaFunctions given = SweepResults energyFlow flowStatus endNodePowers 
  where 
    energyFlow = CubeSweep.solve topology etaFunctions given 
    flowStatus = CubeSweep.getFlowStatus (nc "main") energyFlow
    endNodePowers = CubeSweep.getEndNodeFlows energyFlow

evaluateSweep :: 
  (Eq (demVec a),
   Ord node,
   Ord a,
   Show node,
   Arith.Constant a,
   DV.Zipper demVec,
   DV.Zipper srchVec,
   DV.Walker srchVec,
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       (FlowOpt.TotalBalanceForce a,
                                                                                        (FlowOpt.Eta2Optimise a,
                                                                                         FlowOpt.Loss2Optimise a))))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus)),
   DV.Storage demVec (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec a))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a,
                       FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.Loss2Optimise a),
   DV.Storage srchVec (FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure a),
   DV.Storage srchVec (FlowOpt.TotalBalanceForce a),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityMeasure a))),
   DV.Storage srchVec (FlowOpt.Eta2Optimise a),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec a,
   DV.Storage srchVec (a, a),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus, (a,a)),
   DV.Singleton srchVec,
   DV.Length srchVec) =>
  Caller ->
  FlowOpt.LifeCycleMap node a ->
  CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a a ->
  CubeSweep.FlowStatus node inst demDim srchDim demVec srchVec a ->
  SweepEvaluationResults node inst demDim srchDim demVec srchVec a
evaluateSweep caller lifeCycleMap endNodePowers status = 
  SweepEvaluationResults $ CubeSweep.calculateOptimalityMeasure caller lifeCycleMap endNodePowers status
 

{- optimisationPerState :: 
  CubeSweep.FlowResult node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  CubeSweep.OptimalityMeasure node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  OptSignal.DemandCycle node inst demDim sigVec a a ->
  OptSignal.SupportSignal node inst demDim sigVec a a ->
  [node] ->
  FlowBal.Forcing node (Interp.Val a)  ->
  [DemandAndControl.ControlVar node] ->
  OptimisationPerStateResults node inst demDim srchDim demVec srchVec sigVec a -}

optimisationPerState ::
  (Eq (demVec a),
   Ord a,
   Show (sigVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (demVec (ValueState.Map (CubeGrid.LinIdx,
                                 (ActFlowCheck.EdgeFlowStatus,
                                  FlowOpt.OptimalityValues (Interp.Val a))))),
   Show (demVec (ValueState.Map (Interp.Val a))),
   Show a,
   Show (demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (demVec (ValueState.Map (Maybe (Interp.Val a)))),
   Show node,
   Arith.Constant a,
   Node.C node,
   DV.Zipper sigVec,
   DV.Zipper demVec,
   DV.Zipper srchVec,
   DV.Walker demVec,
   DV.Walker srchVec,
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage sigVec a,
   DV.Storage demVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage demVec (ValueState.Map (Map.Map node (Maybe (Maybe (Interp.Val a))))),
   DV.Storage demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage demVec a,
   DV.Storage sigVec (ND.Data demDim (Strict.SupportingPoints (Strict.Idx,
                                                               a))),
   DV.Storage sigVec (ND.Data demDim a),
   DV.Storage sigVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage demVec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
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
   DV.Slice sigVec,
   DV.Slice demVec,
   DV.Singleton srchVec,
   DV.LookupUnsafe vec1 (Interp.Val a),
   DV.LookupMaybe demVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe sigVec (ValueState.Map (CubeGrid.LinIdx,
                                          (ActFlowCheck.EdgeFlowStatus,
                                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.LookupMaybe demVec (ValueState.Map (Interp.Val a)),
   DV.LookupMaybe demVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.LookupMaybe demVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Length sigVec,
   DV.Length demVec,
   DV.Length srchVec) =>
  CubeSweep.FlowResult node inst demDim dim1 demVec vec1 a (Interp.Val a) ->
  CubeSweep.EndNodeFlows node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  CubeSweep.OptimalityMeasure node inst demDim srchDim demVec srchVec a (Interp.Val a) ->
  OptSignal.DemandCycle node inst demDim sigVec a a ->
  OptSignal.SupportSignal node inst demDim sigVec a a ->
  [node] ->
  FlowBal.Forcing node (Interp.Val a) ->
  [DemandAndControl.ControlVar node] ->
  OptimisationPerStateResults node inst demDim srchDim demVec srchVec sigVec a
optimisationPerState sweepCube endNodePowers optimalityMeasure demandCycle supportSignal storageList balanceForcingMap controlVars = 
  OptimisationPerStateResults
  objectiveFunctionValues    
  optimisationResultPerState
  optimalFlowCube
  optimalStateSignals
  optimalControlSignalsPerState 
  optimalStoragePowersPerState
  where 
    objectiveFunctionValues = CubeSweep.objectiveFunctionValues (nc "main") balanceForcingMap endNodePowers optimalityMeasure
    optimisationResultPerState = CubeSweep.findMaximumEtaPerState  (nc "main") objectiveFunctionValues                          
    optimalFlowCube = CubeSweep.unresultOptimalFlowPerStateCube (nc "main") $ 
                        CubeSweep.getOptimalFlowPerStateCube (nc "main") optimisationResultPerState sweepCube
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
       DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
       DV.Singleton vec,
       DV.FromList vec) =>
 OptSignal.OptimalityPerStateSignal node inst vec a (Interp.Val a) ->
 OptSignal.OptimalStoragePowersPerState node inst vec a (Interp.Val a) ->
 OptSignal.OptimalControlSignalsPerState node inst vec a (Interp.Val a) ->
 OptimalOperation node inst vec a
optimalOperation   optimalStateSignals optimalStoragePowersPerState optimalControlSignalsPerState =
  OptimalOperation optimalStateSignal optimalControlSignals
     optimalStorageSignals balance
  where
    
    optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOff optimalStateSignals
    optimalControlSignals = OptSignal.generateOptimalControl optimalStateSignal optimalControlSignalsPerState                       
    optimalStorageSignals = OptSignal.generateOptimalStorageSignals optimalStateSignal optimalStoragePowersPerState
    balance = OptSignal.getBalance optimalStorageSignals 
  
  
{-  
-- | simulate and provide EFA with new LifeCycle-Efficiencies
simulateAndAnalyse ::   
-}
  


