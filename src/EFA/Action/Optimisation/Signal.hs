{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Signal where

import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Action.DemandAndControl as DemandAndControl

import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.ND as ND
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Value.State as ValueState
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.ND.Cube.Map as CubeMap

import qualified Data.Map as Map

import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Signal"

nc :: FunctionName -> Caller
nc = genCaller modul

data StateForcing = StateForcingOn | StateForcingOff deriving (Show)

-- | Signal Containing Indices and ccordinates of supporting points holding the interpolation tiles 
getSupportPoints ::
  (Ord b,
   DV.Walker vec,
   DV.Storage vec1 b,
   DV.Storage vec (ND.Data dim b),
   DV.Storage vec (ND.Data dim (Strict.SupportingPoints (Strict.Idx, b))),
   DV.LookupUnsafe vec1 b,
   DV.Length vec1,
   DV.Find vec1) =>
  Caller ->
  CubeGrid.Grid inst1 dim label1 vec1 b ->
  SignalFlow.Signal inst label vec a (ND.Data dim b) ->
  SignalFlow.Signal inst label vec a (ND.Data dim (Strict.SupportingPoints (Strict.Idx,b)))
getSupportPoints caller demandGrid demandCycle = 
  SignalFlow.map (CubeGrid.getSupportingPoints (caller |> nc "getSupportPoints") 
                  demandGrid) demandCycle
{-
lookupSupportingPoints ::
  (DV.Storage vec a, DV.LookupUnsafe vec b, DV.Length vec,
   ND.Dimensions dim) =>
  Caller -> Cube inst dim label vec a b -> ND.Data dim (Strict.SupportingPoints (Strict.Idx, a)) -> [b]
lookupSupportingPoints caller cube support = P.map (lookupLinUnsafe cube) linIndices
  where
    linIndices = Grid.getSupportingPointLinearIndices caller (getGrid cube) support
-}

optimalStateSignals ::
  (Ord b,
 Show (vec1 (ValueState.Map (CubeGrid.LinIdx,
 (ActFlowCheck.EdgeFlowStatus,
 FlowOpt.OptimalityValues (Interp.Val b))))),
 Show label1,
 Show b,Arith.Constant b,
 DV.Zipper vec,
 DV.Storage vec1 (ValueState.Map (CubeGrid.LinIdx,
 (ActFlowCheck.EdgeFlowStatus,
 FlowOpt.OptimalityValues (Interp.Val b)))),
 DV.Storage vec1 b,
 DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))),
 DV.Storage vec (ND.Data dim b),
 DV.Storage vec (ND.Data dim (Strict.SupportingPoints (Strict.Idx,
 b))),
 DV.Slice vec1,
 DV.LookupMaybe vec1 (ValueState.Map (CubeGrid.LinIdx,
 (ActFlowCheck.EdgeFlowStatus,
 FlowOpt.OptimalityValues (Interp.Val b)))),
 DV.Length vec1) =>
 Caller -> 
 CubeMap.Cube inst1 dim label1 vec1 b (ValueState.Map (CubeGrid.LinIdx,
 (ActFlowCheck.EdgeFlowStatus,
 FlowOpt.OptimalityValues (Interp.Val b)))) ->
 SignalFlow.Signal inst label vec a (ND.Data dim (Strict.SupportingPoints (Strict.Idx,
 b))) ->
 SignalFlow.Signal inst label vec a (ND.Data dim b) ->
 SignalFlow.Signal inst label vec a (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b)))
optimalStateSignals caller optimumResultCube supportPoints demandCycle = 
  SignalFlow.zipWith 
  (CubeSweep.interpolateOptimalityValuesWithSupportPerState (caller |> nc "optimalStateSignals") 
                            Interp.Linear
                            optimumResultCube) 
        supportPoints demandCycle    

findOptimalStatesUsingMaxEta ::
  (Ord b,Arith.Constant b,
   DV.Storage vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))),
   DV.Storage vec (ValueState.Map (Interp.Val b)),
 DV.Walker vec,DV.Storage vec (Maybe (Interp.Val b)),
 DV.Storage vec (Interp.Val b),
 DV.Zipper vec,
 DV.Storage vec ([Maybe Idx.AbsoluteState],
 Maybe (Interp.Val b))) =>
 Caller -> 
 StateForcing ->
 SignalFlow.Signal inst label vec a (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))) ->
 SignalFlow.Signal inst label vec a ([Maybe Idx.AbsoluteState],
 Maybe (Interp.Val b))
findOptimalStatesUsingMaxEta _ StateForcingOff optimalitySignalPerState = 
  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid . ValueState.map FlowOpt.getOptEtaVal) optimalitySignalPerState

-- | in Case of Stateforcing each State has to occur at least one
-- get total maximum
-- get minimal difference from maximum per state and correct the signals with that value before maxing
findOptimalStatesUsingMaxEta caller StateForcingOn optimalitySignalPerState = 
  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid) conditionedSignal 
  where 
    etaOptSignal = SignalFlow.map (ValueState.map FlowOpt.getOptEtaVal) optimalitySignalPerState
    etaOptSignalMax = SignalFlow.map (Maybe.fromMaybe err2 . snd) $ 
                      findOptimalStatesUsingMaxEta (caller |> nc "findOptimalStatesUsingMaxEta") 
                      StateForcingOff optimalitySignalPerState 
    diffSignal = SignalFlow.zipWith (\m x -> ValueState.map (Arith.~- x) m) etaOptSignal etaOptSignalMax              
    minDifferencePerState = Maybe.fromMaybe err $ SignalFlow.foldl f (Nothing) diffSignal
    f (Nothing) y = Just y
    f (Just x) y = Just $ ValueState.maxWith (Interp.compareMinWithInvalid) x y
    err = merror caller modul "findOptimalStatesUsingMaxEta" "empty Signal"
    err2 = merror caller modul "findOptimalStatesUsingMaxEta" "no State found"
    conditionedSignal = SignalFlow.map (\ m -> ValueState.zipWith (Arith.~-) m minDifferencePerState) etaOptSignal 

generateOptimalSignal:: 
  (Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec [Interp.Val b],
   DV.Storage vec (Interp.Val b),
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.Storage vec (ValueState.Map (Interp.Val b)),
   DV.Storage vec ([Maybe Idx.AbsoluteState], Maybe (Interp.Val b)),
   DV.FromList vec) =>
  SignalFlow.Signal inst label vec a ([Maybe Idx.AbsoluteState], Maybe (Interp.Val b)) ->
  SignalFlow.Signal inst label vec a (ValueState.Map (Interp.Val b)) -> 
  SignalFlow.Signal inst1 label vec a (Interp.Val b)
generateOptimalSignal optimalStateSignal stateSignal = SignalFlow.concatEvenEvenTimeShare
   $ SignalFlow.zipWith f optimalStateSignal stateSignal
  where f (states,_) m = map (ValueState.lookupUnsafe m) states
    
interpolateControlSignalsPerState ::
  (Ord a,
   Ord node,
   Show (vec (ValueState.Map (Interp.Val a))),
   Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show a,
   Show label,
   Arith.Constant a,
   DV.Zipper vec1,
   DV.Walker vec,
   DV.Storage vec1 (ValueState.Map (Interp.Val a)),
   DV.Storage vec1 (ND.Data dim a),
   DV.Storage vec1 (ND.Data dim (Strict.SupportingPoints (Strict.Idx, a))),
   DV.Storage vec a,
   DV.Storage vec (ValueState.Map (TopoQty.Section node (Interp.Val a))),
   DV.Storage vec (ValueState.Map (Interp.Val a)),
   DV.Slice vec,
   DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe vec (ValueState.Map (Interp.Val a)),
   DV.Length vec) =>
  Caller ->
  Interp.Method a ->
  CubeMap.Cube inst dim label vec a (ValueState.Map (TopoQty.Section node (Interp.Val a))) ->
  SignalFlow.Signal inst1 label1 vec1 a1 (ND.Data dim (Strict.SupportingPoints (Strict.Idx,a))) ->
  SignalFlow.Signal inst1 label1 vec1 a1 (ND.Data dim a) ->
  [DemandAndControl.ControlVar node] ->
  Map.Map (DemandAndControl.ControlVar node) (SignalFlow.Signal inst1 label1 vec1 a1 (ValueState.Map (Interp.Val a)))
interpolateControlSignalsPerState caller inmethod flowCube supportSig coordinateSig controlVars = 
  Map.fromList $ zip controlVars $ map f controlVars
  where f var = SignalFlow.zipWith g supportSig coordinateSig
          where 
            g support coordinates = CubeSweep.interpolateWithSupportPerState caller inmethod varCube support coordinates
            varCube = CubeMap.map (\ x -> CubeSweep.lookupControlVariablePerState caller x var) flowCube
        

interpolateStoragePowersPerState caller inmethod flowCube supportSig coordinateSig = 
  Map.fromList $ zip controlVars $ map f controlVars
  where f var = SignalFlow.zipWith g supportSig coordinateSig
          where 
            g support coordinates = CubeSweep.interpolateWithSupportPerState caller inmethod varCube support coordinates
            varCube = CubeMap.map (\ x -> CubeSweep.lookupControlVariablePerState caller x var) flowCube
        
