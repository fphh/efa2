{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Signal where
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

import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Signal"

nc :: FunctionName -> Caller
nc = genCaller modul

data StateForcing = StateForcingOn | StateForcingOff deriving (Show)

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

  