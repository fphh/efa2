{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Signal where

import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph
import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Flow.Topology.Index as XIdx
--import qualified EFA.Utility.Trace as UtTrace
--import qualified EFA.Action.Optimisation.Sweep as Sweep
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
import qualified EFA.Action.Utility as ActUt

import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Signal"

nc :: FunctionName -> Caller
nc = genCaller modul

data StateForcing = StateForcingOn | StateForcingOff deriving (Show)

type DemandCycle node inst dim vec a b = SignalFlow.Signal inst String vec a (ND.Data dim b)

type DemandCycleRec node inst vec a b = SignalFlow.HRecord (DemandAndControl.DemandVar node) inst String vec a b

type DemandCycleMap node inst vec a b = 
  Map.Map (DemandAndControl.DemandVar node) (SignalFlow.Signal inst String vec a b)

type SupportSignal node inst dim  vec a b = 
  SignalFlow.Signal inst String vec a (ND.Data dim (Strict.SupportingPoints (Strict.Idx,b))) 
  
type OptimalityPerStateSignal node inst vec a b = 
  SignalFlow.Signal inst String vec a (ValueState.Map (FlowOpt.OptimalityValues b))

type OptimalControlSignalsPerState node inst vec a b = 
  Map.Map (DemandAndControl.ControlVar node) (SignalFlow.Signal inst String vec a (ValueState.Map b))
--  OptimalControlSignalsPerState (SignalFlow.HRecord (DemandAndControl.ControlVar node) inst String vec a (ValueState.Map b))

type OptimalStoragePowersPerState node inst  vec a b = 
  Map.Map node (SignalFlow.Signal inst String vec a (ValueState.Map (Maybe b)))
--   OptimalStoragePowersPerState (SignalFlow.HRecord node inst String vec a (ValueState.Map (Maybe b)))

type OptimalStateChoice node inst vec a b = 
  SignalFlow.Signal inst String vec a ([Maybe Idx.AbsoluteState],Maybe b)

type OptimalControlSignals node inst  vec a b = 
  Map.Map (DemandAndControl.ControlVar node) (SignalFlow.Signal inst String vec a b)

type OptimalStoragePowers node inst vec a b = Map.Map (node) (SignalFlow.Signal inst String vec a (Maybe b))


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
  DemandCycle node inst dim vec a b ->
--  SignalFlow.Signal inst (DemandAndControl.Var node) vec a (ND.Data dim b) ->
  SupportSignal node inst dim vec a b
getSupportPoints caller demandGrid demandCycle = 
  SignalFlow.map (CubeGrid.getSupportingPoints (caller |> nc "getSupportPoints") demandGrid) demandCycle

optimalStateSignals ::
  (Ord b,Show node,
   DV.Storage sigVec b,
   DV.Slice sigVec,
   DV.LookupMaybe sigVec (ValueState.Map (CubeGrid.LinIdx,
                                       (ActFlowCheck.EdgeFlowStatus,
                                        FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Length sigVec,
   Show (demVec (ValueState.Map (CubeGrid.LinIdx,
                               (ActFlowCheck.EdgeFlowStatus,
                                FlowOpt.OptimalityValues (Interp.Val b))))),
   Show (sigVec (ValueState.Map (CubeGrid.LinIdx,
                              (ActFlowCheck.EdgeFlowStatus,
                               FlowOpt.OptimalityValues (Interp.Val b))))),
   DV.Storage sigVec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val b)))),
   Show b,Arith.Constant b,
   DV.Zipper sigVec,
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                    (ActFlowCheck.EdgeFlowStatus,
                                     FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Storage demVec b,
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b))),
   DV.Storage sigVec (ND.Data dim b),
   DV.Storage sigVec (ND.Data dim (Strict.SupportingPoints (Strict.Idx,
                                                         b))),
   DV.Slice demVec,
   DV.LookupMaybe demVec (ValueState.Map (CubeGrid.LinIdx,
                                        (ActFlowCheck.EdgeFlowStatus,
                                         FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Length demVec
  ) =>
  Caller -> 
  CubeSweep.OptimalChoicePerState node inst dim demVec b (Interp.Val b) ->
  SupportSignal node inst dim sigVec a b ->
  DemandCycle node inst dim sigVec a b ->
  OptimalityPerStateSignal node inst sigVec a (Interp.Val b)
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
 OptimalityPerStateSignal inst label vec a (Interp.Val b) ->
 OptimalStateChoice inst label vec a (Interp.Val b)
findOptimalStatesUsingMaxEta _ StateForcingOff optimalitySignalPerState = 
  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid . 
                                       ValueState.map FlowOpt.getOptEtaVal) optimalitySignalPerState

-- | in Case of Stateforcing each State has to occur at least one
-- get total maximum
-- get minimal difference from maximum per state and correct the signals with that value before maxing
findOptimalStatesUsingMaxEta caller StateForcingOn optimalitySignalPerState = 
  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid) conditionedSignal 
  where 
    etaOptSignal = SignalFlow.map (ValueState.map FlowOpt.getOptEtaVal) optimalitySignalPerState
    etaOptSignalMax = SignalFlow.map (Maybe.fromMaybe err2 . snd) maxSig 
    maxSig = findOptimalStatesUsingMaxEta (caller |> nc "findOptimalStatesUsingMaxEta") 
                      StateForcingOff optimalitySignalPerState 
    diffSignal = SignalFlow.zipWith (\opt optMax -> ValueState.map (\x -> optMax Arith.~- x) opt) etaOptSignal etaOptSignalMax              
    minDifferencePerState = Maybe.fromMaybe err $ SignalFlow.foldl f (Nothing) diffSignal
    f (Nothing) y = Just y
    f (Just x) y = Just $ ValueState.minWith (Interp.compareMinWithInvalid) x y
    err = merror caller modul "findOptimalStatesUsingMaxEta" "empty Signal"
    err2 = merror caller modul "findOptimalStatesUsingMaxEta" "no State found"
    conditionedSignal = SignalFlow.map (\ m -> ValueState.zipWith (Arith.~+) m minDifferencePerState) etaOptSignal 



interpolateControlSignalsPerState ::
  (Ord a,
   Ord node,
   Show (vec (ValueState.Map (Interp.Val a))),
   Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show a,
   Show node,
   Arith.Constant a,
   DV.Zipper sigVec,
   DV.Walker vec,
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ND.Data dim a),
   DV.Storage sigVec (ND.Data dim (Strict.SupportingPoints (Strict.Idx, a))),
   DV.Storage vec a,
   DV.Storage vec (ValueState.Map (TopoQty.Section node (Interp.Val a))),
   DV.Storage vec (ValueState.Map (Interp.Val a)),
   DV.Slice vec,
   DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe vec (ValueState.Map (Interp.Val a)),
   DV.Length vec) =>
  Caller ->
  Interp.Method a ->
--  CubeMap.Cube inst dim label vec a (ValueState.Map (TopoQty.Section node (Interp.Val a))) ->
  CubeSweep.OptimalFlowPerState node inst dim vec a (Interp.Val a) ->
  SupportSignal node inst dim sigVec a a ->
  DemandCycle node inst dim sigVec a a ->
  [DemandAndControl.ControlVar node] ->
  OptimalControlSignalsPerState node inst sigVec a (Interp.Val a) 
interpolateControlSignalsPerState caller inmethod flowCube 
  supportSig demandCycle controlVars = 
  Map.fromList $ zip controlVars $ map f controlVars
  where f var = SignalFlow.zipWith g supportSig demandCycle
          where 
            g support coordinates = CubeSweep.interpolateWithSupportPerState caller inmethod varCube support coordinates
            varCube = CubeMap.map (\ x -> CubeSweep.lookupControlVariablePerState caller x var) flowCube

interpolateStoragePowersPerState ::
  (Ord a,
   Show (vec (ValueState.Map (Maybe (Interp.Val a)))),
   Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show a,
   Show node,
   Arith.Constant a,
   Node.C node,
   DV.Zipper sigVec,
   DV.Walker vec,
   DV.Storage sigVec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage sigVec (ND.Data dim a),
   DV.Storage sigVec (ND.Data dim (Strict.SupportingPoints (Strict.Idx,a))),
   DV.Storage vec a,
   DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage vec (ValueState.Map (FlowTopo.Section node Graph.EitherEdge (Interp.Val a) (TopoQty.Sums (Interp.Val a)) (Maybe (TopoQty.Flow (Interp.Val a))))),
   DV.Storage vec (ValueState.Map (Map.Map node (Maybe (FlowOpt.StorageFlow (Interp.Val a))))),
   DV.Slice vec,
   DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.LookupMaybe vec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Length vec) =>
  Caller ->
  Interp.Method a ->
  CubeSweep.OptimalFlowPerState node inst dim vec a (Interp.Val a) ->
  SupportSignal node inst dim sigVec a a ->
  DemandCycle node inst dim sigVec a a ->
  [node] ->
  OptimalStoragePowersPerState node inst sigVec a (Interp.Val a)
interpolateStoragePowersPerState caller inmethod flowCube 
  supportSig demandCycle storageList = 
  Map.fromList $ zip storageList $ map f storageList
   where stoCube = CubeMap.map (ValueState.map getStoragePowers) flowCube
         newCaller = caller |> nc "interpolateStoragePowersPerState" 
         getStoragePowers flowSection = let 
                topo = TopoQty.topology flowSection
                nodes = Graph.nodeLabels topo
                storages = Map.mapWithKey (\node _ -> TopoQty.lookupSums node flowSection) 
                           $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
                in Map.map (FlowOpt.getStoragePowerWithSignNew newCaller) storages 
         f sto = SignalFlow.zipWith g supportSig demandCycle
           where 
             g support coordinates = CubeSweep.interpolateWithSupportPerStateMaybe newCaller
                                     inmethod stoPowerCube support coordinates
             stoPowerCube = CubeMap.map (ValueState.map (fmap FlowOpt.unStorageFlow . flip (Map.!) sto)) stoCube
         
generateOptimalControl ::
  (Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec ([Maybe Idx.AbsoluteState], Maybe (Interp.Val b)),
   DV.Storage vec (ValueState.Map (Interp.Val b)),
   DV.Storage vec [SignalFlow.TimeStep a],
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec [a],
   DV.Storage vec (Interp.Val b),
   DV.Storage vec [Interp.Val b],
   DV.Storage vec a,
   DV.Singleton vec,
   DV.FromList vec) =>
  OptimalStateChoice node inst vec a (Interp.Val b) ->
  OptimalControlSignalsPerState node inst vec a (Interp.Val b) ->
  OptimalControlSignals node inst vec a (Interp.Val b)
generateOptimalControl optimalStateSignal optimalControlSignalsPerState = 
  Map.map f optimalControlSignalsPerState 
  where f sig = generateOptimalSignal optimalStateSignal sig
        
generateOptimalStorageSignals :: 
  (DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Storage vec [Maybe (Interp.Val a)],
   DV.Storage vec (Maybe (Interp.Val a)), 
   Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec [a],
   DV.Storage vec [SignalFlow.TimeStep a],
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec a,
   DV.Storage vec ([Maybe Idx.AbsoluteState], Maybe (Interp.Val a)),
   DV.Singleton vec,
   DV.FromList vec) =>  
  OptimalStateChoice node inst vec a (Interp.Val a) ->
  OptimalStoragePowersPerState node inst vec a (Interp.Val a) ->
  OptimalStoragePowers node inst vec a (Interp.Val a)
generateOptimalStorageSignals optimalStateSignal storagePowerSignalPerState = 
  Map.map f storagePowerSignalPerState
  where f sig =  generateOptimalSignal optimalStateSignal sig
        
-- TODO  -- check if Balance is calculated on power or energy signals
getBalance :: 
  (Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (a, Maybe (Interp.Val a)),
   DV.Storage vec (SignalFlow.TimeStep a, Maybe (Interp.Val a)),
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec (Maybe (Interp.Val a)),
   DV.Storage vec a) =>       
  OptimalStoragePowers node inst vec a (Interp.Val a) ->
  Balance.Balance node (Maybe (Interp.Val a))
getBalance storageSignals = Balance.Balance $ Map.map f storageSignals
  where f storageSignal = SignalFlow.foldlWithTime g Nothing storageSignal
        g _ (_,Nothing) = Nothing
        g Nothing (t,Just x) = Just x --Just ((Interp.Inter t) Arith.~* x)
        g (Just acc) (t, Just x) = Just $ x Arith.~+ acc  -- ++ Just $ ((Interp.Inter t) Arith.~* x)  Arith.~+ acc        
        
-- TODO: make sure this is not applied to Energy Signals (Time Splitting than would require to alter signal values)
generateOptimalSignal ::
  (Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (ValueState.Map b),
   DV.Storage vec ([Maybe Idx.AbsoluteState],c),
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec [SignalFlow.TimeStep a],
   DV.Storage vec a,
   DV.Storage vec [b],
   DV.Storage vec b,
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList vec) =>
  SignalFlow.Signal inst label vec a ([Maybe Idx.AbsoluteState],c) ->
  SignalFlow.Signal inst label vec a (ValueState.Map b) ->
  SignalFlow.Signal inst1 label vec a b
generateOptimalSignal optimalStateSignal stateSignal = SignalFlow.concatEvenEvenTimeShare
   $ SignalFlow.zipWith f optimalStateSignal stateSignal
  where f (states,_) m = map (ValueState.lookupUnsafe m) states    


convertToDemandCycleRecord :: 
  (Ord node, 
   DV.Walker vec,
   DV.Storage vec (ND.Data dim b),
   DV.Storage vec b) =>
  DemandCycle node inst dim vec a b ->
  [DemandAndControl.DemandVar node] ->
  DemandCycleRec node inst vec a b
convertToDemandCycleRecord (SignalFlow.Signal t dat) demandVars = 
  SignalFlow.HRecord t (Map.fromList $ zip demandVars $ map f [0..(length demandVars)-1])
  where 
    f index = SignalFlow.mapData (flip ND.unsafeLookup (ND.Idx index)) dat

convertToDemandCycleMap :: 
  (Ord node, 
   DV.Walker vec,
   DV.Storage vec (ND.Data dim b),
   DV.Storage vec b) =>
  DemandCycle node inst dim vec a b ->
  [DemandAndControl.DemandVar node] ->
  DemandCycleMap node inst vec a b
convertToDemandCycleMap sig demandVars = 
  Map.fromList $ zip demandVars $ map f [0..(length demandVars)-1]
  where 
    f index = SignalFlow.map (flip ND.unsafeLookup (ND.Idx index)) sig
    
makeGivenRecord :: 
  (Ord node, 
   DV.Walker sigVec,
   DV.Storage sigVec a,
   DV.Storage sigVec (Interp.Val a), 
   DV.Zipper sigVec,
   DV.Storage sigVec (sigVec a), Arith.Constant a,
   DV.Singleton sigVec,Show (sigVec a), Show node,
   DV.FromList sigVec, 
   Show (sigVec (SignalFlow.TimeStep a)),
   Show (sigVec Int),
   DV.Storage sigVec ([Maybe Idx.AbsoluteState], c),
   DV.Storage sigVec Int) =>
  Caller ->
  SignalFlow.Signal inst String sigVec a ([Maybe Idx.AbsoluteState],c) ->
  DemandCycleMap node inst sigVec a a ->
  OptimalControlSignals node inst1 sigVec a (Interp.Val a) ->
  SignalFlow.HRecord (XIdx.Position node) inst1 String sigVec a (Interp.Val a) 
makeGivenRecord caller optimalStateSignal demand control = 
  SignalFlow.HRecord  (--UtTrace.simTrace "Time" 
                       time) (Map.map SignalFlow.getData m)
  where 
    (SignalFlow.Signal time _, _) = Maybe.fromMaybe err $ Map.minView control
    err = merror caller modul "makeGivenRecord" "empty ControlSignalMap"
    demandInter = Map.map (SignalFlow.map Interp.Inter) demandRepli 
    demandRepli = --UtTrace.simTrace "demandRepli" $ 
                  Map.map (SignalFlow.replicateSamples numSig) demand
    numSig = --UtTrace.simTrace "numSig" $  
             SignalFlow.map (\(states,_) -> length states) optimalStateSignal
    m = Map.union (Map.mapKeys g control)
                  (Map.mapKeys h demandInter)
    -- TODO:: Variablen in die Generierung des Simulationsgleichungssystems reinschleifen !!    
    g (DemandAndControl.ControlPower (XIdx.Power x)) = x
    g _ = error "makeGivenRecord -- not yet supported"
    h (DemandAndControl.DemandPower (XIdx.Power x)) = x
    h _ = error "makeGivenRecord -- not yet supported"
  