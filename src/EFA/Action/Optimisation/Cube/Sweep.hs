{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Action.Optimisation.Cube.Sweep where
import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Graph as Graph 
import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Balance as FlowBal
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
-- import qualified EFA.Flow.Topology as FlowTopo
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
--import qualified EFA.Data.OrdData as OrdData
import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Flow.Topology.Quantity as TopoQty

-- import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Action.Optimisation.Cube.Solve as CubeSolve

import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative

--import qualified EFA.Flow.Topology as FlowTopo

-- TODO: Modul so verallgemeinern, dass mit verschiedenen Datentypen gesweept werden kann
modul :: ModuleName
modul = ModuleName "DoubleSweep"

nc :: FunctionName -> Caller
nc = genCaller modul

newtype Variation node inst demDim srchDim demVec srchVec a b = 
  Variation 
  (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
   (Collection.Collection (DemandAndControl.Var node) (CubeMap.Cube (Sweep.Search inst) 
                                 srchDim (DemandAndControl.Var node) srchVec a b)))

newtype FlowResult node inst demDim srchDim demVec srchVec a b = 
  FlowResult
  (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
   (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) 
                                         srchDim srchVec b))))

newtype FlowStatus node inst demDim srchDim demVec srchVec a = 
  FlowStatus 
   (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
    (Result.Result (CubeMap.Data (Sweep.Search inst) 
                    srchDim srchVec ActFlowCheck.EdgeFlowStatus)))

   
newtype EndNodeFlows node inst demDim srchDim demVec srchVec a b =  
  EndNodeFlows
  (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
  (FlowTopoOpt.EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) 
                                                    srchDim srchVec b))))  
newtype OptimalityMeasure node inst demDim srchDim demVec srchVec a b = 
  OptimalityMeasure
 (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
  (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec 
                  (ActFlowCheck.EdgeFlowStatus, (FlowOpt.OptimalityMeasure  b)))))
 
newtype ObjectiveFunctionValues node inst demDim srchDim demVec srchVec a b = 
  ObjectiveFunctionValues 
  (CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a 
   (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec 
   (ActFlowCheck.EdgeFlowStatus, (FlowOpt.OptimalityValues  b)))))

newtype OptimalChoicePerState node inst dim vec a b = 
  OptimalChoicePerState                                                              
  (CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a 
   (ValueState.Map (CubeGrid.LinIdx,
                    (ActFlowCheck.EdgeFlowStatus,
                     FlowOpt.OptimalityValues  b))))

newtype OptimalFlowPerState node inst dim vec a b = 
  OptimalFlowPerState
  (CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a 
   (ValueState.Map (TopoQty.Section node b)))


generateVariation :: 
  (DV.Walker vec,Ord node,
   DV.Storage vec (ND.Data dim a),
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (vec [a]),
   DV.Singleton vec,
   DV.Storage vec (Collection.Collection (DemandAndControl.Var node) 
                   (CubeMap.Cube (Sweep.Search inst) dim1 
                    (DemandAndControl.Var node) vec1 a (Interp.Val a))),
   Eq (vec1 a), 
   DV.Walker vec1,
   DV.Storage vec1 a,
   DV.Storage vec1 (ND.Data dim1 a),
   DV.Storage vec1 [a],
   DV.Storage vec1 (vec1 [a]),
   DV.Singleton vec1,
   DV.FromList vec1,
   DV.Storage vec1 (Interp.Val a),
   DV.FromList vec) =>
  Caller ->
  (CubeGrid.Grid (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a) -> 
  (CubeGrid.Grid (Sweep.Search inst) dim1 (DemandAndControl.Var node) vec1 a) -> 
  Variation node inst dim dim1 vec vec1 a (Interp.Val a)
-- Todo :: check for similar variables
generateVariation caller demandGrid searchGrid = Variation result
  where 
--    err = merror (caller |> nc "generateWithGrid")  modul "generateWithGrid"
--          "Demand and search grid must not have the same Variables"
    result = CubeMap.generateWithGrid (makeCollection) demandGrid
    makeCollection demandCoord = Collection.fromList  (nc "generateVariation") $ 
                                 (ND.toList demandCubes) ++ (ND.toList searchCubes)
       where
        demandCubes = ND.imap (\dimIdx axis -> (Strict.getLabel axis, 
                                                CubeMap.map Interp.Inter $
                            CubeMap.generateWithGrid (const $ ND.unsafeLookup 
                                                      demandCoord dimIdx) searchGrid)) 
                      demandGrid 
        
        searchCubes = ND.imap (\dimIdx axis -> (Strict.getLabel axis, 
                                                CubeMap.map Interp.Inter $
                            CubeMap.generateWithGrid (flip ND.unsafeLookup dimIdx) 
                            searchGrid)) searchGrid

solve:: 
  (DV.Walker vec,
   DV.Storage vec (Collection.Collection (DemandAndControl.Var node) 
                   (CubeMap.Cube (Sweep.Search inst) dim1 
                    (DemandAndControl.Var node) vec1 a (Interp.Val a))),
   Eq a,
   Arith.Constant a,
   Node.C node,
   DV.Zipper vec1,
   DV.Walker vec1,
   DV.Storage vec1 Bool,
   DV.Storage vec1 (Interp.Val a),
   DV.Storage vec1 a,
   DV.Singleton vec1,
   DV.Length vec1,
   DV.Storage vec (TopoQty.Section node 
                   (Result.Result (CubeMap.Data 
                                   (Sweep.Search inst) dim1 vec1 (Interp.Val a))))) =>
  Topo.Topology node -> 
  EtaFunctions.FunctionMap node a ->  
  Variation node inst dim dim1 vec vec1 a (Interp.Val a) ->
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a)
solve topology etaFunctions (Variation varCube) = 
  FlowResult $ CubeMap.map (CubeSolve.solve topology etaFunctions) varCube



getEndNodeFlows ::
  (Node.C node,
   DV.Walker vec,
   DV.Storage vec (TopoQty.Section node 
                   (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 b))),
   DV.Storage vec (FlowTopoOpt.EndNodeEnergies node 
                   (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 b)))) =>
  FlowResult node inst dim dim1 vec vec1 a b ->
  EndNodeFlows node inst dim dim1 vec vec1 a b
getEndNodeFlows (FlowResult result) = EndNodeFlows $ 
                                      CubeMap.map FlowTopoOpt.getEndNodeFlows result 

getFlowStatus ::
  (Ord b,
  Ord node,
  Arith.Constant b,
  DV.Zipper vec1,
  DV.Walker vec1,
  DV.Walker vec,
  DV.Storage vec1 ActFlowCheck.Validity,
  DV.Storage vec1 (Interp.Val b),
  DV.Storage vec1 (Maybe Idx.AbsoluteState),
  DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
  DV.Storage vec (TopoQty.Section node 
                  (Maybe (TopoQty.Flow 
                          (Result.Result (CubeMap.Data 
                                          (Sweep.Search inst) dim1 vec1 (Interp.Val b)))))),
  DV.Storage vec (Result.Result 
                  (CubeMap.Data (Sweep.Search inst) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
  DV.Storage vec (TopoQty.Section node (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val b))))) =>
  Caller ->
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val b) ->
  FlowStatus node inst dim dim1 vec vec1 a
getFlowStatus caller (FlowResult result) = 
  FlowStatus $ CubeMap.map (FlowTopoCheck.getFlowStatus 
                            (caller |> nc "getEndNodeFlows")) result

calculateOptimalityMeasure ::
  (
   Eq (vec a),
   Ord b,
   Ord node,
   Show node,
   Arith.Constant b,
   DV.Zipper vec1,
   DV.Zipper vec,
   DV.Walker vec1,
   DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,(b,b)), 
   DV.Storage vec1 (b,b),
   DV.Storage vec1 b,
   DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec1 (FlowOpt.Eta2Optimise b),
   DV.Storage  vec (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 
                                   (ActFlowCheck.EdgeFlowStatus, 
                                    FlowOpt.OptimalityMeasure b))),
   DV.Storage vec1 (FlowOpt.TotalBalanceForce b),
   DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure b),
   DV.Storage vec1 (FlowOpt.OptimalityMeasure b),
   DV.Storage vec1 (FlowOpt.Loss2Optimise b),
   DV.Storage vec1 (FlowOpt.Eta2Optimise b,
                    FlowOpt.Loss2Optimise b),
   DV.Storage vec (FlowTopoOpt.EndNodeEnergies node (
                      Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 b))),
   DV.Storage vec (Result.Result 
                   (CubeMap.Data (Sweep.Search inst) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
   DV.Storage vec (Result.Result 
                   (CubeMap.Data (Sweep.Search inst) dim1 vec1 
                    (ActFlowCheck.EdgeFlowStatus,
                     (FlowOpt.TotalBalanceForce b,
                      (FlowOpt.Eta2Optimise b,
                       FlowOpt.Loss2Optimise b))))),
   DV.Singleton vec1,
   DV.Length vec1) =>
  Caller ->
  FlowOpt.LifeCycleMap node b ->
  EndNodeFlows node inst dim dim1 vec vec1 a b ->
  FlowStatus node inst dim dim1 vec vec1 a ->
  OptimalityMeasure node inst dim dim1 vec vec1 a b
calculateOptimalityMeasure caller lifeCycleMap (EndNodeFlows endNodeValues) (FlowStatus status) = 
  OptimalityMeasure $
  CubeMap.zipWith (caller |> nc "calculateOptimalityMeasuregetEndNodeFlows") 
  (\ x st -> FlowTopoOpt.calculateOptimalityMeasure 
             (caller |> nc "calculateOptimalityMeasure") 
             st lifeCycleMap x) 
  endNodeValues status
  
objectiveFunctionValues ::
  (
  Eq (vec a),
  Ord b,
  Ord node,
  Show node,
  Arith.Constant b,
  DV.Zipper vec1,
  DV.Zipper vec,
  DV.Walker vec1,
  DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,(b,b)), 
  DV.Storage vec1 (b,b),
  DV.Storage vec1 b,
  DV.Storage vec1 ActFlowCheck.EdgeFlowStatus,
  DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure b),
  DV.Storage vec1 (FlowOpt.OptimalityMeasure b),
  DV.Storage vec1 (FlowOpt.Eta2Optimise b),
  DV.Storage vec (Result.Result (CubeMap.Data 
                                 (Sweep.Search inst) dim1 vec1 
                                 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure b))),
  DV.Storage  vec (Result.Result 
                   (CubeMap.Data (Sweep.Search inst) dim1 vec1 
                    (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues b))),
  DV.Storage vec1 (FlowOpt.TotalBalanceForce b),
  DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues b),
  DV.Storage vec1 (FlowOpt.OptimalityValues b),
  DV.Storage vec1 (FlowOpt.Loss2Optimise b),
  DV.Storage vec1 (FlowOpt.Eta2Optimise b,
  FlowOpt.Loss2Optimise b),
  DV.Storage vec (FlowTopoOpt.EndNodeEnergies node 
                  (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 b))),
  DV.Storage vec (Result.Result (CubeMap.Data 
                                 (Sweep.Search inst) dim1 vec1 ActFlowCheck.EdgeFlowStatus)),
  DV.Storage vec (Result.Result (CubeMap.Data 
                                 (Sweep.Search inst) dim1 vec1 (ActFlowCheck.EdgeFlowStatus,
 (FlowOpt.TotalBalanceForce b,
  (FlowOpt.Eta2Optimise b,
  FlowOpt.Loss2Optimise b))))),
  DV.Singleton vec1,
  DV.Length vec1) =>
  Caller ->
  FlowBal.Forcing node b ->
  EndNodeFlows node inst dim dim1 vec vec1 a b ->
  OptimalityMeasure node inst dim dim1 vec vec1 a b ->
  ObjectiveFunctionValues node inst dim dim1 vec vec1 a b 
objectiveFunctionValues caller balanceForcingMap (EndNodeFlows endNodeValues) (OptimalityMeasure optimalityMeasure) = 
  ObjectiveFunctionValues $
  CubeMap.zipWith (caller |> nc "objectiveFunctionValues") 
  (\ endNodeFlows optiMeasure -> FlowTopoOpt.objectiveFunctionValues 
                                 (caller |> nc "objectiveFunctionValues") 
             balanceForcingMap endNodeFlows optiMeasure) 
  endNodeValues optimalityMeasure
      
findMaximumEtaPerState ::
 (Ord  b,
 Arith.Constant  b,
 DV.Walker vec1,
 DV.Walker vec,
 DV.Storage vec1 (CubeGrid.LinIdx,
 (ActFlowCheck.EdgeFlowStatus,
 (Interp.Val  b,
 Interp.Val  b))),
 DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 
                                (ActFlowCheck.EdgeFlowStatus,
                                 FlowOpt.OptimalityValues (Interp.Val b)))),
 DV.Storage vec1 (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus, 
                                    FlowOpt.OptimalityValues (Interp.Val b))), 
 DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val b)), 
 DV.Storage vec1 (ActFlowCheck.EdgeFlowStatus, (Interp.Val  b, Interp.Val  b)),
 DV.Storage vec (ValueState.Map (CubeGrid.LinIdx, 
                                 (ActFlowCheck.EdgeFlowStatus, 
                                  FlowOpt.OptimalityValues (Interp.Val b))))) =>
 Caller ->
 ObjectiveFunctionValues node inst dim dim1 vec vec1 a (Interp.Val  b) ->
 OptimalChoicePerState node inst dim vec a (Interp.Val b)
findMaximumEtaPerState caller (ObjectiveFunctionValues objFunctionValues) = 
  OptimalChoicePerState $
  CubeMap.map (FlowTopoOpt.findMaximumEta 
               (caller |> nc "findMaximumEtaPerState")) objFunctionValues



getOptimalFlowPerStateCube :: 
  (DV.LookupUnsafe vec1 (Interp.Val b),Eq node,
   Eq (vec a),
   DV.Zipper vec,
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val b)))),
   DV.Storage vec (TopoQty.Section node 
                   (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val b)))),
   DV.Storage vec (ValueState.Map (TopoQty.Section node (Result.Result (Interp.Val b))))) =>
   Caller ->
   OptimalChoicePerState node inst dim vec a (Interp.Val  b) ->
   FlowResult node inst dim dim1 vec vec1 a (Interp.Val  b) ->
  OptimalFlowPerState node inst dim vec a (Result.Result (Interp.Val b))
getOptimalFlowPerStateCube caller (OptimalChoicePerState optimalityCube) (FlowResult sweepCube) = 
  OptimalFlowPerState $
  CubeMap.zipWith (caller |> nc "getOptimalValueCube") f  optimalityCube  sweepCube
  where f stateMap flowSection = 
          ValueState.map (\(linIdx,_) -> 
                           TopoQty.mapSection (\searchCubeData ->  
                                                fmap (flip CubeMap.lookupLinUnsafeData linIdx) 
                                                searchCubeData ) flowSection) stateMap

--------------------- Helper Functions below

unresultOptimalFlowPerStateCube :: 
  (DV.Walker vec,
   DV.Storage vec (ValueState.Map (TopoQty.Section node (Result.Result (Interp.Val b)))),
   DV.Storage vec (ValueState.Map (TopoQty.Section node (Interp.Val b)))) =>
  Caller ->
  OptimalFlowPerState node inst dim vec a (Result.Result (Interp.Val b)) ->
  OptimalFlowPerState node inst dim vec a (Interp.Val b)
unresultOptimalFlowPerStateCube caller (OptimalFlowPerState cube) = 
  OptimalFlowPerState $ CubeMap.map (ValueState.map (TopoQty.mapSection f)) cube 
  where f (Result.Determined x) = x 
        f Result.Undetermined = 
          merror caller modul "unresultOptimalFlowPerStateCube" "Undetermined Values in Sweep"
  

-- TODO Move to better place
lookupControlVar :: (Ord node) =>
  TopoQty.Section node b ->  
  DemandAndControl.ControlVar node -> 
  Maybe b
lookupControlVar flowSection (DemandAndControl.ControlPower idx) = 
  TopoQty.lookupPower idx flowSection
lookupControlVar flowSection (DemandAndControl.ControlRatio idx) = 
  TopoQty.lookupX idx flowSection

lookupControlVariablesPerState ::(Ord node)=> 
  Caller ->
  ValueState.Map (TopoQty.Section node b) ->
  [DemandAndControl.ControlVar node] ->
  ValueState.Map (Map.Map (DemandAndControl.ControlVar node) b)
lookupControlVariablesPerState  caller flowSectionMap controlVars = 
  ValueState.map g flowSectionMap
  where g flowSection = Map.fromList $ zip controlVars $ 
                        map (Maybe.fromMaybe err . lookupControlVar flowSection) controlVars
        err = merror caller modul "lookupControlVariables" "ControlVariable not found in flowSection"

lookupControlVariablePerState ::(Ord node)=> 
  Caller ->
  ValueState.Map (TopoQty.Section node b) ->
  DemandAndControl.ControlVar node ->
  ValueState.Map  b
lookupControlVariablePerState  caller flowSectionMap controlVar = 
  ValueState.map g flowSectionMap
  where g flowSection = (Maybe.fromMaybe err . lookupControlVar flowSection) controlVar
        err = merror caller modul "lookupControlVariables" "ControlVariable not found in flowSection"

interpolateWithSupportPerState :: 
  (Ord a, Show a, Arith.Constant a,Show label,
   DV.Storage vec a,
   DV.Storage vec (ValueState.Map (Interp.Val a)),
   DV.Slice vec,
   DV.Length vec, 
   Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (vec (ValueState.Map (Interp.Val a))),
   DV.LookupMaybe vec (ValueState.Map (Interp.Val a)),
   DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))) =>
  Caller -> 
  Interp.Method a ->
  CubeMap.Cube inst dim label vec a (ValueState.Map (Interp.Val  a)) ->
  ND.Data dim (Strict.SupportingPoints (Strict.Idx,a)) ->
  (ND.Data dim a) ->
  ValueState.Map (Interp.Val a)
interpolateWithSupportPerState caller inmethod cube support coordinates = 
  g (ND.getFirst newCaller support) 
  where 
    newCaller = (caller |> (nc "interpolateWithSupportPerState"))
    label = show $ Strict.getLabel $ ND.getFirst newCaller $ CubeMap.getGrid cube
    f idx = if ND.len coordinates >=2 
            then
               interpolateWithSupportPerState newCaller inmethod (CubeMap.getSubCube newCaller cube idx) 
               (ND.dropFirst (caller |> (nc "interpolateOptimalityValuesWithSupportPerState-support")) support) 
               (ND.dropFirst (caller |> (nc "interpolateOptimalityValuesWithSupportPerState-coordinates")) coordinates)
            else CubeMap.lookUp newCaller (ND.Data [idx]) cube   
    g (Strict.LeftPoint (idx,_)) = f idx 
    g (Strict.RightPoint (idx,_)) = f idx
    g (Strict.PairOfPoints (idx1,x1) (idx2,x2)) = 
      Interp.dim1PerState caller inmethod label (x1,x2) (y1,y2) 
                                             $ ND.getFirst newCaller coordinates 
      where    
        (y1,y2) = (f idx1, f idx2)

interpolateWithSupportPerStateMaybe :: 
  (Ord a, Show a, Arith.Constant a,Show label,
   DV.Storage vec a,
   DV.Storage vec (ValueState.Map (Maybe (Interp.Val a))),
   DV.Slice vec,
   DV.Length vec, 
   Show (vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))),
   Show (vec (ValueState.Map (Maybe(Interp.Val a)))),
   DV.LookupMaybe vec (ValueState.Map (Maybe(Interp.Val a))),
   DV.LookupMaybe vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a)))) =>
  Caller -> 
  Interp.Method a ->
  CubeMap.Cube inst dim label vec a (ValueState.Map (Maybe(Interp.Val  a))) ->
  ND.Data dim (Strict.SupportingPoints (Strict.Idx,a)) ->
  (ND.Data dim a) ->
  ValueState.Map (Maybe(Interp.Val a))
interpolateWithSupportPerStateMaybe caller inmethod cube support coordinates = 
  g (ND.getFirst newCaller support) 
  where 
    newCaller = (caller |> (nc "interpolateWithSupportPerState"))
    label = show $ Strict.getLabel $ ND.getFirst newCaller $ CubeMap.getGrid cube
    f idx = if ND.len coordinates >=2 
            then
               interpolateWithSupportPerStateMaybe newCaller inmethod 
               (CubeMap.getSubCube newCaller cube idx) 
               (ND.dropFirst (caller |> 
                              (nc "interpolateOptimalityValuesWithSupportPerState-support")) support) 
               (ND.dropFirst (caller |> 
                              (nc "interpolateOptimalityValuesWithSupportPerState-coordinates")) coordinates)
            else CubeMap.lookUp newCaller (ND.Data [idx]) cube   
    g (Strict.LeftPoint (idx,_)) = f idx 
    g (Strict.RightPoint (idx,_)) = f idx
    g (Strict.PairOfPoints (idx1,x1) (idx2,x2)) = 
      Interp.dim1PerStateWithMaybe caller inmethod label (x1,x2) (y1,y2) 
                                             $ ND.getFirst newCaller coordinates 
      where    
        (y1,y2) = (f idx1, f idx2)


interpolateOptimalityValuesWithSupportPerState :: 
  (Ord a, Show a, Arith.Constant a,DV.Storage vec a, Show node,
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Slice vec,
   Show(vec (ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a))))),
   DV.LookupMaybe vec (ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Length vec) =>
  Caller -> 
  Interp.Method a ->
  OptimalChoicePerState node inst dim vec a (Interp.Val a) ->
  ND.Data dim (Strict.SupportingPoints (Strict.Idx,a)) ->
  (ND.Data dim a) ->
  ValueState.Map (FlowOpt.OptimalityValues(Interp.Val a))
interpolateOptimalityValuesWithSupportPerState caller inmethod (OptimalChoicePerState cube) support coordinates = 
  g (ND.getFirst newCaller support) 
  where 
    faccess = snd . snd
    newCaller = (caller |> (nc "interpolateOptimalityValuesWithSupportPerState"))
    label = show $ Strict.getLabel $ ND.getFirst newCaller $ CubeMap.getGrid cube
    f idx = if ND.len coordinates >=2 
            then
               interpolateOptimalityValuesWithSupportPerState newCaller 
               inmethod (OptimalChoicePerState $ CubeMap.getSubCube newCaller cube idx) 
               (ND.dropFirst (caller |> 
                              (nc "interpolateOptimalityValuesWithSupportPerState-support")) 
                support) 
               (ND.dropFirst (caller |> 
                              (nc "interpolateOptimalityValuesWithSupportPerState-coordinates")) 
                coordinates)
               
            else ValueState.map faccess $ CubeMap.lookUp newCaller (ND.Data [idx]) cube   
    g (Strict.LeftPoint (idx,_)) = f idx 
    g (Strict.RightPoint (idx,_)) = f idx
    g (Strict.PairOfPoints (idx1,x1) (idx2,x2)) = 
      combine3PerStateOptimality 
      (FlowOpt.interpolateOptimalityPerState caller inmethod label (x1,x2) (y1,y2) 
                                             $ ND.getFirst newCaller coordinates) y1 y2 
      where    
        (y1,y2) = (f idx1, f idx2)

  
  
  
                       
combine3PerStateOptimality :: 
  ValueState.Map (FlowOpt.OptimalityValues(Interp.Val a)) -> 
  ValueState.Map (FlowOpt.OptimalityValues(Interp.Val a)) -> 
  ValueState.Map (FlowOpt.OptimalityValues(Interp.Val a)) -> 
  ValueState.Map (FlowOpt.OptimalityValues(Interp.Val a))
combine3PerStateOptimality m m1 m2 =  ValueState.zipWith3 f  m m1 m2
  where f (FlowOpt.OptimalityValues 
           (FlowOpt.OptimalityMeasure (FlowOpt.EtaSys e) 
            (FlowOpt.LossSys l)) (FlowOpt.TotalBalanceForce fo)) 
          (FlowOpt.OptimalityValues 
           (FlowOpt.OptimalityMeasure(FlowOpt.EtaSys e1)
            (FlowOpt.LossSys l1)) (FlowOpt.TotalBalanceForce fo1))
          (FlowOpt.OptimalityValues 
           (FlowOpt.OptimalityMeasure(FlowOpt.EtaSys e2)
            (FlowOpt.LossSys l2)) (FlowOpt.TotalBalanceForce fo2))
          
          = (FlowOpt.OptimalityValues 
             (FlowOpt.OptimalityMeasure(FlowOpt.EtaSys $  g e e1 e2)
              (FlowOpt.LossSys $ g l l1 l2)) 
             (FlowOpt.TotalBalanceForce $ g fo fo1 fo2 ))  
        g = Interp.combine3   

{-        
       
haveNoCommonAxes :: (Eq label) =>
  CubeGrid.Grid (Sweep.Demand inst) dim (ControlAndDemand.DemandVar node) vec a  -> 
  CubeGrid.Grid (Sweep.Search inst) dim1 (ControlAndDemand.ControlVar node) vec1 a1  ->
  Bool
haveNoCommonAxes demandGrid searchGrid = (List.intersect (f demandGrid) (g searchGrid) == []) 
  where 
    f gr = ND.toList $ ND.map Strict.getLabel gr
    g (ControlAndDemand.DemandPower 
    h 
--    f gr = ND.toList $ ND.map (ControlAndDemand.unControlVar . Strict.getLabel) gr
    
-}
        




----------------- Acess Functions below


-- | Delivers the energy flow sweep at one location in the demand room 
getSearchSweepFlow::
  (DV.Storage vec a,
   DV.Length vec, 
   DV.LookupMaybe vec (TopoQty.Section node 
                       (Result.Result (CubeMap.Data 
                                       (Sweep.Search inst) dim1 vec1 (Interp.Val a)))))=>
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a) ->
  CubeGrid.DimIdx dim ->
  Maybe (TopoQty.Section node (Result.Result 
                               (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a))))  
getSearchSweepFlow (FlowResult result) ndIdx = CubeMap.lookupMaybe ndIdx result

-- | Delivers the energy flow at one location in the demand room and one location in the search room
getSearchSweepPowers :: 
  (Ord node, 
   DV.Storage vec a,
   DV.LookupMaybe vec (TopoQty.Section node 
                       (Result.Result (CubeMap.Data 
                                       (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Length vec) =>
  CubeGrid.Grid (Sweep.Search inst) dim1 (TopoIdx.Position node) vec1 a ->
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a) -> 
  CubeGrid.DimIdx dim -> 
  Maybe (Collection.Collection (TopoIdx.Position node) 
         (Result.Result (CubeMap.Cube (Sweep.Search inst) dim1 
                         (TopoIdx.Position node) vec1 a (Interp.Val a))))
getSearchSweepPowers sweepGrid result ndIdx = f $ getSearchSweepFlow result ndIdx 
  where f (Just flow) = Just $ Collection.Collection 
                        (Result.Determined sweepGrid) 
                        (TopoRecord.sectionResultToPowerMap flow)
        f Nothing = Nothing


-- | Delivers the energy flow at one location in the demand room and one location in the search room
getFlowAtSingleSweepPoint::
  (DV.Storage vec a,
   DV.LookupMaybe vec (TopoQty.Section node 
                       (Result.Result (CubeMap.Data 
                                       (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Length vec, 
   DV.Storage vec1 a,
   DV.LookupMaybe vec1 (Interp.Val a),
   DV.Length vec1) =>
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a) ->
  CubeGrid.Grid (Sweep.Search inst) dim1 label vec1 a ->
  CubeGrid.DimIdx dim ->
  CubeGrid.DimIdx dim1 ->
  Maybe (TopoQty.Section node (Result.Result  (Maybe (Interp.Val a))))  
getFlowAtSingleSweepPoint (FlowResult result) sweepGrid demandIdx searchIdx = 
  f $ CubeMap.lookupMaybe demandIdx result
  where f (Just sweepFlow) = Just $ TopoQty.mapSection g sweepFlow
        f Nothing = Nothing  
        g (Result.Determined cubeData) = Result.Determined $ 
                                         CubeMap.lookupMaybe searchIdx 
                                         (CubeMap.Cube sweepGrid cubeData)
        g Result.Undetermined = Result.Undetermined


getDemandSweepFlow ::
  (DV.Walker vec,
  DV.Storage vec (TopoQty.Section node v0),
  DV.Storage vec (TopoQty.Section node v1)) =>
  (v0 -> v1) ->
  CubeMap.Cube inst dim label vec a (TopoQty.Section node v0) ->
  CubeMap.Cube inst dim label vec a (TopoQty.Section node v1)
getDemandSweepFlow f result = CubeMap.map (TopoQty.mapSection f) result  

{-  
getDemandSweepPower:: 
  (Eq a,Ord node,
   DV.Storage vec (TopoQty.Section node 
                   (Result.Result (CubeMap.Data 
                                   (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Storage vec (Maybe (Result.Result (Interp.Val a))),
   Arith.Constant a,
   DV.Storage vec (Interp.Val a), 
   DV.Walker vec,
   DV.Storage vec (TopoQty.Section node (Result.Result (Interp.Val a))),
   DV.Storage vec (Map.Map (TopoIdx.Position node) (Result.Result (Interp.Val a))),
   DV.Singleton vec)=>
  (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)) -> 
   Result.Result (Interp.Val a)) -> 
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a) ->
  TopoIdx.Position node  -> 
  Maybe (Result.Result (CubeMap.Cube (Sweep.Demand inst) dim 
                        (TopoIdx.Position node) vec a (Interp.Val a)))
  
getDemandSweepPower f (FlowResult result) key = g cube
  where
    g cu = case (CubeMap.any (== Nothing) cu, 
                 CubeMap.any (== Just (Result.Undetermined)) cu) of
      (True,_) -> Nothing
      (False,True) -> Just (Result.Undetermined)
      (False,False) -> Just $ Result.Determined $ 
                       CubeMap.map (\(Just (Result.Determined x)) -> x) cu          
    cube = CubeMap.map (Map.lookup key) powerMap      
    powerMap = CubeMap.map TopoRecord.sectionResultToPowerMap $ getDemandSweepFlow f result                       

getDemandSweepPowers::
  (Eq a,Ord node,
   DV.Storage vec (TopoQty.Section node 
                   (Result.Result (CubeMap.Data 
                                   (Sweep.Search inst) dim1 vec1 (Interp.Val a)))), 
        Arith.Constant a,
   DV.Walker vec, 
   DV.LookupUnsafe vec (TopoQty.Section node 
                        (Result.Result (CubeMap.Data 
                                        (Sweep.Search inst) dim1 vec1 (Interp.Val a)))),
   DV.Storage
   vec
   (Map.Map (TopoIdx.Position node) (Result.Result (Interp.Val a))),
   DV.Storage
   vec (TopoQty.Section node (Result.Result (Interp.Val a))),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec (Maybe (Result.Result (Interp.Val a))),
   DV.Singleton vec)=>
  (Result.Result (CubeMap.Data (Sweep.Search inst) dim1 vec1 (Interp.Val a)) -> 
   Result.Result (Interp.Val a)) -> 
  FlowResult node inst dim dim1 vec vec1 a (Interp.Val a) ->
  Collection.Collection (TopoIdx.Position node) 
  (Result.Result (CubeMap.Cube (Sweep.Demand inst) dim 
                  (TopoIdx.Position node) vec a (Interp.Val a)))
getDemandSweepPowers f (FlowResult result) = Collection.Collection (Result.Determined (CubeMap.getGrid result)) $ 
                                Map.mapWithKey (\ key _ -> fmap CubeMap.getData $ Maybe.fromJust $ getDemandSweepPower f (FlowResult result) key) powerMap
  where
    powerMap = TopoRecord.sectionResultToPowerMap $ CubeMap.lookupLinUnsafe result (CubeGrid.LinIdx 0)

-}