{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Action.Optimisation.Cube.Sweep.Access where
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

collectionCubeToCubeCollection ::
  (DV.LookupUnsafe demVec (Collection.Collection key b), 
   DV.Walker demVec,
   DV.Storage demVec (Collection.Collection key b),
   DV.Storage demVec b, 
   Ord key, Show key, Collection.Unpack b) =>
  CubeMap.Cube inst demDim label demVec a (Collection.Collection key b) ->
  Collection.Collection key (CubeMap.Cube inst demDim label demVec a b)
collectionCubeToCubeCollection cube = Collection.Collection grid (Map.mapWithKey f m)
  where
  (Collection.Collection _ m) = CubeMap.lookupLinUnsafe cube (CubeGrid.LinIdx 0)
  f k _ = CubeMap.getData $ CubeMap.map (Collection.lookupUnsafe k) cube
  grid = CubeMap.getGrid cube
        

stateCubeToStateCubes ::
  (DV.Storage vec b,DV.Walker vec, DV.Storage vec (ValueState.Map b),DV.Storage vec (Maybe b)) =>
  CubeMap.Cube node inst dim vec a (ValueState.Map b) ->
  ValueState.Map (CubeMap.Cube node inst dim vec a (Maybe b))
stateCubeToStateCubes cube = ValueState.Map $ Map.fromList $ zip states $ map f states
  where 
    f st = CubeMap.map (flip ValueState.lookUp st) cube
    states = getAllStates cube
    
  
  
getAllStates :: 
  (DV.Walker vec, DV.Storage vec (ValueState.Map b)) => 
  CubeMap.Cube node inst dim vec a (ValueState.Map b) -> [Maybe (Idx.AbsoluteState)]
getAllStates cube = ValueState.states $ DV.foldl (ValueState.union) (ValueState.Map Map.empty) vec 
  where vec = CubeMap.getVector $ CubeMap.getData cube

{-
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
-}

{-
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
-}