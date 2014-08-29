{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology.LifeCycle where

--import qualified EFA.Action.DemandAndControl as DemandAndControl

--import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
--import qualified EFA.Action.Flow.Balance as ActBal

--import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Flow.Topology as FlowTopo
--import EFA.Utility.Trace(mytrace)

import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Data.Vector as DV

--import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic ((~+), (~/))
--import EFA.Equation.Result (Result(..))
--import qualified EFA.Equation.Result as Result

import qualified Data.Map as Map
--import qualified Data.Set as Set

--import qualified EFA.Graph as Graph

--import qualified Data.Map as Map

--import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
--import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Report.Format as Format

import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2, liftA)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

--import Debug.Trace(trace)

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul

calcEtaLossSys ::
  (Ord a,
   Ord node,
   Show node,
   Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (FlowOpt.Eta2Optimise (Interp.Val a),
                   FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage vec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage vec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Singleton vec,
   DV.Length vec) =>
  Caller ->
  FlowOpt.LifeCycleMap node (Interp.Val a) ->
  FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) dim vec (Interp.Val a)) ->
  CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus ->  
  CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityMeasure (Interp.Val a))

calcEtaLossSys caller lifeCycleEfficiencies  
  (FlowOpt.EndNodeEnergies (FlowOpt.SinkMap sinks) (FlowOpt.SourceMap sources) (FlowOpt.StorageMap  storages)) state = let 
    
  -- CHECK :: is mapMaybe OK here ?   
  chargeStorages = Map.map FlowOpt.unStorageFlow $ Map.mapMaybeWithKey (\node x -> 
          (applyUsageEfficiency caller state lifeCycleEfficiencies node) x) storages
                   
  dischargeStorages =Map.map FlowOpt.unStorageFlow $ Map.mapMaybeWithKey (\node x ->
          (applyGenerationEfficiency caller state lifeCycleEfficiencies node) x)  storages
  
  makeSum = foldl1 (Arith.~+) . Map.elems
  sinkTerm =  (makeSum sinks)  Arith.~+ (makeSum chargeStorages)
  sourceTerm = (makeSum sources) Arith.~+ (makeSum dischargeStorages)
  eta = (CubeMap.mapData FlowOpt.EtaSys) $ sinkTerm Arith.~/ sourceTerm
  loss = (CubeMap.mapData FlowOpt.LossSys) $ sourceTerm Arith.~- sinkTerm
  in CubeMap.zipWithData ((,)) state $ CubeMap.zipWithData FlowOpt.OptimalityMeasure eta loss


applyGenerationEfficiency ::
  (Ord a,
   Ord node,
   Show node,
   Arith.Constant a,
   DV.Zipper vec,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Interp.Val a)) =>
  Caller ->
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus ->
  FlowOpt.LifeCycleMap node (Interp.Val a) ->
  node ->
  Maybe (FlowOpt.StorageFlow (CubeMap.Data inst dim vec (Interp.Val a))) ->
  Maybe (FlowOpt.StorageFlow (CubeMap.Data inst dim vec (Interp.Val a)))
applyGenerationEfficiency _ _ _ _ Nothing  = Nothing
applyGenerationEfficiency caller state lifeCycleEfficiencies node (Just (FlowOpt.StorageFlow storageFlow)) = 
  Just $ FlowOpt.StorageFlow $ CubeMap.zipWithData f storageFlow state
  where 
    -- x < 0 means storage is discharged -> apply lifeCycle-Generation Efficiency
    f x st = if x < (Interp.Inter $ Arith.zero) then Arith.negate $ x Arith.~/ etaGen else Interp.Inter $ Arith.zero
      where
        newCaller = caller |> nc "applyGenerationEfficiency"
        etaGen = FlowOpt.lookupGenerationEfficiency newCaller lifeCycleEfficiencies (ActFlowCheck.getState st) node 

                    
applyUsageEfficiency ::
  (Ord a,
   Ord node,
   Show node,
   Arith.Constant a,
   DV.Zipper vec,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Interp.Val a)) =>
  Caller ->
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus ->
  FlowOpt.LifeCycleMap node (Interp.Val a) ->
  node ->
  Maybe (FlowOpt.StorageFlow (CubeMap.Data inst dim vec (Interp.Val a))) ->
  Maybe (FlowOpt.StorageFlow (CubeMap.Data inst dim vec (Interp.Val a)))
applyUsageEfficiency caller state lifeCycleEfficiencies node (Just (FlowOpt.StorageFlow storageFlow)) = 
  Just $ FlowOpt.StorageFlow $ CubeMap.zipWithData f storageFlow state  
  where
    -- x > 0 means storage is charged -> apply lifeCycle-Usage Efficiency
    f x st = if x > Arith.zero then x Arith.~* etaUse else Arith.zero
      where 
        newCaller = caller |> nc "applyUsageEfficiency"
        etaUse = FlowOpt.lookupUsageEfficiency newCaller lifeCycleEfficiencies (ActFlowCheck.getState st) node 

