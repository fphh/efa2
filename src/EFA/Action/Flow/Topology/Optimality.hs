{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology.Optimality where


import qualified EFA.Action.DemandAndControl as DemandAndControl

import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Balance as ActBal
import qualified EFA.Action.Utility as ActUt

import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Flow.Topology as FlowTopo
--import EFA.Utility.Trace(mytrace)

import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Data.Vector as DV

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic ((~+), (~/))
--import EFA.Equation.Result (Result(..))
--import qualified EFA.Equation.Result as Result

import qualified Data.Map as Map
--import qualified Data.Set as Set

import qualified EFA.Graph as Graph

--import qualified Data.Map as Map

--import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
import qualified EFA.Flow.SequenceState.Index as Idx

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

getEndNodeFlows :: 
  (Node.C node,Arith.Sum v) =>
  Caller ->
  TopoQty.Section node v ->
  FlowOpt.EndNodeEnergies node v
getEndNodeFlows caller flowSection =
   let 
     newCaller = caller |> nc "getEndNodeFlows"
     
     topo = TopoQty.topology flowSection
       
     nodes = Graph.nodeLabels topo
       
     sinks = FlowOpt.SinkMap $ Map.mapMaybe TopoQty.sumIn $ 
               Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node) nodes
               
     sources = FlowOpt.SourceMap $ Map.mapMaybe TopoQty.sumOut $ 
               Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node) nodes

     storages = FlowOpt.StorageMap $ Map.mapWithKey (\node _ -> FlowOpt.getStoragePowerWithSignNew newCaller $ 
                                                                  TopoQty.lookupSums node flowSection) 
              $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
                  
    in (FlowOpt.EndNodeEnergies sinks sources storages)
       
{-
getEndNodeFlows :: 
  Node.C node =>
  TopoQty.Section node v ->
  FlowOpt.EndNodeEnergies node v
getEndNodeFlows flowSection =
   let topo = TopoQty.topology flowSection
       
       nodes = Graph.nodeLabels topo
       
       sinks = FlowOpt.SinkMap $ Map.mapMaybe TopoQty.sumIn $ 
               Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node) nodes
               
       sources = FlowOpt.SourceMap $ Map.mapMaybe TopoQty.sumOut $ 
               Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node) nodes

       storages = FlowOpt.StorageMap $ Map.mapWithKey (\node _ -> TopoQty.lookupSums node flowSection) 
              $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
                  
    in (FlowOpt.EndNodeEnergies sinks sources storages)
-}
      


lookupControlVar :: Ord node => TopoQty.Section node v -> DemandAndControl.ControlVar node -> Maybe v
lookupControlVar flowSection (DemandAndControl.ControlPower idx) = TopoQty.lookupPower idx flowSection
lookupControlVar flowSection (DemandAndControl.ControlRatio idx) = TopoQty.lookupX idx flowSection

objectiveFunctionValues :: 
  (Ord node, Ord a, Show node, Arith.Constant a, DV.Zipper vec,
   DV.Storage vec (FlowOpt.OptimalityMeasure a),
   DV.Storage vec (FlowOpt.Eta2Optimise a, FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.TotalBalanceForce a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues a),
   DV.Storage vec (FlowOpt.Eta2Optimise a),
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Walker vec, DV.Storage vec a, DV.Singleton vec,
   DV.Storage vec (a, a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (a, a)),
   DV.Length vec) =>
   Caller -> 
   ActBal.Forcing node a ->
   FlowOpt.EndNodeEnergies node ((CubeMap.Data (Sweep.Search inst) dim vec a)) ->
   (CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityMeasure a)) ->
   (CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues a))
objectiveFunctionValues caller balanceForcing ( FlowOpt.EndNodeEnergies _ _ (FlowOpt.StorageMap stos)) optimalityMeasure = let
    forcing = (CubeMap.mapData FlowOpt.TotalBalanceForce) $ 
              makeSum $ Map.mapWithKey (\node x -> applyBalanceForcing caller balanceForcing node x) stos
    makeSum = foldl1 ((Arith.~+)) . Maybe.catMaybes . Map.elems
  in (CubeMap.zipWithData (\(st,optMeas) fo -> (st,FlowOpt.OptimalityValues optMeas fo))) optimalityMeasure forcing

{-
applyBalanceForcing :: 
  (DV.Walker vec, 
   DV.Storage vec a,
   Arith.Sum a,
   Ord a, 
   Ord node, Show node,
   Arith.Constant a)=>
 Caller ->  
 ActBal.Forcing node a -> 
 node -> 
 Maybe (TopoQty.Sums ((CubeMap.Data (Sweep.Search inst) dim vec a))) ->
 Maybe ((CubeMap.Data (Sweep.Search inst) dim vec a))
applyBalanceForcing _ _ _ Nothing = Nothing
applyBalanceForcing caller balanceForcing node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ (CubeMap.mapData f) energy
  TopoQty.Sums  (Just energy) Nothing -> Just $ (CubeMap.mapData (f. Arith.negate)) energy
  TopoQty.Sums Nothing Nothing -> Nothing 
  _ -> e2  
  where
    e2 = merror caller modul "applyBalanceForcing" 
         ("Storage-Sums Variable contains values in positive and negative part-node: " ++ show node)
    f x =  x Arith.~* forcing
    forcing = ActBal.getSocDrive $ ActBal.lookupBalanceForcing (caller |> nc "applyBalanceForcing") balanceForcing node  
-}

applyBalanceForcing :: 
  (DV.Walker vec, 
   DV.Storage vec a,
   Arith.Sum a,
   Ord a, 
   Ord node, Show node,
   Arith.Constant a)=>
 Caller ->  
 ActBal.Forcing node a -> 
 node -> 
 Maybe (FlowOpt.StorageFlow (CubeMap.Data (Sweep.Search inst) dim vec a)) ->
 Maybe (CubeMap.Data (Sweep.Search inst) dim vec a)
applyBalanceForcing _ _ _ Nothing = Nothing
applyBalanceForcing caller balanceForcing node (Just (FlowOpt.StorageFlow storageFlow)) = Just $ (CubeMap.mapData f) storageFlow
  where
    f x =  x Arith.~* forcing
    forcing = ActBal.getSocDrive $ ActBal.lookupBalanceForcing (caller |> nc "applyBalanceForcing") balanceForcing node  



-- | If State is Nothing, than state could not be detected because of invalid values
findMaximumEta :: 
  (DV.Walker vec,Ord a, Arith.Constant a, Show a,
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage vec (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)),
   DV.Storage vec (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)))) =>
  Caller ->
  [Idx.AbsoluteState] ->
  CubeMap.Data (Sweep.Search inst) dim vec 
                 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)) -> 
  ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val a)))
findMaximumEta _ states cubeData = CubeMap.findBestWithIndexByPerState (ActFlowCheck.getState . fst) f states cubeData 
  where  f (_,FlowOpt.OptimalityValues (FlowOpt.OptimalityMeasure (FlowOpt.EtaSys eta) _) (FlowOpt.TotalBalanceForce forcing)) 
           (_,FlowOpt.OptimalityValues (FlowOpt.OptimalityMeasure (FlowOpt.EtaSys eta1) _) (FlowOpt.TotalBalanceForce forcing1))= 
                Interp.greaterThanWithInvalid (eta Arith.~+ forcing) (eta1 Arith.~+ forcing1) 

findMinimumLoss :: 
  (DV.Walker vec,Ord a, Arith.Constant a, Show a,
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, 
                   (FlowOpt.TotalBalanceForce (Interp.Val a),
                    (FlowOpt.Eta2Optimise (Interp.Val a),FlowOpt.Loss2Optimise (Interp.Val a)))),
   DV.Storage vec (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus, 
                                     (FlowOpt.TotalBalanceForce (Interp.Val a),
                                      (FlowOpt.Eta2Optimise (Interp.Val a), FlowOpt.Loss2Optimise (Interp.Val a))))),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)),
   DV.Storage vec (CubeGrid.LinIdx, 
                   (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)))) =>
  Caller ->
  [Idx.AbsoluteState] ->
  CubeMap.Data (Sweep.Search inst) dim vec 
                 (ActFlowCheck.EdgeFlowStatus,
                  (FlowOpt.TotalBalanceForce (Interp.Val a),
                   (FlowOpt.Eta2Optimise (Interp.Val a), FlowOpt.Loss2Optimise (Interp.Val a)))) -> 
  ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,(FlowOpt.TotalBalanceForce (Interp.Val a),
                   (FlowOpt.Eta2Optimise (Interp.Val a), FlowOpt.Loss2Optimise (Interp.Val a)))))
findMinimumLoss _ states cubeData = CubeMap.findBestWithIndexByPerState (ActFlowCheck.getState . fst) f states cubeData 
  where  f (_,(FlowOpt.TotalBalanceForce forcing,(_,FlowOpt.LossSys loss))) 
           (_,(FlowOpt.TotalBalanceForce forcing1, (_,FlowOpt.LossSys loss1)))= 
                Interp.lessThanWithInvalid (loss Arith.~+ forcing) (loss1 Arith.~+ forcing1) 


sumSinkSourceFlows ::(Arith.Sum v) =>  
  FlowOpt.EndNodeEnergies node v ->
  (FlowOpt.TotalSourceFlow v,FlowOpt.TotalSinkFlow v, FlowOpt.StorageMap node (Maybe (FlowOpt.StorageFlow v)))
sumSinkSourceFlows endNodeFlows = (FlowOpt.TotalSourceFlow $ sumRes sourceMap, 
                                   FlowOpt.TotalSinkFlow $ sumRes sinkMap, 
                                   stoMap)
  where
   sinkMap = FlowOpt.unSinkMap $ FlowOpt.getSinkMap endNodeFlows
   sourceMap = FlowOpt.unSourceMap $ FlowOpt.getSourceMap endNodeFlows
   stoMap = FlowOpt.getStorageMap endNodeFlows
   sumRes = foldl1 (Arith.~+) . Map.elems 
