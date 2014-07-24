{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology.Optimality where


import qualified EFA.Action.DemandAndControl as DemandAndControl

import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Balance as ActBal

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
import EFA.Equation.Result (Result(..))
import qualified EFA.Equation.Result as Result

import qualified Data.Map as Map

import qualified EFA.Graph as Graph

--import qualified Data.Map as Map

--import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Report.Format as Format

import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2, liftA)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

import Debug.Trace(trace)

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul

-- TODO :: old Code adjust to new needs
data Orientation = Dir | UnDir deriving Show

absoluteStateIndex ::
  (Node.C node) =>
  Graph.Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph.Graph node Graph.EitherEdge nodeLabel a ->
  Idx.AbsoluteState
absoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber: edge not found "
                          ++ Format.showRaw (Node.display f :: Format.ASCII)
                          ++ "->"
                          ++ Format.showRaw (Node.display t :: Format.ASCII)

      toTernary xs = Idx.AbsoluteState $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels

data EndNodeEnergies node v = EndNodeEnergies 
                              {getSinkMap:: (FlowOpt.SinkMap node v),
                              getSourceMap :: FlowOpt.SourceMap node v,
                              getStorageMap :: FlowOpt.StorageMap node (Maybe (TopoQty.Sums v))} deriving Show

getEndNodeFlows :: 
  Node.C node =>
  TopoQty.Section node v ->
  EndNodeEnergies node v
getEndNodeFlows flowSection =
   let topo = TopoQty.topology flowSection
       
       nodes = Graph.nodeLabels topo
       
       sinks = FlowOpt.SinkMap $ Map.mapMaybe TopoQty.sumIn $ 
               Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node) nodes
               
       sources = FlowOpt.SourceMap $ Map.mapMaybe TopoQty.sumOut $ 
               Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node) nodes

       storages = FlowOpt.StorageMap $ Map.mapWithKey (\node _ -> TopoQty.lookupSums node flowSection) 
              $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
                  
    in (EndNodeEnergies sinks sources storages)
      
lookupControlVar :: Ord node => TopoQty.Section node v -> DemandAndControl.ControlVar node -> Maybe v
lookupControlVar flowSection (DemandAndControl.ControlPower idx) = TopoQty.lookupPower idx flowSection
lookupControlVar flowSection (DemandAndControl.ControlRatio idx) = TopoQty.lookupX idx flowSection

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
   DV.Singleton vec,
   DV.Length vec) =>
  Caller ->
  CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus ->
  FlowOpt.LifeCycleMap node a ->
  EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) dim vec (Interp.Val a)) ->
  CubeMap.Data (Sweep.Search inst) dim vec (FlowOpt.Eta2Optimise (Interp.Val a),
                             FlowOpt.Loss2Optimise (Interp.Val a))
calcEtaLossSys caller state lifeCycleEfficiencies (EndNodeEnergies (FlowOpt.SinkMap sinks) (FlowOpt.SourceMap sources) (FlowOpt.StorageMap  storages)) = let 
  chargeStorages = Map.mapMaybeWithKey (\node x -> 
          (applyUsageEfficiency caller state lifeCycleEfficiencies node) x) storages
                   
  dischargeStorages = Map.mapMaybeWithKey (\node x ->
          (applyGenerationEfficiency caller state lifeCycleEfficiencies node) x)  storages
  
  makeSum = foldl1 (Arith.~+) . Map.elems
  sinkTerm =  (makeSum sinks)  Arith.~+ (makeSum chargeStorages)
  sourceTerm = (makeSum sources) Arith.~+ (makeSum dischargeStorages)
  eta = (CubeMap.mapData FlowOpt.EtaSys) $ sinkTerm Arith.~/ sourceTerm
  loss = (CubeMap.mapData FlowOpt.LossSys) $ sourceTerm Arith.~- sinkTerm
  in (CubeMap.zipWithData ((,))) eta loss

-- TODO: move to right place -- use in applyGenerationEfficiency,applyUsageEfficiency
getStoragePowerWithSign :: (Arith.Sum v) => TopoQty.Sums v -> Maybe v
getStoragePowerWithSign sums = case sums of                 
  -- TODO how to mach case Just Just ?
  TopoQty.Sums Nothing (Just energy) -> Just $ energy
  TopoQty.Sums  (Just energy) Nothing -> Just $ Arith.negate energy
  TopoQty.Sums Nothing Nothing -> Nothing 

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
  FlowOpt.LifeCycleMap node a ->
  node ->
  Maybe (TopoQty.Sums (CubeMap.Data inst dim vec (Interp.Val a))) ->
  Maybe (CubeMap.Data inst dim vec (Interp.Val a))
applyGenerationEfficiency _ _ _ _ Nothing = Nothing
applyGenerationEfficiency caller state lifeCycleEfficiencies node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ (CubeMap.zipWithData f) energy state
  TopoQty.Sums  (Just energy) Nothing -> Just $ (CubeMap.zipWithData (f. Arith.negate)) energy state
  TopoQty.Sums Nothing Nothing -> Nothing 
  _ -> e2  
  where
    e2 = merror caller modul "applyGenerationEfficiency" 
         ("Storage-Sums Variable contains values in positive and negative part-node: " ++ show node)
    f x st = if x > Arith.zero then x Arith.~/ eta else Arith.zero
      where
        (FlowOpt.GenerationEfficiency eta,_) = 
          Maybe.fromMaybe e $ FlowOpt.lookupLifeCycleEta lifeCycleEfficiencies (ActFlowCheck.getState st) node 
        e = merror caller modul "applyGenerationEfficiency" 
                    ("Node not in LifeCycleEfficiencyMap: " ++ show node)
                    

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
  FlowOpt.LifeCycleMap node a ->
  node ->
  Maybe (TopoQty.Sums (CubeMap.Data inst dim vec (Interp.Val a))) ->
  Maybe (CubeMap.Data inst dim vec (Interp.Val a))
applyUsageEfficiency _ _ _ _ Nothing = Nothing
applyUsageEfficiency caller state lifeCycleEfficiencies node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ (CubeMap.zipWithData (f. Arith.negate)) energy state
  TopoQty.Sums  (Just energy) Nothing -> Just $ (CubeMap.zipWithData f) energy state
  TopoQty.Sums Nothing Nothing -> Nothing 
  _ -> e2  
  where
    e2 = merror caller modul "applyUsageEfficiency" 
         ("Storage-Sums Variable contains values in positive and negative part-node: " ++ show node)
    f x st = if x > Arith.zero then x Arith.~* eta else Arith.zero
      where 
        (_,FlowOpt.UsageEfficiency eta) = 
          Maybe.fromMaybe e $ FlowOpt.lookupLifeCycleEta lifeCycleEfficiencies (ActFlowCheck.getState st) node 
    e = merror caller modul "applyUsageEfficiency" 
                    ("Node not in LifeCycleEfficiencyMap: " ++ show node)
--    e3 = merror caller modul "applyUsageEfficiency" 
--                    ("Undefined State")
                    
calculateOptimalityMeasure :: 
  (Ord node, Ord a, Show node, Arith.Constant a, DV.Zipper vec,Show (vec a),
   DV.Storage vec (FlowOpt.Eta2Optimise a, FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.TotalBalanceForce a),
   DV.Storage vec (FlowOpt.OptimalityMeasure a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure a),
   DV.Storage vec (FlowOpt.Eta2Optimise a),
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Walker vec, DV.Storage vec a, DV.Singleton vec,
   DV.Storage vec (a, a),
   DV.Storage vec (FlowOpt.Eta2Optimise (Interp.Val a),
                   FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec (FlowOpt.Eta2Optimise (Interp.Val a)),
   DV.Storage vec (FlowOpt.Loss2Optimise (Interp.Val a)),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (a, a)),
   DV.Storage vec (FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus,
                         FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Length vec) =>
   Caller -> 
   CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus ->
   FlowOpt.LifeCycleMap node a -> 
    EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) dim vec (Interp.Val a)) ->
   CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure (Interp.Val a))
calculateOptimalityMeasure caller state lifeCycleEfficiencies endNodeEnergies = let
    etaLossSys = calcEtaLossSys caller state lifeCycleEfficiencies endNodeEnergies
  in CubeMap.zipWithData ((,)) state $ (CubeMap.mapData (\(x,y) -> FlowOpt.OptimalityMeasure x y)) etaLossSys

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
   EndNodeEnergies node ((CubeMap.Data (Sweep.Search inst) dim vec a)) ->
   (CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityMeasure a)) ->
   (CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues a))
objectiveFunctionValues caller balanceForcing (EndNodeEnergies _ _ (FlowOpt.StorageMap stos)) optimalityMeasure = let
    forcing = (CubeMap.mapData FlowOpt.TotalBalanceForce) $ 
              makeSum $ Map.mapWithKey (\node x -> applyBalanceForcing caller balanceForcing node x) stos
    makeSum = foldl1 ((Arith.~+)) . Maybe.catMaybes . Map.elems
  in (CubeMap.zipWithData (\(st,optMeas) fo -> (st,FlowOpt.OptimalityValues optMeas fo))) optimalityMeasure forcing

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

-- | If State is Nothing, than state could not be detected because of invalid values
findMaximumEta :: 
  (DV.Walker vec,Ord a, Arith.Constant a,
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage vec (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)),
   DV.Storage vec (CubeGrid.LinIdx, (ActFlowCheck.EdgeFlowStatus, (Interp.Val a, Interp.Val a)))) =>
  Caller ->
  CubeMap.Data (Sweep.Search inst) dim vec 
                 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a)) -> 
  ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val a)))
findMaximumEta caller cubeData = CubeMap.findBestWithIndexByPerState (ActFlowCheck.getState . fst) f cubeData
  where  f (_,FlowOpt.OptimalityValues (FlowOpt.OptimalityMeasure (FlowOpt.EtaSys eta) _) (FlowOpt.TotalBalanceForce forcing)) 
           (_,FlowOpt.OptimalityValues (FlowOpt.OptimalityMeasure (FlowOpt.EtaSys eta1) _) (FlowOpt.TotalBalanceForce forcing1))= 
                Interp.greaterThanWithInvalid (eta Arith.~+ forcing) (eta1 Arith.~+ forcing1) 

findMinimumLoss :: 
  (DV.Walker vec,Ord a, Arith.Constant a,
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
  Result.Result (CubeMap.Data (Sweep.Search inst) dim vec 
                 (ActFlowCheck.EdgeFlowStatus,
                  (FlowOpt.TotalBalanceForce (Interp.Val a),
                   (FlowOpt.Eta2Optimise (Interp.Val a), FlowOpt.Loss2Optimise (Interp.Val a))))) -> 
  ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,(FlowOpt.TotalBalanceForce (Interp.Val a),
                   (FlowOpt.Eta2Optimise (Interp.Val a), FlowOpt.Loss2Optimise (Interp.Val a)))))
findMinimumLoss caller cubeData = CubeMap.findBestWithIndexByPerState (ActFlowCheck.getState . fst) f $ g cubeData
  where  f (_,(FlowOpt.TotalBalanceForce forcing,(_,FlowOpt.LossSys loss))) 
           (_,(FlowOpt.TotalBalanceForce forcing1, (_,FlowOpt.LossSys loss1)))= 
                Interp.lessThanWithInvalid (loss Arith.~+ forcing) (loss1 Arith.~+ forcing1) 
         g (Determined x) = x
         g (Undetermined) = merror caller modul "findMinimumLoss" "Undetermined Variables in Solution" 

