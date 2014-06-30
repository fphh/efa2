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

import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Report.Format as Format

import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2, liftA)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

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
                              (FlowOpt.SinkMap node v)
                              (FlowOpt.SourceMap node v)
                              (FlowOpt.StorageMap node (Maybe (TopoQty.Sums v)))


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
                  
--       control = Map.fromList $ zip controlVars $ map (lookupControlVar flowSection) controlVars          
               
    in (EndNodeEnergies sinks sources storages)
      
lookupControlVar :: Ord node => TopoQty.Section node v -> DemandAndControl.ControlVar node -> Maybe v
lookupControlVar flowSection (DemandAndControl.ControlPower idx) = TopoQty.lookupPower idx flowSection
lookupControlVar flowSection (DemandAndControl.ControlRatio idx) = TopoQty.lookupX idx flowSection

calcEtaLossSys :: 
  (Ord node,DV.Zipper vec, 
   DV.Storage vec (a, a),
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Singleton vec, 
   DV.Length vec,
   Ord a,
   Show node,
   Arith.Constant a,
   DV.Walker vec,
   DV.Storage vec (FlowOpt.Eta2Optimise a),
   DV.Storage vec (FlowOpt.Loss2Optimise a), 
   DV.Storage vec a, 
   DV.Storage vec (FlowOpt.Eta2Optimise a, FlowOpt.Loss2Optimise a)) =>
  Caller ->
  Result (CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus) ->
   FlowOpt.LifeCycleMap node a -> 
   EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a)) -> 
   Result.Result (CubeMap.Data (Sweep.Search inst) dim vec (FlowOpt.Eta2Optimise a,FlowOpt.Loss2Optimise a))
calcEtaLossSys caller state lifeCycleEfficiencies (EndNodeEnergies (FlowOpt.SinkMap sinks) (FlowOpt.SourceMap sources) (FlowOpt.StorageMap  storages)) = let 
  chargeStorages = Map.mapMaybeWithKey (\node x -> 
          (applyUsageEfficiency caller state lifeCycleEfficiencies node) x) storages
                   
  dischargeStorages = Map.mapMaybeWithKey (\node x ->
          (applyGenerationEfficiency caller state lifeCycleEfficiencies node) x)  storages
  
  makeSum = foldl1 (liftA2 (Arith.~+)) . Map.elems
  term = liftA2 (Arith.~+) (makeSum sinks)  (makeSum chargeStorages)
  term1 = liftA2 (Arith.~+) (makeSum sources) (makeSum dischargeStorages)
  eta = fmap (CubeMap.mapData FlowOpt.EtaSys) $ liftA2 (Arith.~/) term term1
  loss = fmap (CubeMap.mapData FlowOpt.LossSys) $ liftA2 (Arith.~-) term1 term
  in liftA2 (CubeMap.zipWithData ((,))) eta loss

applyGenerationEfficiency :: 
  (DV.Walker vec, 
   DV.Zipper vec,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec a,
   Arith.Sum a,
   Ord a, 
   Ord node, Show node,
   Arith.Constant a)=>
 Caller ->  
 Result (CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus) ->
 FlowOpt.LifeCycleMap node a -> 
 node -> 
 Maybe (TopoQty.Sums (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))) ->
 Maybe (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))
applyGenerationEfficiency _ _ _ _ Nothing = Nothing
applyGenerationEfficiency caller state lifeCycleEfficiencies node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ liftA2 (CubeMap.zipWithData f) energy state
  TopoQty.Sums  (Just energy) Nothing -> Just $ liftA2 (CubeMap.zipWithData (f. Arith.negate)) energy state
  TopoQty.Sums Nothing Nothing -> Nothing 
  _ -> e2  
  where
    e2 = merror caller modul "applyGenerationEfficiency" 
         ("Storage-Sums Variable contains values in positive and negative part-node: " ++ show node)
    f x st = if x < Arith.zero then x Arith.~/ eta else Arith.zero
      where
        (FlowOpt.GenerationEfficiency eta,_) = 
          Maybe.fromMaybe e $ FlowOpt.lookupLifeCycleEta lifeCycleEfficiencies (Maybe.fromMaybe e3 $ ActFlowCheck.getState st) node 
        e = merror caller modul "applyGenerationEfficiency" 
                    ("Node not in LifeCycleEfficiencyMap: " ++ show node)
        e3 = merror caller modul "applyGenerationEfficiency" "Undefined State"
                    


applyUsageEfficiency :: 
  (DV.Walker vec, Show node,
   DV.Zipper vec,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec a,
   Arith.Sum a,
   Ord a, 
   Ord node, 
   Arith.Constant a)=> 
  Caller ->  
 Result (CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus) ->
 FlowOpt.LifeCycleMap node a -> 
 node -> 
 Maybe (TopoQty.Sums (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))) ->
 Maybe (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))
applyUsageEfficiency _ _ _ _ Nothing = Nothing
applyUsageEfficiency caller state lifeCycleEfficiencies node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ liftA2 (CubeMap.zipWithData f) energy state
  TopoQty.Sums  (Just energy) Nothing -> Just $ liftA2 (CubeMap.zipWithData (f. Arith.negate)) energy state
  TopoQty.Sums Nothing Nothing -> Nothing 
  _ -> e2  
  where
    e2 = merror caller modul "applyUsageEfficiency" 
         ("Storage-Sums Variable contains values in positive and negative part-node: " ++ show node)
    f x st = if x < Arith.zero then x Arith.~/ eta else Arith.zero
      where 
        (_,FlowOpt.UsageEfficiency eta) = 
          Maybe.fromMaybe e $ FlowOpt.lookupLifeCycleEta lifeCycleEfficiencies (Maybe.fromMaybe e3 $ ActFlowCheck.getState st) node 
    e = merror caller modul "applyUsageEfficiency" 
                    ("Node not in LifeCycleEfficiencyMap: " ++ show node)
    e3 = merror caller modul "applyUsageEfficiency" 
                    ("Undefined State")
                    

objectiveFunctionValues :: 
  (Ord node, Ord a, Show node, Arith.Constant a, DV.Zipper vec,
   DV.Storage vec (FlowOpt.Eta2Optimise a, FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.Loss2Optimise a), 
   DV.Storage vec (FlowOpt.TotalBalanceForce a),
   DV.Storage vec (FlowOpt.OptimalityValues a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues a),
   DV.Storage vec (FlowOpt.Eta2Optimise a),
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Walker vec, DV.Storage vec a, DV.Singleton vec,
   DV.Storage vec (a, a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, (a, a)),
   DV.Length vec) =>
   Caller -> 
   Result (CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus) ->
   FlowOpt.LifeCycleMap node a -> 
   ActBal.Forcing node a ->
   EndNodeEnergies node (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a)) ->
   Result.Result (CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues a))
objectiveFunctionValues caller state lifeCycleEfficiencies balanceForcing 
  endNodeEnergies@(EndNodeEnergies _ _ (FlowOpt.StorageMap stos)) = let
    etaLossSys = calcEtaLossSys caller state lifeCycleEfficiencies endNodeEnergies
    forcing = fmap (CubeMap.mapData FlowOpt.TotalBalanceForce) $ 
              makeSum $ Map.mapWithKey (\node x -> applyBalanceForcing caller balanceForcing node x) stos
    makeSum = foldl1 (liftA2 (Arith.~+)) . Maybe.catMaybes . Map.elems
  in liftA2 (CubeMap.zipWithData ((,))) state $ (liftA2 (CubeMap.zipWithData FlowOpt.OptimalityValues) etaLossSys forcing)

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
 Maybe (TopoQty.Sums (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))) ->
 Maybe (Result.Result (CubeMap.Data (Sweep.Search inst) dim vec a))
applyBalanceForcing _ _ _ Nothing = Nothing
applyBalanceForcing caller balanceForcing node (Just sums) = case sums of
  TopoQty.Sums Nothing (Just energy) -> Just $ fmap (CubeMap.mapData f) energy
  TopoQty.Sums  (Just energy) Nothing -> Just $ fmap (CubeMap.mapData (f. Arith.negate)) energy
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
  Result.Result (CubeMap.Data (Sweep.Search inst) dim vec 
                 (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityValues (Interp.Val a))) -> 
  ValueState.Map (CubeGrid.LinIdx,(ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityValues (Interp.Val a)))
findMaximumEta caller cubeData = CubeMap.findBestWithIndexByPerState (ActFlowCheck.getState . fst) f $ g cubeData
  where  f (_,FlowOpt.OptimalityValues (FlowOpt.EtaSys eta,_) (FlowOpt.TotalBalanceForce forcing)) 
           (_,FlowOpt.OptimalityValues (FlowOpt.EtaSys eta1,_) (FlowOpt.TotalBalanceForce forcing1))= 
                greaterThanWithInvalid (eta Arith.~+ forcing) (eta1 Arith.~+ forcing1) 
         g (Determined x) = x
         g (Undetermined) = merror caller modul "findMaximumEta" "Undetermined Variables in Solution" 

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
                lessThanWithInvalid (loss Arith.~+ forcing) (loss1 Arith.~+ forcing1) 
         g (Determined x) = x
         g (Undetermined) = merror caller modul "findMinimumLoss" "Undetermined Variables in Solution" 

-- if new value is bigger then True (take new value) 
greaterThanWithInvalid :: (Arith.Constant a,Ord a) => Interp.Val a -> Interp.Val a -> Bool
greaterThanWithInvalid (Interp.Invalid _) _ = True 
greaterThanWithInvalid _ (Interp.Invalid _) = False
greaterThanWithInvalid x y = (Interp.unpack y) > (Interp.unpack x)

-- the new value y is taken, when it is smaller
lessThanWithInvalid :: (Arith.Constant a,Ord a) => Interp.Val a -> Interp.Val a -> Bool
lessThanWithInvalid (Interp.Invalid _) _ = True
lessThanWithInvalid _ (Interp.Invalid _) = False
lessThanWithInvalid x y = (Interp.unpack y) < (Interp.unpack x)
