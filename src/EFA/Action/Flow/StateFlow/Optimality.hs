{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.StateFlow.Optimality where

--import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
--import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Action.Flow.Balance as ActBal
--import qualified EFA.Action.Utility as ActUt
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap

import qualified EFA.Data.Interpolation as Interp

--import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Data.ND.Cube.Map as CubeMap
--import qualified EFA.Data.Interpolation as Interp

--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.State.Quantity as StateQty
--import qualified EFA.Flow.Topology as FlowTopo
--import EFA.Utility.Trace(mytrace)

--import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Data.Vector as DV

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
----import EFA.Equation.Arithmetic ((~+), (~/))
--import EFA.Equation.Result (Result(..))
import qualified EFA.Equation.Result as Result

import qualified EFA.Signal.Data as D

import qualified Data.Map as Map
--import qualified Data.Set as Set

--import qualified EFA.Graph as Graph

--import qualified Data.Map as Map

--import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
--import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Report.Format as Format

import qualified Data.Maybe as Maybe
import Control.Applicative (liftA2) --, liftA)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as Fold
--import Debug.Trace(trace)

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.StateFlow.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul



unresultStateFlow :: 
  Caller ->
  StateQty.Graph node (Result.Result v) (Result.Result v) ->
  StateQty.Graph node v v 
unresultStateFlow caller sfg = StateQty.mapGraph f f sfg  
  where f (Result.Determined x) = x
        f  Result.Undetermined = merror caller modul "unresultStateFlow"
                                 "StateFlowGraph contains undetermined values"

unDataStateFlow :: 
  StateQty.Graph node (D.Data D.Nil a) (D.Data D.Nil a) ->
  StateQty.Graph node a a 
unDataStateFlow sfg = StateQty.mapGraph f f sfg  
  where f (D.Data x) = x


-- WARNING -- needs an absolute State Graph - not yet type safe !
-- TODO make all stateflows absolute        
getEndNodeFlows :: 
  (Node.C node,Arith.Sum v) =>
  Caller ->
  StateQty.Graph node v v ->
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v)
getEndNodeFlows caller absSfg = 
  Map.mapKeys f $ Map.map (FlowTopoOpt.getEndNodeFlows newCaller) $ StateQty.states absSfg
  where f (Idx.State idx) = (Idx.AbsoluteState $ fromIntegral idx) 
        newCaller = caller |> nc "getEndNodeFlows"


removeState:: 
  Idx.AbsoluteState ->
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v) ->
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v)
removeState state endNodeFlows = Map.filterWithKey (\st _ -> st /= state) endNodeFlows

                                          
sumSinkSourceFlowsAllStates ::(Arith.Sum v) =>  
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v) ->
  (FlowOpt.TotalSourceFlow v,FlowOpt.TotalSinkFlow v)
sumSinkSourceFlowsAllStates endNodeFlowsMap = 
  foldl1 f $ Map.elems $ Map.map FlowTopoOpt.sumSinkSourceFlows endNodeFlowsMap
  where f (x1,y1) (x2,y2) = (liftA2 (Arith.~+) x1 x2, 
                             liftA2 (Arith.~+) y1 y2)


calculateEtaSys :: (Node.C node,Show node,Ord a, Arith.Sum a,Arith.Constant a) =>
  Caller ->
  FlowOpt.GlobalLifeCycleMap node (Interp.Val a) ->
  StateQty.Graph node (Interp.Val a) (Interp.Val a) ->
  (Interp.Val a)
calculateEtaSys caller globalLifeCycleMap sfg = let
    newCaller = caller |> nc "calculateEtaLossSys"
    (FlowOpt.TotalSourceFlow src,FlowOpt.TotalSinkFlow snk) = sumSinkSourceFlowsAllStates endNodeFlowsMap
    endNodeFlowsMap = getEndNodeFlows newCaller sfg                                                          
  
    initBalance = Map.toList $ fmap (PartMap.init . Storage.nodes) (StateQty.storages sfg)
    exitBalance = Map.toList $ fmap (PartMap.exit . Storage.nodes) (StateQty.storages sfg)

    balance = zipWith (\(node,x) (node1,y) -> 
                        if node == node1 then (node,x Arith.~- y) else err node node1) exitBalance initBalance
    
    err n n1 = merror caller modul "calculateEtaSys" $ "inconsistent storage nodes: " ++ show n ++ " " ++ show n1
    err2 n =  merror caller modul "calculateEtaSys" $ "node not found in globalLifeCycleMap: " ++ show n

    sumCharge = 
         Fold.foldl (\acc (node,x) -> let 
                        (FlowOpt.UsageEfficiency etaUse) = snd $  Maybe.fromMaybe (err2 node) $
                             FlowOpt.lookupLifeCycleEtaGlobalLifeCycleEta globalLifeCycleMap node  
                                 in if x > Arith.zero 
                                   then acc Arith.~+ (x Arith.~* etaUse) 
                                   else acc) Arith.zero 

       
    sumDischarge = 
          Fold.foldl (\acc (node,x) -> let 
                         (FlowOpt.GenerationEfficiency etaGen) =  fst $ Maybe.fromMaybe (err2 node) $
                                FlowOpt.lookupLifeCycleEtaGlobalLifeCycleEta globalLifeCycleMap node
                         in if x < Arith.zero 
                            then acc Arith.~- (x Arith.~/ etaGen) 
                            else acc) Arith.zero
  
    in (snk Arith.~+ (sumCharge balance))
        Arith.~/ 
        (src Arith.~+ (sumDischarge balance))
