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

--import qualified EFA.Equation.Arithmetic as Arith
----import EFA.Equation.Arithmetic ((~+), (~/))
--import EFA.Equation.Result (Result(..))
import qualified EFA.Equation.Result as Result

--import qualified EFA.Signal.Data as D

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
--                   (|>),
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

{-
-- WARNING -- needs an absolute State Graph - not yet type safe !
-- TODO make all stateflows absolute        
getEndNodeFlows :: 
  Node.C node =>
  StateQty.Graph node v v ->
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v)
getEndNodeFlows absSfg = 
  Map.mapKeys f $ Map.map (FlowTopoOpt.getEndNodeFlows) $ StateQty.states absSfg
  where f (Idx.State idx) = (Idx.AbsoluteState $ fromIntegral idx) 
-}

extractState:: 
  Caller ->
  Idx.AbsoluteState ->
  Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v) ->
  (FlowOpt.EndNodeEnergies node v, Map.Map Idx.AbsoluteState (FlowOpt.EndNodeEnergies node v))
extractState caller state endNodeFlows = (selectedState, restStates) 
  where selectedState = Maybe.fromMaybe err $ Map.lookup state endNodeFlows
        restStates = Map.filterWithKey (\st _ -> st /= state) endNodeFlows
        err = merror caller modul "extractState" $ "State not found: " ++ show state
                                          
{-
sumEndNodeFlows ::  
  Map.Map Idx.State (FlowOpt.EndNodeEnergies node v) ->
  FlowOpt.EndNodeEnergies node v
  
sumEndNodeFlows flows = 
   sumRes = Map.foldl ((Arith.~+)) Arith.zero . Fold.fold . Map.elems
-}