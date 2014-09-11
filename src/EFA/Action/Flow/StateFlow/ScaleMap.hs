{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.StateFlow.ScaleMap where

--import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified  EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
--import qualified EFA.Action.Flow.Balance as ActBal
import qualified EFA.Action.Utility as ActUt

--import qualified EFA.Data.ND.Cube.Grid as CubeGrid
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Interpolation as Interp

--import qualified EFA.Flow.Topology.Quantity as TopoQty
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
import qualified EFA.Flow.State.Quantity as StateQty

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
import qualified EFA.Utility.Trace as UtTrace 

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology.ScaleMap"

nc :: FunctionName -> Caller
nc = genCaller modul

-- | This Method works only for one Storage in the system
calculateScaleMap ::
  (Ord a,
   Show a,
   Show node,
   Arith.Constant a,
   Node.C node,
   Show (FlowOpt.ScaleMap (Interp.Val a)))=>
  Caller -> 
  FlowOpt.GlobalLifeCycleMap node (Interp.Val a) ->
  StateQty.Graph node (Interp.Val a) (Interp.Val a) ->
  FlowOpt.ScaleMap (Interp.Val a) ->
  ((Interp.Val a), FlowOpt.ScaleMap (Interp.Val a))
calculateScaleMap caller globalLifeCycleMap sfg (FlowOpt.ScaleMap oldScaleMap) = UtTrace.simTrace "ScaleMap" $
  (etaSys, FlowOpt.ScaleMap $ Map.union (Map.mapWithKey f endNodeMap) oldScaleMap)
  where
    newCaller = caller |> nc "calculateScaleMap"
    etaSys = StateFlowOpt.calculateEtaSys newCaller globalLifeCycleMap sfg
    endNodeMap = StateFlowOpt.getEndNodeFlows newCaller sfg
    f state (FlowOpt.EndNodeEnergies _ _ stoMapState) = g $ Map.elems $ FlowOpt.unStorageMap stoMapState
      where 

       g [Just (FlowOpt.StorageFlow x)] = UtTrace.simTrace "Source,SinkScale" $ 
                                          Just (FlowOpt.ScaleSource $ sourceFlow Arith.~/x, 
                                                FlowOpt.ScaleSink $ sinkFlow Arith.~/x, 
                                                FlowOpt.ScaleSto $ stoBalance Arith.~/x)
       g [Nothing] = Nothing             
       g _ = merror caller modul "calculateScaleMap" "This Method works only for one Storage in the system"
       (FlowOpt.StorageFlow stoBalance) = Maybe.fromMaybe err $ FlowOpt.getSingleStorage newCaller stoMapRest
       err = merror caller modul "calculateScaleMap" "No Storage balance found"  
       (FlowOpt.TotalSourceFlow sourceFlow, 
        FlowOpt.TotalSinkFlow sinkFlow, 
        stoMapRest) = UtTrace.simTrace "srcSinkFlow" $ StateFlowOpt.sumSinkSourceFlowsAllStates 
                                              $ StateFlowOpt.removeState (UtTrace.simTrace "state" state) endNodeMap
            

       
      
      
  
  
  
  

  
  
