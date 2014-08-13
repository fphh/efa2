{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.StateFlow.ScaleMap where

--import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
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
modul = ModuleName "Action.Flow.Topology.ScaleMap"

nc :: FunctionName -> Caller
nc = genCaller modul


calculateScales ::
  Caller ->
  StateQty.Graph node (Result.Result ((D.Data D.Nil (a)))) 
                      (Result.Result ((D.Data D.Nil (a)))) ->
  a ->
  Idx.State ->
  (ScaleSource a, ScaleSink a)
calculateScales sfg =   
  
  
  
getEndNodeFlows :: 
  Node.C node =>
  StateQty.Graph node (Result.Result ((D.Data D.Nil (a)))) 
                      (Result.Result ((D.Data D.Nil (a)))) ->
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
   

  
  
