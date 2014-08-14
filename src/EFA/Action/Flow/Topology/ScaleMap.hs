{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology.ScaleMap where

--import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Value.State as ValueState 
--import qualified EFA.Action.Flow.Topology.Check as ActFlowTopoCheck
import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
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
import Control.Monad as Monad
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

calcEtaLossSys ::
  (Arith.Sum a,Arith.Constant a,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Interp.Val a, Interp.Val a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, Interp.Val a),
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (Interp.Val a)) =>
  Caller -> 
  FlowOpt.ScaleMap (Interp.Val a) ->
--  .Map (Maybe Idx.AbsoluteState) (Maybe (FlowOpt.ScaleSource (Interp.Val a), FlowOpt.ScaleSink (Interp.Val a))) ->
  FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) dim vec (Interp.Val a)) ->
  CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus ->  
  CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityMeasure (Interp.Val a))
calcEtaLossSys caller (FlowOpt.ScaleMap scaleMap) topoEndNodeEnergies flowStatus = g maybeStorageFlow 
  where
   newCaller = caller |> nc "calcEtaLossSys"
   maybeStorageFlow = Map.elems $ FlowOpt.unStorageMap $ FlowOpt.getStorageMap topoEndNodeEnergies
   
   (FlowOpt.TotalSourceFlow sourceFlow, 
    FlowOpt.TotalSinkFlow sinkFlow) = FlowTopoOpt.sumSinkSourceFlows topoEndNodeEnergies
   
   g [] = CubeMap.zipWithData h2 flowStatus $ CubeMap.zipWithData ((,)) sourceFlow sinkFlow
   g [Nothing] =  CubeMap.zipWithData h2 flowStatus $ CubeMap.zipWithData ((,)) sourceFlow sinkFlow
   g [Just (FlowOpt.StorageFlow storageFlow)] = CubeMap.zipWithData h1 
                                                (CubeMap.zipWithData ((,)) flowStatus storageFlow)
                                                (CubeMap.zipWithData ((,)) sourceFlow sinkFlow)

   g _ = merror caller modul "calcEtaLossSys" "Method works only for one storage"     
   
   h1 (status,x) (srcFl,snkFl) = f scales
     where
       scales = h3 (ActFlowCheck.getState status)
       h3 (Just st) = Maybe.fromMaybe err $ Monad.join $ Map.lookup st scaleMap 
       h3 (Nothing) = (FlowOpt.ScaleSource $ Interp.Invalid ["calcEtaLossSys"],
                      FlowOpt.ScaleSink $ Interp.Invalid ["calcEtaLossSys"])
       err = merror caller modul "calcEtaLossSys" $ "State not in Eta-ScaleMap - status: " ++ show status
       f(FlowOpt.ScaleSource sourceScale, 
           FlowOpt.ScaleSink sinkScale) = (status, FlowOpt.OptimalityMeasure 
                                                   (FlowOpt.EtaSys $ snkTerm Arith.~/ srcTerm) 
                                                   (FlowOpt.LossSys $ srcTerm Arith.~- snkTerm))
                                          
         where srcTerm = srcFl Arith.~+ sourceScale Arith.~* x
               snkTerm = snkFl Arith.~+ sinkScale Arith.~* x
   
   h2 status (src,snk) = (status, FlowOpt.OptimalityMeasure 
                                                   (FlowOpt.EtaSys $ snk Arith.~/ src) 
                                                   (FlowOpt.LossSys $ src Arith.~- snk))            
         

                                           