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

import Debug.Trace(trace)

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology.ScaleMap"

nc :: FunctionName -> Caller
nc = genCaller modul


calcEtaLossSys ::
  (Ord a, Arith.Sum a,Arith.Constant a, Show a,
   DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Interp.Val a, Interp.Val a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus, Interp.Val a),
   DV.Storage vec (Interp.Val a, Interp.Val a, Interp.Val a),
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec (Interp.Val a)) =>
  Caller -> 
  FlowOpt.GlobalLifeCycleMap node (Interp.Val a) ->
  FlowOpt.ScaleMap (Interp.Val a) ->
  FlowOpt.EndNodeEnergies node (CubeMap.Data (Sweep.Search inst) dim vec (Interp.Val a)) ->
  CubeMap.Data (Sweep.Search inst) dim vec ActFlowCheck.EdgeFlowStatus ->  
  CubeMap.Data (Sweep.Search inst) dim vec (ActFlowCheck.EdgeFlowStatus,FlowOpt.OptimalityMeasure (Interp.Val a))
calcEtaLossSys caller globalLifeCycleMap scaleMap topoEndNodeEnergies flowStatus = 
  CubeMap.zipWithData f flowStatus  (CubeMap.zipWith3Data ((,,)) sourceFlow sinkFlow (FlowOpt.unStorageFlow $ Maybe.fromMaybe err $ 
                                                                                      FlowOpt.getSingleStorage newCaller stoMap))
    where
      err = merror caller modul "calcEtaLossSys" "No Storage in System" 
      newCaller = caller |> nc "calcEtaLossSys"
      (FlowOpt.TotalSourceFlow sourceFlow, FlowOpt.TotalSinkFlow sinkFlow,stoMap) = 
                    FlowTopoOpt.sumSinkSourceFlows topoEndNodeEnergies
   
      f status (src,snk,sto) = (status, calcEtaLossWithScales newCaller globalLifeCycleMap scales 
                                       (FlowOpt.TotalSourceFlow src,
                                        FlowOpt.TotalSinkFlow snk,
                                        FlowOpt.TotalStorageFlow sto))
                               
        where scales = FlowOpt.lookupScales newCaller scaleMap (ActFlowCheck.getState status) 
   

calcEtaLossWithScales :: (Ord a,Arith.Constant a, Show a) =>
  Caller ->
  FlowOpt.GlobalLifeCycleMap node a ->
  (FlowOpt.ScaleSource a, FlowOpt.ScaleSink a, FlowOpt.ScaleSto a) ->                                       
  (FlowOpt.TotalSourceFlow a, FlowOpt.TotalSinkFlow a, FlowOpt.TotalStorageFlow a) ->
  FlowOpt.OptimalityMeasure a
  
calcEtaLossWithScales caller globalLifeCycleMap 
  (FlowOpt.ScaleSource srcScale, FlowOpt.ScaleSink snkScale, FlowOpt.ScaleSto stoScale) 
  (FlowOpt.TotalSourceFlow srcFl, FlowOpt.TotalSinkFlow snkFl, FlowOpt.TotalStorageFlow stoFl) = 
    FlowOpt.OptimalityMeasure (FlowOpt.EtaSys $ f etaSys) (FlowOpt.LossSys $ srcTerm Arith.~- snkTerm)

         where etaSys = snkTerm Arith.~/ srcTerm
               
               -- use absolute Storage flow when applying the scales
               -- absolute storage flow was used in the calculation of the scalemap as well
               absStoFl = Arith.abs stoFl
               srcTerm = srcFl Arith.~+ absStoFl Arith.~* srcScale Arith.~+ stoTermDisCharge
               snkTerm = snkFl Arith.~+ absStoFl Arith.~* snkScale Arith.~+ stoTermCharge
               

               -- | when storage is charged, positive power, apply usage efficiency for latter use
               stoTermCharge = Arith.nullifyNegative $ stoScale Arith.~* absStoFl Arith.~* etaUse
               
               -- | when storage is discharged, negative power, apply generation efficiency 
               stoTermDisCharge = Arith.nullifyPositive $ stoScale Arith.~* absStoFl Arith.~/ etaGen
               
               (FlowOpt.GenerationEfficiency etaGen, FlowOpt.UsageEfficiency etaUse) = 
                 FlowOpt.getSingleLifeCycleEtas (caller |> nc "calcEtaLossWithScales") globalLifeCycleMap
                                                  
               -- manual Switch on / off trace
               f x = if True 
                     then (if x < Arith.zero || x > Arith.one then trace msg x else x)
                     else x                                                              
                                                             
               msg = "etaSys inkorrekt: " ++ show etaSys ++ " | src: " ++ show srcTerm ++ 
                     " | snk: " ++ show snkTerm ++ " | stoC: " ++ show stoTermCharge ++ " | stoD: " ++ show stoTermDisCharge   
                 


