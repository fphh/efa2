{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Action.Optimisation.Cube.Solve where

import EFA.Action.EtaFunctions as EtaFunctions

import qualified EFA.Data.Interpolation as Interp
import EFA.Data.Vector as DV
--import EFA.Data.ND as ND
-- import EFA.Data.Axis.Strict as Strict
import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (quantityTopology)
--import qualified EFA.Application.Optimisation.Sweep as Sweep
import EFA.Application.Optimisation.Params (Name(Name))
import qualified EFA.Application.Optimisation.Params as Params

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology.Variable as Variable
import EFA.Flow.Topology.Absolute ( (.=), 
                                    (=.=) )

--import qualified EFA.Flow.Absolute as EqAbs
import qualified EFA.Flow.Absolute as EqAbs

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Verify as Verify
--import EFA.Equation.Result (Result)
--import qualified EFA.Flow.Topology.Record as TopoRecord

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid

solve ::
  (Eq a,
   Arith.Constant a,
   Storage vec a,
   Node.C node,
   Zipper vec,
   Walker vec,
   Storage vec (Interp.Val a),
   Storage vec Bool,
   Singleton vec,
   Length vec) =>
  Topo.Topology node -> 
  EtaFunctions.FunctionMap node a ->  
  Collection.Collection (TopoIdx.Position node) (CubeMap.Cube inst dim label vec a (Interp.Val a)) -> 
  FlowTopo.Section node (Result.Result (CubeMap.Data inst dim vec (Interp.Val a)))
solve topology etaFunctions powerCollection =
   EqSys.solve (quantityTopology topology) $
   given etaFunctions powerCollection

given :: 
  (Verify.GlobalVar mode (CubeMap.Data inst dim vec (Interp.Val a)) (RecIdx.Record RecIdx.Absolute (Variable.Signal node)), 
   ULSystem.Value mode (CubeMap.Data inst dim vec a),
   Arith.Constant a,Storage vec (Interp.Val a),
   Storage vec a, Zipper vec,Walker vec,
   Length vec, 
   Node.C node, 
   Singleton vec)=>
  EtaFunctions.FunctionMap node a ->
  Collection.Collection (TopoIdx.Position node) (CubeMap.Cube inst dim label vec a (Interp.Val a)) -> 
  EqSys.EquationSystem mode node s (CubeMap.Data inst dim vec (Interp.Val a))
given etaFunctions  (Collection.Collection grid mp) =
   (XIdx.dTime .= (CubeMap.Data $ DV.replicate (CubeGrid.linearLength grid) Arith.one))
   <> EqSys.withExpressionGraph (makeEtaFuncGiven etaFunctions)
   <> Fold.fold (Map.mapWithKey f mp)
   where
     f ppos p  =  XIdx.powerFromPosition ppos .= p


makeEtaFuncGiven:: 
  (ULSystem.Value
   mode (CubeMap.Data inst dim vec (Interp.Val a)), Walker vec, Storage vec (Interp.Val a),
   Ord node,
   Arith.Sum a, Zipper vec) =>
  EtaFunctions.FunctionMap node a ->  
  FlowTopo.Section node (EqAbs.Expression mode vars s (CubeMap.Data inst dim vec (Interp.Val a))) ->
  EqAbs.VariableSystem mode vars s
makeEtaFuncGiven etaFunctions topo =
   Fold.fold $
   Map.mapWithKey
      (\position etaFunc ->
         Fold.foldMap
            (\(eta, power) ->
               eta =.= EqAbs.liftF (CubeMap.mapData etaFunc) power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id position topo))
      etaFunctions

-- TODO :: get rid of AppUt.checkDetermined
getPowers ::
   (Ord node, Show node) =>
   CubeGrid.Grid inst dim label vec a ->
   FlowTopo.Section node (Result.Result (CubeMap.Data inst dim vec b)) ->
   Collection.Collection (XIdx.Position node) (CubeMap.Cube inst dim label vec a b)
getPowers grid (FlowTopoPlain.Section _ topo) = Collection.Collection grid (
   Map.mapWithKey (\ key x -> AppUt.checkDetermined (show key) x) $ 
   TopoRecord.topologyToPowerMap topo)

