{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Action.Optimisation where

import EFA.Data.Vector as DV
import EFA.Data.ND as ND
import EFA.Data.Axis.Strict as Strict
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
  (Collection.OrdData (CubeMap.Cube inst dim label vec a b)
   ~ ND.Data dim (Strict.Axis inst label vec a),
   Collection.ValData (CubeMap.Cube inst dim label vec a b)
   ~ CubeMap.Data inst dim vec b,
   Eq b,
   Zipper vec,
   Ord b, 
   Show b, 
   Storage vec a, 
   FromList vec,
   Walker vec,
   Storage vec b,
   Storage vec Bool,
   Singleton vec,
   Length vec,
   Arith.Constant b,
   Node.C node) =>
  Topo.Topology node -> 
  Map (TopoIdx.Position node) (Name, Name) -> 
  Map Name (Params.EtaFunction b b) -> 
  Collection.Collection (TopoIdx.Position node) (CubeMap.Cube inst dim label vec a b) -> 
  FlowTopo.Section node (Result.Result (CubeMap.Data inst dim vec b))
solve topology etaAssign etaFunc powerCollection =
   EqSys.solve (quantityTopology topology) $
   given etaAssign etaFunc powerCollection


given :: 
  (Collection.OrdData (CubeMap.Cube inst dim label vec a b)
   ~ ND.Data dim (Strict.Axis inst label vec a),
   Collection.ValData (CubeMap.Cube inst dim label vec a b)
   ~ CubeMap.Data inst dim vec b,
   ULSystem.Value mode (CubeMap.Data inst dim vec b),
   Node.C node, 
   Storage vec a, 
   Length vec,
   Walker vec,
   Storage vec b,
   Singleton vec,
   Arith.Constant b,
   Verify.GlobalVar mode (CubeMap.Data inst dim vec b) (RecIdx.Record RecIdx.Absolute (Variable.Signal node)),
   Ord b, Show b, Zipper vec, FromList vec) =>
   Map (TopoIdx.Position node) (Name, Name) -> 
  Map Name (Params.EtaFunction b b) -> 
  Collection.Collection (TopoIdx.Position node) (CubeMap.Cube inst dim label vec a b) -> 
  EqSys.EquationSystem mode node s (CubeMap.Data inst dim vec b)
given etaAssign etaFunc (Collection.Collection grid mp) =
   (XIdx.dTime .= (CubeMap.Data $ DV.replicate (CubeGrid.linearLength grid) Arith.one))
   <> EqSys.withExpressionGraph (makeEtaFuncGiven etaAssign etaFunc)
   <> Fold.fold (Map.mapWithKey f mp)
   where
     f ppos p  =  XIdx.powerFromPosition ppos .= p


-- TODO: Ist diese Funktion am richtigen Platz ?
makeEtaFuncGiven ::
  (ULSystem.Value mode (CubeMap.Data inst dim vec a), 
   Zipper vec, 
   Arith.Sum a, 
   Walker vec, 
   Storage vec a, 
   Ord node, Ord a, 
   Show a, 
   Arith.Constant a) =>
  Map (XIdx.Position node) (Name, Name) ->
  Map Name (Params.EtaFunction a a) ->
  FlowTopo.Section node (EqAbs.Expression mode vars s (CubeMap.Data inst dim vec a)) ->
  EqAbs.VariableSystem mode vars s
makeEtaFuncGiven etaAssign etaFunc topo =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         Fold.foldMap
            (\(eta, power) ->
               eta =.= EqAbs.liftF (CubeMap.mapData (absEtaFunction strP strN etaFunc)) power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id se topo))
      etaAssign

-- TODO: Ist diese Funktion am richtigen Platz ?
absEtaFunction ::
   (Ord a, Show a, Arith.Constant a, Arith.Product b) =>
   Name -> Name -> Map Name (Params.EtaFunction a b) -> a -> b
absEtaFunction strP strN etaFunc =
   let fpos = check strP id $ Map.lookup strP $ Map.map Params.func etaFunc
       fneg = check strN rev $ Map.lookup strN $ Map.map Params.func etaFunc
       rev h = Arith.recip . h . Arith.negate
       check (Name str) =
          maybe (\x -> error ("not defined: '" ++ str ++ "' for " ++ show x))
   in  \x -> if x >= Arith.zero then fpos x else fneg x

-- TODO :: get rid of AppUt.checkDetermined
getPowers ::
   (Ord node, Show node) =>
   CubeGrid.Grid inst dim label vec a ->
   FlowTopo.Section node (Result.Result (CubeMap.Data inst dim vec b)) ->
   Collection.Collection (XIdx.Position node) (CubeMap.Cube inst dim label vec a b)
getPowers grid (FlowTopoPlain.Section _ topo) = Collection.Collection grid (
   Map.mapWithKey (\ key x -> AppUt.checkDetermined (show key) x) $ 
   TopoRecord.topologyToPowerMap topo)

