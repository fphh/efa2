{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Application.Optimisation.Optimisation where

--import qualified Modules.Setting as ModSet
import qualified EFA.Application.Utility as AppUt


import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.DoubleSweep as DoubleSweep
import qualified EFA.Application.Type as Type
import qualified EFA.Application.Optimisation.Params as Params
import EFA.Application.Optimisation.Params
  (EtaAssignMap, Name, InitStorageState(InitStorageState))
--import qualified EFA.Application.Optimisation.Balance as Forcing

import EFA.Application.Simulation (makeEtaFuncGiven2)

import qualified EFA.Flow.State as State
import qualified EFA.Flow.State.Absolute as StateAbs
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Index as StateIdx

import qualified EFA.Application.Flow.State.SystemEta as StateEta

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Topology as Topology

import qualified EFA.Graph as Graph

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Vector as Vec
import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified EFA.Graph.Topology.Node as Node

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.Vector.Unboxed as UV(Unbox)
import Data.Monoid (Monoid)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>), mempty)

--import Control.Monad (join)


optionsScalar ::(Arith.Sum a, Verify.LocalVar mode (Data Nil a),
                 Arith.Constant a) =>
  StateAbs.Options mode rec (Data Nil a) (Data Nil a)
optionsScalar =
  StateAbs.optionsBase
    SeqStateEqSys.equalStInOutSums
    (StorageEqSys.customOne (Data Arith.one))

options ::
  (UV.Unbox a, Arith.Constant a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Arith.Sum (sweep vec a),
   Verify.LocalVar mode (sweep vec a)) =>
  Params.Optimisation node list sweep vec a ->
  StateAbs.Options mode rec (sweep vec a) (sweep vec a)
options params =
  StateAbs.optionsBase
    SeqStateEqSys.equalStInOutSums
    (StorageEqSys.customOne (Sweep.fromRational (Params.sweepLength params) Arith.one))


toPowerMap ::
  (Ord node, Show node, Arith.Sum (sweep vec a)) =>
  StateQty.Graph node b (Result (sweep vec a)) ->
  Idx.State ->
  Type.StoragePowerMap node sweep vec a
toPowerMap graph state = Map.mapWithKey g  $ State.storages graph
  where flowTopo = Maybe.fromMaybe (error $ "toPowerMap State" ++ show state ++  "not in Graph")
                   (Map.lookup state $ fmap Topology.topology $ State.states graph)

        look p = StateQty.lookup p graph -- join $ fmap toMaybe $ StateQty.lookup p graph

        g stoNode _ =
          h $ case Set.toAscList $ Graph.adjacentEdges flowTopo stoNode of
                   [x] -> x
                   _ -> error $ "toPowerMap: more or less than one adjacent edge to node"
                                ++ show stoNode
          where
                h (Graph.EDirEdge (Graph.DirEdge from to)) =
                  (if stoNode == from
                      then look
                      else (fmap $ fmap Arith.negate) . look . AppUt.flipPower)
                      (StateIdx.power state from to)
                h _ = error "toPowerMap: undir edge"



solve ::
  (Node.C node, Ord node, Show node,RealFloat a,
   Show a, Ord a, UV.Unbox a, Arith.Constant a,
   Sweep.SweepMap sweep vec a a, Arith.Sum (sweep vec a),
   Sweep.SweepVector vec a, Sweep.SweepClass sweep vec a,
   Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a),
   Monoid (sweep vec Bool),
   Sweep.SweepMap sweep vec a Bool,
   Sweep.SweepClass sweep vec Bool) =>
  Params.Optimisation node list sweep vec a ->
  [TopoIdx.Power node] ->
  Type.EnvResult node (sweep vec a) ->
  EtaAssignMap node ->
  Map Name (Params.EtaFunction a a) ->
  Idx.State ->
  ReqsAndDofs.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a ->
  Type.SweepPerReq node sweep vec a
solve params reqsAndDofs stateFlowGraph etaAssign etaFunc state pts =
  let
      ss = Sweep.unList (ReqsAndDofs.reqs pts)
           ++ Sweep.unList (ReqsAndDofs.dofs pts)
      res = StateAbs.solveOpts
              (options params)
              (AppOpt.givenAverageWithoutState state
                (Map.fromList (zip reqsAndDofs ss)) stateFlowGraph)
              ((StateAbs.withExpressionGraph $
                Fold.foldMap (makeEtaFuncGiven2 etaAssign etaFunc) .
                Map.lookup state . StateQty.states)
                <>  commonGiven params state stateFlowGraph)

      eta = case Params.etaToOptimise params of
                 Nothing -> StateEta.etaSys res
                 Just (TopoIdx.Position from to) ->
                      case StateQty.lookup
                             (StateIdx.eta state from to)
                             res of
                           Just e -> e
                           Nothing -> case StateQty.lookup
                                             (StateIdx.eta state to from)
                                             res of
                                           Just e -> e
                                           Nothing -> error "Optimise.solve: position not found"

  in Type.SweepPerReq
       eta
       (DoubleSweep.checkGreaterZeroNotNaN res)
       (toPowerMap res state)
       res

commonGiven ::
  (Node.C node, Ord node,
   Arith.Constant a, UV.Unbox a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a) =>
  Params.Optimisation node list sweep vec a ->
  Idx.State ->
  Type.EnvResult node (sweep vec a) ->
  Type.EqSystem node (sweep vec a)
commonGiven params state stateFlowGraph =
  case Map.lookup state $ State.states stateFlowGraph of
       Just _ -> StateIdx.dTime state StateAbs..=
                   Sweep.fromRational (Params.sweepLength params) Arith.one
       _ -> mempty

external ::
  (Eq (v a), Arith.Constant a, Vec.Zipper v,
   Vec.Storage v Bool, Arith.ZeroTestable a,
  Vec.Walker v, Vec.Singleton v, Vec.Storage v a, Node.C node, Show node) =>
  InitStorageState node a ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a)) ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
external (InitStorageState initSto) flowGraph =
  SeqAbs.solveOpts (SeqAbs.independentInOutSums SeqAbs.optionsDefault) flowGraph $
  Map.foldWithKey f mempty initSto
  where f st val = ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>)