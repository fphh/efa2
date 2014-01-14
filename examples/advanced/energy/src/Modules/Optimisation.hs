{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation where

import qualified Modules.System as System
import qualified Modules.Setting as ModSet
import qualified Modules.Types as Types


import qualified EFA.Application.DoubleSweep as DoubleSweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.OneStorage
  (EtaAssignMap, Name, InitStorageState(InitStorageState))
import EFA.Application.Simulation (makeEtaFuncGiven2)

import qualified EFA.Flow.State as State
import qualified EFA.Flow.State.Absolute as StateAbs
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Index as XIdx

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Vector as Vec
import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified EFA.Graph.Topology.Node as Node

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.Vector.Unboxed as UV

import Data.Monoid ((<>), mempty)

options ::
  (UV.Unbox a, Arith.Constant a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Arith.Sum (sweep vec a),
   Verify.LocalVar mode (sweep vec a)) =>
  StateAbs.Options mode rec (sweep vec a) (sweep vec a)
options =
  StateAbs.optionsBase
    SeqStateEqSys.equalStInOutSums
    (StorageEqSys.customOne (Sweep.fromRational ModSet.sweepLength Arith.one))



solve ::
  (Node.C node, Ord node,
   Show a, Ord a, UV.Unbox a, Arith.Constant a,
   Sweep.SweepMap sweep vec a a, Arith.Sum (sweep vec a),
   Sweep.SweepVector vec a, Sweep.SweepClass sweep vec a,
   Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a)) =>
  [TopoIdx.Power node] ->
  Types.EnvResult node (sweep vec a) ->
  EtaAssignMap node ->
  Map Name (a -> a) ->
  Idx.State ->
  DoubleSweep.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a ->
  Types.EnvResult node (sweep vec a)
solve reqsAndDofs stateFlowGraph etaAssign etaFunc state params =
  let ss = Sweep.unList (DoubleSweep.fstRecord params)
             ++ Sweep.unList (DoubleSweep.sndRecord params)
  in StateAbs.solveOpts
      options
      (AppOpt.givenAverageWithoutState state
          (Map.fromList (zip reqsAndDofs ss))
          stateFlowGraph)
      ((StateAbs.withExpressionGraph $
           Fold.foldMap (makeEtaFuncGiven2 etaAssign etaFunc) .
           Map.lookup state . StateQty.states)
       <>  commonGiven state stateFlowGraph)

commonGiven ::
  (Node.C node, Ord node,
   Arith.Constant a, UV.Unbox a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a) =>
  Idx.State ->
  Types.EnvResult node (sweep vec a) ->
  Types.EqSystem node (sweep vec a)
commonGiven state stateFlowGraph =
  case Map.lookup state $ State.states stateFlowGraph of
       Just _ -> XIdx.dTime state StateAbs..=
                   Sweep.fromRational ModSet.sweepLength Arith.one
       _ -> mempty

external ::
  (Eq (v a), Arith.Constant a, Vec.Zipper v,
   Vec.Storage v Bool, Arith.ZeroTestable a,
  Vec.Walker v, Vec.Singleton v, Vec.Storage v a, Node.C node) =>
  InitStorageState node a ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a)) ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
external (InitStorageState initSto) flowGraph =
  SeqAbs.solveOpts (SeqAbs.independentInOutSums SeqAbs.optionsDefault) flowGraph $
  Map.foldWithKey f mempty initSto
  where f st val = ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>)