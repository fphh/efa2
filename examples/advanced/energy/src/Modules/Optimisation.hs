{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation where

import qualified Modules.System as System
import qualified Modules.Setting as ModSet


import qualified EFA.Application.DoubleSweep as DoubleSweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Simulation (EtaAssignMap, Name, makeEtaFuncGiven2)

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



--type EnvData a = StateQty.Graph System.Node (Data Nil a) (Data Nil a)
type EnvResult a = StateQty.Graph System.Node (Result a) (Result a)

--type EnvResultData a = EnvResult (Data Nil a)

type EqSystem a =
  forall s. StateAbs.EquationSystemIgnore System.Node s a a

idxParams :: DoubleSweep.Pair [] [] (TopoIdx.Power System.Node)
idxParams = DoubleSweep.Pair  idxLoad idxDOF

idxLoad :: [TopoIdx.Power System.Node]
idxLoad =
   TopoIdx.power System.LocalRest System.LocalNetwork :
   TopoIdx.power System.Rest System.Network :
   []

idxDOF :: [TopoIdx.Power System.Node]
idxDOF =
   TopoIdx.power System.Network System.Water :
   TopoIdx.power System.LocalNetwork System.Gas :
   []


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
  (Show a, Ord a, UV.Unbox a, Arith.Constant a,
   Sweep.SweepMap sweep vec a a, Arith.Sum (sweep vec a),
   Sweep.SweepVector vec a, Sweep.SweepClass sweep vec a,
   Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a)) =>
  EnvResult (sweep vec a) ->
  EtaAssignMap System.Node ->
  Map Name (a -> a) ->
  Idx.State ->
  DoubleSweep.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a ->
  EnvResult (sweep vec a)
solve stateFlowGraph etaAssign etaFunc state params =
  let ss = Sweep.unList (DoubleSweep.fstRecord params)
             ++ Sweep.unList (DoubleSweep.sndRecord params)
      vs = idxLoad ++ idxDOF
  in StateAbs.solveOpts
      options
      (AppOpt.givenAverageWithoutState state
          (Map.fromList (zip vs ss))
          stateFlowGraph)
      ((StateAbs.withExpressionGraph $
           Fold.foldMap (makeEtaFuncGiven2 etaAssign etaFunc) .
           Map.lookup state . StateQty.states)
       <>  commonGiven state stateFlowGraph)

-- Hier sollte die customOne eigentlich nicht verwendet werden!
commonGiven ::
  (Arith.Constant a, UV.Unbox a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a) =>
  Idx.State ->
  EnvResult (sweep vec a) ->
  EqSystem (sweep vec a)
commonGiven state stateFlowGraph =
  case Map.lookup state $ State.states stateFlowGraph of
       Just _ -> XIdx.dTime state StateAbs..=
                   Sweep.fromRational ModSet.sweepLength Arith.one
       _ -> mempty

external ::
  (Eq (v a), Arith.Constant a, Vec.Zipper v,
   Vec.Storage v Bool, Arith.ZeroTestable a,
  Vec.Walker v, Vec.Singleton v, Vec.Storage v a, Node.C node) =>
  [(node, a)] ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a)) ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))

external initSto flowGraph =
  SeqAbs.solveOpts (SeqAbs.independentInOutSums SeqAbs.optionsDefault) flowGraph $
  Fold.foldMap f initSto
  where f (st, val) =
          SeqIdx.storage Idx.initial st SeqAbs..= Data val
