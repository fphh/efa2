{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module EFA.Application.Type where

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateAbs


import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))

import Data.Vector (Vector)

import Data.Map (Map)



type EnvResult node a = StateQty.Graph node (Result a) (Result a)

type EqSystem node a =
  forall s. StateAbs.EquationSystemIgnore node s a a


data PerStateSweep node (sweep :: (* -> *) -> * -> *) vec a =
  PerStateSweep {
    etaSys :: Result (sweep vec a),
    condVec :: Result (sweep vec Bool),
    storagePowerMap :: Map Idx.State (Map node (Maybe (sweep vec a))),
    envResult :: EnvResult node (sweep vec a) }


data QuasiStationary node (sweep :: (* -> *) -> * -> *) vec a =
  QuasiStationary {
    perStateSweep ::
      Map Idx.State (Map [a] (PerStateSweep node sweep vec a)),

    optimalObjectivePerState ::
      Map Idx.State (Map [a] (Maybe (a, a, EnvResult node a))),

    expectedEtaPerState ::
      Map Idx.State (Map [a] (Maybe a)),

    optimalState ::
      Map [a] (Maybe (a, a, Idx.State, EnvResult node a)) }

data Simulation node (sweep :: (* -> *) -> * -> *) vec a =
  Simulation {
    stateFlowGraph :: EnvResult node (sweep vec a),
    sequenceFlowGraph ::
      SeqQty.Graph node (Result (Data Nil a)) (Result (Data ([] :> Nil) a)),

    givenSignals :: Record.PowerRecord node Vector a,

    signals :: Record.PowerRecord node [] a }

data Optimisation node (sweep :: (* -> *) -> * -> *) vec a =
  Optimisation {
    quasiStationary :: QuasiStationary node sweep vec a,
    simulation :: Simulation node sweep vec a }
