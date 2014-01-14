{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}


module Modules.Types where

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateAbs

import EFA.Equation.Result (Result)
import qualified EFA.Signal.Record as Record


import EFA.Signal.Data (Data, Nil, (:>))

import Data.Map (Map)

type EnvResult node a = StateQty.Graph node (Result a) (Result a)

type EqSystem node a =
  forall s. StateAbs.EquationSystemIgnore node s a a


data QuasiStationary node (sweep :: (* -> *) -> * -> *) vec a =
  QuasiStationary {
    perStateSweep ::
      Map Idx.State (Map [a] (EnvResult node (sweep vec a))),

    optimalObjectivePerState ::
      Map Idx.State (Map [a] (Maybe (a, a, EnvResult node a))),

    optimalState ::
      Map [a] (Maybe (a, a, Idx.State, EnvResult node a)) }

data Simulation node (sweep :: (* -> *) -> * -> *) vec a =
  Simulation {
    stateFlowGraph :: EnvResult node (sweep vec a),
    sequenceFlowGraph ::
      SeqQty.Graph node (Result (Data Nil a)) (Result (Data ([] :> Nil) a)),
    signals :: Record.PowerRecord node [] a }

data Optimisation node (sweep :: (* -> *) -> * -> *) vec a =
  Optimisation {
    quasiStationary :: QuasiStationary node sweep vec a,
    simulation :: Simulation node sweep vec a }
