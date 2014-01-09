{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Types where


import Modules.Optimisation (EnvResult)
import Modules.System (Node)

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import EFA.Equation.Result (Result)
import qualified EFA.Signal.Record as Record


import EFA.Signal.Data (Data, Nil, (:>))

import Data.Map (Map)

data QuasiStationary (sweep :: (* -> *) -> * -> *) vec a =
  QuasiStationary {
    perStateSweep ::
      Map Idx.State (Map [a] (EnvResult (sweep vec a))),

    optimalObjectivePerState ::
      Map Idx.State (Map [a] (Maybe (a, a, EnvResult a))),

    optimalState ::
      Map [a] (Maybe (a, a, Idx.State, EnvResult a)) }

data Simulation (sweep :: (* -> *) -> * -> *) vec a =
  Simulation {
    stateFlowGraph :: EnvResult (sweep vec a),
    sequenceFlowGraph ::
      SeqQty.Graph Node (Result (Data Nil a)) (Result (Data ([] :> Nil) a)),
    signals :: Record.PowerRecord Node [] a }

data Optimisation (sweep :: (* -> *) -> * -> *) vec a =
  Optimisation {
    quasiStationary :: QuasiStationary sweep vec a,
    simulation :: Simulation sweep vec a }
