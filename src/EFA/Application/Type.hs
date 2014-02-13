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
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Signal.Sequence as Sequ

import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))

--import Data.Vector (Vector)

import Data.Map (Map)



type EnvResult node a = StateQty.Graph node (Result a) (Result a)

type EqSystem node a =
  forall s. StateAbs.EquationSystemIgnore node s a a


type PerStateSweepVariable node sweep vec a = 
  Map Idx.State (Map node (Maybe ((sweep::(* -> *) -> * -> *) vec a)))

data PerStateSweep node (sweep :: (* -> *) -> * -> *) vec a =
  PerStateSweep {
    etaSys :: Result (sweep vec a),
    condVec :: Result (sweep vec Bool),
    storagePowerMap :: Map Idx.State (Map node (Maybe (sweep vec a))),
    envResult :: EnvResult node (sweep vec a) }

data BalOptimisation node (sweep :: (* -> *) -> * -> *) vec a = 
  PerStateOptimum {
    optimalObjectivePerState ::
      Map Idx.State (Map [a] (Maybe (a, a, EnvResult node a))),
    expectedEtaPerState ::
      Map Idx.State (Map [a] (Maybe a))}
    optimalState ::
      Map [a] (Maybe (a, a, Idx.State, EnvResult node a))}

data StatOptimisation node (sweep :: (* -> *) -> * -> *) vec a = 
  Optimum {
    stateOptimalState ::
      Map [a] (Maybe (a, a, Idx.State, EnvResult node a))}

data Simulation node vec a =
  Simulation {
--    stateFlowGraph :: EnvResult node (Data Nil a),
--    stateFlowGraphSweep :: EnvResult node (sweep vec a),
--    sequenceFlowGraph ::
--    SeqQty.Graph node (Result (Data Nil a)) (Result (Data ([] :> Nil) a)),
    givenSignals :: Record.PowerRecord node vec a,
    envSim:: TopoQty.Section node (Result (Data (vec :> Nil) a)),
    outSignals :: Record.PowerRecord node vec a }

-- | vec2 can be used to switch to vector after cutting
data EnergyFlowAnalysis node vec vec2 a = EnergyFlowAnalysis {
    signals :: Record.PowerRecord node vec a,
    powerSequence :: Sequ.List (Record.PowerRecord node vec2 a),
    sequenceFlowGraph ::
      SeqQty.Graph node (Result (Data Nil a)) (Result (Data (vec2 :> Nil) a)),
    stateFlowGraph :: EnvResult node (Data Nil a)}
{-
data Optimisation node (sweep :: (* -> *) -> * -> *) vec sigVec a =
  Optimisation {
    quasiStationary :: QuasiStationary node sweep vec a,
    simulation :: Simulation node sweep vec sigVec a }
-}