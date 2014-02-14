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
import qualified EFA.Flow.Topology.Index as TopoIdx

import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data, Nil, (:>))

import Data.Vector (Vector)

import Data.Map (Map)



type EnvResult node a = StateQty.Graph node (Result a) (Result a)


type PerStateSweepVariable node sweep vec a = 
  Map Idx.State (Map node (Maybe ((sweep::(* -> *) -> * -> *) vec a)))

type OptimalSolutionPerState node a = 
  Map Idx.State (Map [a] (Maybe (a, a, EnvResult node a)))
                     --Map [a] (Maybe (a, a, Idx.State, EnvResult node a))

type AverageSolutionPerState node a = Map Idx.State (Map [a] (Maybe a))
  --Map [a] (Maybe (a, a, Idx.State, EnvResult node a))

type OptimalSolution node a = Map [a] (Maybe (a, a, Idx.State, EnvResult node a))

type ControlMatrices node vec a = Map (TopoIdx.Position node) (Sig.PSignal2 Vector vec a)

type EqSystem node a =
  forall s. StateAbs.EquationSystemIgnore node s a a


data Sweep node sweep vec a = Sweep {
    sweepData :: Map Idx.State (Map [a] (PerStateSweep node sweep vec a)),
    sweepStoragePower :: Map Idx.State (Map node (Maybe (sweep vec a)))}

data PerStateSweep node (sweep :: (* -> *) -> * -> *) vec a =
  PerStateSweep {
    etaSys :: Result (sweep vec a),
    condVec :: Result (sweep vec Bool),
    storagePowerMap :: Map Idx.State (Map node (Maybe (sweep vec a))),
    envResult :: EnvResult node (sweep vec a) }

data Interpolation node vec a = 
  Interpolation {
    controlMatrices :: ControlMatrices node vec a,
    demandAndControlSignals :: Record.PowerRecord node vec a}

data Simulation node vec a =
  Simulation {
    envSim:: TopoQty.Section node (Result (Data (vec :> Nil) a)),
    signals :: Record.PowerRecord node vec a }

data EnergyFlowAnalysis node vec a = EnergyFlowAnalysis {
    powerSequence :: Sequ.List (Record.PowerRecord node vec a),
    sequenceFlowGraph :: --SeqQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)),
      SeqQty.Graph node (Result (Data Nil a)) (Result (Data (vec :> Nil) a)),
    stateFlowGraph :: EnvResult node (Data Nil a)}

data OptimisationPerState node a = OptimisePerState {
    optimalSolutionPerState :: OptimalSolutionPerState node a,
    averageSolutionPerState :: AverageSolutionPerState node a}

data OptimiseStateAndSimulate node (sweep :: (* -> *) -> * -> *) 
     sweepVec a intVec b simVec c efaVec d = 
  OptimiseStateAndSimulate {
    optimalSolution :: OptimalSolution node a,
    interpolation :: Interpolation node intVec a,
    simulation :: Simulation node simVec a,
    analysis :: EnergyFlowAnalysis node efaVec d, 
    stateFlowGraphSweep :: EnvResult node (sweep sweepVec a)}


