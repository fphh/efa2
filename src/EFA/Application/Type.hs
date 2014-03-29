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


-- type PerStateSweepVariable node sweep vec a =
--  Map Idx.State (Map node (Maybe ((sweep::(* -> *) -> * -> *) vec a)))

-- | Data Type to store the optimal solution per state
type OptimalSolutionPerState node a =
  Map Idx.State (Map [a] (Maybe (a, a, Int, EnvResult node a)))

type OptimalSolutionOfOneState node a = Map [a] (Maybe (a, a, Int, EnvResult node a))

-- | Data Type to store the optimal solution of all states
type OptimalSolution node a = Map [a] (Maybe (a, a, Idx.State, Int, EnvResult node a))

-- | Data Type to store the stack of the optimal abjective Function per state
type OptStackPerState (sweep :: (* -> *) -> * -> *) vec a =  Map Idx.State (Map [a] (Result (sweep vec a)))

-- | Data Type to store the average solution per state
type AverageSolutionPerState node a = Map Idx.State (Map [a] (Maybe a))


type ControlMatrices node vec a = Map (TopoIdx.Position node) (Sig.PSignal2 Vector vec a)

type EqSystem node a =
  forall s. StateAbs.EquationSystemIgnore node s a a

-- | Complete Sweep over all States and complete Requirement Room
type Sweep node sweep vec a = Map Idx.State (Map [a] (SweepPerReq node sweep vec a))

{-
data Sweep node sweep vec a = Sweep {
    sweepData :: Map Idx.State (Map [a] (PerStateSweep node sweep vec a)),
    sweepStoragePower :: Map Idx.State (Map node (Maybe (sweep vec a)))}
-}

type StoragePowerMap node (sweep :: (* -> *) -> * -> *) vec a = Map node (Maybe (Result (sweep vec a)))

-- | Sweep over one Position in Requirement Room
data SweepPerReq node (sweep :: (* -> *) -> * -> *) vec a =
  SweepPerReq {
    etaSys :: Result (sweep vec a),
    condVec :: Result (sweep vec Bool),
    storagePowerMap :: StoragePowerMap node sweep vec a,
    envResult :: EnvResult node (sweep vec a) }


instance Show (SweepPerReq node sweep vec a) where
  show _ = "<SweepPerReq>"

{-
data Interpolation node vec a =
  Interpolation {
    controlMatrices :: ControlMatrices node vec a,
    reqsAndDofsSignals :: Record.PowerRecord node vec a}
-}


type InterpolationOfAllStates node vec a =
  Map Idx.State (InterpolationOfOneState node vec a)

data InterpolationOfOneState node vec a =
  InterpolationOfOneState {
    controlMatricesOfState :: ControlMatrices node vec a,
    optObjectiveSignalOfState :: Sig.UTSignal vec a,
--    tileChangeSignal :: vec [(a,a,a,a)],
    reqsAndDofsSignalsOfState :: Record.PowerRecord node vec a}


data Simulation node vec a =
  Simulation {
    envSim:: TopoQty.Section node (Result (Data (vec :> Nil) a)),
    signals :: Record.PowerRecord node vec a }

data EnergyFlowAnalysis node vec a = EnergyFlowAnalysis {
    powerSequence :: Sequ.List (Record.PowerRecord node vec a),
    sequenceFlowGraph ::
      SeqQty.Graph node (Result (Data Nil a)) (Result (Data (vec :> Nil) a)),
    stateFlowGraph :: EnvResult node (Data Nil a)}

data SignalBasedOptimisation node (sweep :: (* -> *) -> * -> *)
     sweepVec a intVec b simVec c efaVec d =
  SignalBasedOptimisation {
    optimalSolutionPerState :: OptimalSolutionPerState node a,
    averageSolutionPerState :: AverageSolutionPerState node a,
    interpolationPerState :: InterpolationOfAllStates node intVec a,
--    optimalSolutionSig :: OptimalSolution node a,
    simulation :: Simulation node simVec a,
    analysis :: EnergyFlowAnalysis node efaVec d,
    stateFlowGraphSweep :: EnvResult node (sweep sweepVec a)}


instance Show (SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d) where
  show _ = "<SignalBasedOptimisation>"