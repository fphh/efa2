{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Optimisation.NonIO where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Optimisation.Base as Base
--import qualified Modules.Utility as ModUt
--import qualified Modules.Setting as ModSet
import Modules.Optimisation (external)

import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs
import qualified EFA.Application.Type as Type
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Simulation as AppSim
--import EFA.Application.Sweep (Sweep)

import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Graph.Topology as Topology

import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.Topology as FlowTopo

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Record as SeqRec
import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateEqAbs
--import qualified EFA.Flow.State as FlowState

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Vector as SV

--import EFA.Report.FormatValue(formatValue)

import EFA.Signal.Data (Data(Data), Nil) --, (:>))


import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
--import qualified Data.Vector.Unboxed as UV
import Data.Monoid (Monoid, mempty, (<>))

{-
quasiStationaryOptimisation ::
  (Ord a, Ord (sweep UV.Vector a), UV.Unbox a,
   Monoid (sweep UV.Vector Bool),
   Arith.Product (sweep UV.Vector a),Show a,
   Arith.Constant a,
   Node.C node, Show node,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepMap sweep UV.Vector a Bool,
   Sweep.SweepMap sweep UV.Vector a a,
   Sweep.SweepClass sweep UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector a) =>
  One.OptimalEnvParams node [] sweep UV.Vector varVec sigVec a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Map Idx.State (Map [a] (Type.PerStateSweep node sweep UV.Vector a)) ->
  Type.QuasiStationary node sweep UV.Vector a

quasiStationaryOptimisation params stoForcings stateForcings perStateSweep =
  let a = Base.optimalObjectivePerState params stoForcings perStateSweep
      b = Base.expectedValuePerState perStateSweep
      c = Base.selectOptimalState stateForcings a
  in Type.QuasiStationary perStateSweep a b c
-}

interpolation :: 
  (a ~ b, a ~ c, 
   Show (sigVec b), 
   Node.C node, 
   Eq (sigVec b),
   Ord b,
   Show b,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec Bool,
   SV.Storage sigVec b,
   SV.Singleton sigVec,
   SV.Lookup sigVec,
   SV.Len (sigVec b),
   SV.Find sigVec,
   Arith.Constant b, 
   Show node, 
   SV.Storage intVec b, 
   SV.Convert intVec sigVec) =>
  One.OptimisationParams node list sweep sweepVec a ->
  One.SimulationParams node sigVec b -> 
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector intVec c) ->
  Record.PowerRecord node sigVec b
 
interpolation optParams simParams optimalPowerMatrices = 
  let (prest, plocal) =
        case map (Record.getSig reqsRec) (ReqsAndDofs.unReqs $ One.reqsPos optParams) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"
             
      reqsRec = One.reqsRec simParams

      dofsSignals =  Map.mapWithKey f optimalPowerMatrices
        where f key mat =
                Sig.tzipWith
                (Sig.interp2WingProfile
                 ("simulation-interpolate Signals" ++ show key) 
                 (One.varReqRoomPower1D simParams)
                 (One.varReqRoomPower2D simParams)
                 $ Sig.convert mat)
                prest
                plocal
                  
  in Record.addSignals (Map.toList dofsSignals) reqsRec


simulation ::
  (Ord a, Show a, 
   SV.Zipper vec, 
   SV.Walker vec, 
   SV.Storage vec Bool,
   SV.Storage vec a, 
   SV.Singleton vec, 
   SV.Len (vec a),
   SV.FromList vec, 
   Node.C node, 
   Arith.ZeroTestable a,
   Arith.Constant a) =>
  One.SystemParams node a ->
  Record.PowerRecord node vec a ->
  Type.Simulation node vec a
simulation sysParams givenSigs = Type.Simulation givenSigs envSim rec
  where
      envSim = 
        AppSim.solve
          (One.systemTopology sysParams)
          (One.etaAssignMap sysParams)
          (One.etaMap sysParams)
          givenSigs

      rec = Base.envToPowerRecord envSim
      

analysis :: 
  (Ord a, Show node, 
   Show a, Node.C node, 
   Arith.ZeroTestable a,
   Arith.Constant a) =>
  One.SystemParams node a ->
  Record.PowerRecord node [] a ->
  Type.EnergyFlowAnalysis node [] [] a
analysis sysParams powerRecord = 
      -- Liefert leider nur delta-Zeiten und keinen Zeitvektor
      -- Deshalb merken wir uns den urspruenglichen Zeitvektor
  let recZeroCross = 
        Chop.addZeroCrossings powerRecord

 {-     sequencePowerRecord = Sequ.mapWithSection (\ _ r ->  Base.convertRecord) $ 
                            Chop.genSequ recZeroCross-}

      sequencePowerRecord = Chop.genSequ recZeroCross

      (_, sequenceFlowsFilt) =
        Sequ.unzip $
        Sequ.filter (Record.major (Sig.toScalar Arith.zero) 
                                  (Sig.toScalar $ Arith.fromRational 0.2) . snd) $
        fmap (\x -> (x, Record.partIntegrate x)) sequencePowerRecord


      sequenceFlowGraph =
        SeqAbs.solveOpts
          (SeqAbs.independentInOutSums SeqAbs.optionsDefault)
          (SeqRec.flowGraphFromSequence $
            fmap (TopoRecord.flowTopologyFromRecord (One.systemTopology sysParams)) $
            sequenceFlowsFilt)
          (Map.foldWithKey
            (\st val -> ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>))
            mempty (One.unInitStorageSeq $ One.initStorageSeq sysParams))

      stateFlowGraph = 
        StateEqAbs.solveOpts
          Optimisation.optionsScalar
           (StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external (One.initStorageState sysParams) sequenceFlowGraph)
          mempty

{-
      stateFlowGraphSweep = 
        StateEqAbs.solveOpts
          Optimisation.options
          (toSweep params $ 
           StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external (One.initStorageState params) sequenceFlowGraphSim)
          mempty
-}

  in Type.EnergyFlowAnalysis powerRecord sequencePowerRecord sequenceFlowGraph stateFlowGraph 


toSweep ::
  (Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  One.OptimisationParams node list sweep vec a ->
  StateQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)) ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a))
toSweep params = StateQty.mapGraph f f
  where one = Sweep.fromRational (One.sweepLength params) Arith.one
        f (Determined (Data x)) = Determined $ Sweep.replicate one x
        f Undetermined = Undetermined

{-
optimiseAndSimulate ::
  (Show (sigVec a),
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec Bool,
   SV.Storage varVec a,
   SV.Singleton sigVec,
   SV.Len (sigVec a),
   SV.FromList sigVec,
   SV.Find varVec,
   SV.Convert varVec varVec, 
   SV.Storage varVec (Maybe (Result a)),
   Eq (varVec a),
   SV.Lookup varVec,
   SV.Find sigVec,
   SV.Convert sigVec sigVec,
   Show (varVec a),
   SV.Zipper varVec,
   SV.Walker varVec,
   SV.Storage sigVec a,
   SV.Storage varVec Bool,
   SV.Singleton varVec,
   SV.Len (varVec a),
   SV.Convert sigVec [],
   SV.Convert sigVec UV.Vector,
   SV.FromList varVec, 
   Fractional a, Arith.ZeroTestable a,
   Show node, Node.C node,Ord a, 
   Show a, UV.Unbox a, 
   Arith.Constant a,Arith.Product a,Arith.Sum a,
   Ord (Sweep UV.Vector a),
   Monoid (Sweep UV.Vector Bool),
   Show (UV.Vector a),
   Arith.ZeroTestable (Sweep UV.Vector a),
   Arith.Product (Sweep UV.Vector a),
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepMap Sweep UV.Vector a a,
   Sweep.SweepMap Sweep UV.Vector a Bool,
   Sweep.SweepClass Sweep UV.Vector a,
   Sweep.SweepClass Sweep UV.Vector Bool) => 
  One.OptimisationParams node Sweep UV.Vector a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Map Idx.State (Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Type.Optimisation node Sweep UV.Vector sigVec a
  
optimiseAndSimulate params stoForcing stateForcing perStateSweep =
  let optimalResult = quasiStationaryOptimisation params stoForcing stateForcing perStateSweep

      reqsRec = One.reqsRec params

      dofsMatrices = 
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            params
            (Type.optimalState optimalResult)
            (One.dofsPos params)

  in Type.Optimisation
       optimalResult
       (simulation params dofsMatrices reqsRec)


optimiseStatesAndSimulate ::
  (Ord a, Show a, Arith.Sum a,
   Arith.Constant a, Ord node,
   Eq (sigVec a),
   SV.Lookup sigVec,
   SV.Find sigVec,
   SV.Convert sigVec sigVec,
   Show node,
   SV.Storage sigVec (Maybe (Result a)),
   SV.FromList sigVec,Fractional a,
   Show (sigVec a),
   UV.Unbox a,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec a,
   SV.Storage sigVec Bool,
   SV.Storage vec a,
   SV.Storage varVec (Maybe (Result a)),
   SV.FromList varVec,
   SV.Singleton sigVec,
   SV.Len (sigVec a),
   SV.Convert sigVec [],
   SV.Convert sigVec UV.Vector,
   SV.Convert vec UV.Vector,
   Node.C node,
   Arith.ZeroTestable (sweep vec a),
   Arith.ZeroTestable a,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepMap sweep vec a a,
   Eq (varVec a),
   Show (varVec a),
   SV.Zipper varVec,
   SV.Walker varVec,
   SV.Storage varVec a,
   SV.Storage varVec Bool,
   SV.Singleton varVec,
   SV.Lookup varVec,
   SV.Len (varVec a),
   SV.Find varVec,
   SV.Convert varVec varVec,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node [] sweep vec varVec sigVec a ->
  Map Idx.State (One.StateForcing a) ->
  Map Idx.State (Map [a] (Maybe (a, a, Type.EnvResult node a))) ->
  -- Map [a] (Maybe (a, a, Idx.State, Type.EnvResult node a))
  Type.Simulation node sweep vec sigVec a
optimiseStatesAndSimulate params stateForcing optStates = 
  let optimalState = Base.selectOptimalState stateForcing optStates
      reqsRec = One.reqsRec params

      dofsMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            params
            optimalState
            (One.dofsPos params)

  in simulation params dofsMatrices reqsRec
  
-}