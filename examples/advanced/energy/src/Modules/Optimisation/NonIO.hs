{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Optimisation.NonIO where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Utility as ModUt
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
--import qualified EFA.Flow.Topology.Index as TopoIdx
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

import qualified Data.Map as Map; --import Data.Map (Map)
--import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Monoid (Monoid, mempty, (<>))


interpolateOptimalSolution ::
  (Eq (vec2 b), Ord b, Show b, Show (vec2 b),
   Show node, SV.Zipper vec2, SV.Walker vec2,
   SV.Storage vec2 Bool, SV.Storage vec2 b,
   SV.Storage vec2 (Maybe (Result b)),
   SV.Singleton vec2, SV.Lookup vec2, SV.Len (vec2 b),
   SV.FromList vec2, SV.Find vec2, SV.Convert vec2 vec2,
   Node.C node, Arith.Constant b) =>
  One.SystemParams node b->
  One.OptimisationParams node list sweep vec a->
  One.SimulationParams node vec2 b->
  Map.Map[b](Maybe(b, b, Idx.State, Type.EnvResult node b))->
  Type.Interpolation node vec2 b
interpolateOptimalSolution sysParams optParams simParams optimalSolution =
  let (prest, plocal) =
        case map (Record.getSig demandSignals) (ReqsAndDofs.unReqs $ One.reqsPos optParams) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"

      demandSignals = One.reqsRec simParams

      dofsSignals =  Map.mapWithKey f optimalControlMatrices
        where f key mat =
                Sig.tzipWith
                (Sig.interp2WingProfile
                 ("simulation-interpolate Signals" ++ show key)
                 (One.varReqRoomPower1D simParams)
                 (One.varReqRoomPower2D simParams)
                 $ Sig.convert mat)
                prest
                plocal

      optimalControlMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            sysParams
            optimalSolution
            (One.dofsPos optParams)

      demandAndControlSignals = Record.addSignals (Map.toList dofsSignals) demandSignals

  in Type.Interpolation optimalControlMatrices demandAndControlSignals


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
simulation sysParams givenSigs = Type.Simulation envSim rec
  where
      envSim =
        AppSim.solve
          (One.systemTopology sysParams)
          (One.etaAssignMap sysParams)
          (One.etaMap sysParams)
          givenSigs

      rec = (Base.envToPowerRecord envSim)
              { Record.recordTime = Record.recordTime givenSigs }


energyFlowAnalysis ::
  (Ord a, Show node, SV.Convert [] vec,
   Eq (vec a), SV.Storage vec Bool,
   SV.Storage vec a, SV.Convert vec [],
   SV.Walker vec, SV.Singleton vec,
   SV.Zipper vec,
   Show a, Node.C node,
   Arith.ZeroTestable a,
   Arith.Constant a) =>
  One.SystemParams node a ->
  One.SimulationParams node vec a ->
  Record.PowerRecord node vec a ->
  Type.EnergyFlowAnalysis node vec a
energyFlowAnalysis sysParams simParams powerRecord =
      -- Liefert nur delta-Zeiten und keinen Zeitvektor
      -- Deshalb wird der urspruenglichen Zeitvektor behalten
  let recZeroCross =
        Chop.addZeroCrossings $ Base.convertRecord powerRecord

      sequencePowerRecord = Sequ.mapWithSection (\ _ r ->  Base.convertRecord r) 
                            $ Chop.genSequ recZeroCross
                            
      thrT = One.sequFilterTime simParams
      thrE = One.sequFilterEnergy simParams

      (_, sequenceFlowsFilt) =
        Sequ.unzip $
        Sequ.filter (Record.major (Sig.toScalar thrE)
                                  (Sig.toScalar thrT) . snd) $
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

  in Type.EnergyFlowAnalysis sequencePowerRecord sequenceFlowGraph stateFlowGraph


toSweep ::
  (Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  One.OptimisationParams node list sweep vec a ->
  StateQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)) ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a))
toSweep params = StateQty.mapGraph f f
  where one = Sweep.fromRational (One.sweepLength params) Arith.one
        f (Determined (Data x)) = Determined $ Sweep.replicate one x
        f Undetermined = Undetermined

optimiseAndSimulate :: (efaVec ~ simVec,intVec ~ simVec,a ~ d,intVec ~ [],
  Eq (simVec d),
  RealFloat d,
  Show (simVec d),
  SV.Zipper simVec,
  SV.Walker simVec,
  SV.Storage simVec (Maybe (Result d)),
  SV.Storage simVec d,
  SV.Storage simVec Bool,
  SV.Singleton simVec,
  SV.Lookup simVec,
  SV.Len (simVec d),
  SV.FromList simVec,
  SV.Find simVec,
  SV.Convert simVec simVec,
  SV.Convert simVec [], SV.Convert [] simVec ,
  Ord (sweep UV.Vector a), Ord a, Show a, Show node,
   Monoid (sweep UV.Vector Bool), UV.Unbox a, Node.C node,
   Arith.ZeroTestable a, Arith.Product (sweep UV.Vector a),
   Arith.Constant a, Sweep.SweepClass sweep UV.Vector (a, a),
   Sweep.SweepClass sweep UV.Vector (a, Bool),
   Sweep.SweepClass sweep UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector a) =>
  One.SystemParams node a
  -> One.OptimisationParams node [] sweep UV.Vector a
  -> One.SimulationParams node intVec a
  -> Map.Map node (One.SocDrive a)
  -> Map.Map Idx.AbsoluteState (One.StateForcing a)
  -> Map.Map
  Idx.State
  (Map.Map [a] (Type.SweepPerReq node sweep UV.Vector a))
  -> One.IndexConversionMap
  -> (Type.OptimisationPerState node a,
      Type.OptimiseStateAndSimulate node sweep UV.Vector a intVec b simVec c efaVec d)
optimiseAndSimulate sysParams optParams simParams balanceForcing stateForcing perStateSweep indexConversionMap =
  let perStateOptimum  = Base.optimalObjectivePerState optParams balanceForcing perStateSweep
      perStateAverage = Base.expectedValuePerState perStateSweep
      optimalSolution = Base.selectOptimalState optParams stateForcing perStateOptimum indexConversionMap
      interpolation = interpolateOptimalSolution sysParams optParams simParams optimalSolution
      sim = simulation sysParams $ Type.reqsAndDofsSignals interpolation
      efa = energyFlowAnalysis sysParams simParams $ Type.signals sim
      sfgSweep = toSweep optParams $ Type.stateFlowGraph efa

  in (Type.OptimisationPerState perStateOptimum perStateAverage,
     Type.OptimiseStateAndSimulate optimalSolution interpolation sim efa sfgSweep)


optimiseStateAndSimulate:: (d ~ a, intVec ~ [], simVec ~ [], b ~ a, efaVec ~ [],RealFloat a) =>
  (Ord b, Show node, Show b, Node.C node,Sweep.SweepClass sweep vec a,
   Arith.ZeroTestable b, Arith.Constant b) =>
  One.SystemParams node b
  -> One.OptimisationParams node list sweep vec a
  -> One.SimulationParams node [] b
  -> Map.Map Idx.AbsoluteState (One.StateForcing b)
  -> Map.Map
  Idx.State
  (Map.Map
   [b] (Maybe (b, b, Type.EnvResult node b)))
  -> One.IndexConversionMap
  -> Type.OptimiseStateAndSimulate node sweep vec a intVec b simVec c efaVec d
optimiseStateAndSimulate sysParams optParams simParams stateForcing perStateOptimum indexConversionMap =
  let optimalSolution = Base.selectOptimalState optParams stateForcing perStateOptimum indexConversionMap
      interpolation = interpolateOptimalSolution sysParams optParams simParams optimalSolution
      sim = simulation sysParams  $ Type.reqsAndDofsSignals interpolation
      efa = energyFlowAnalysis sysParams simParams $ Type.signals sim
      sfgSweep = toSweep optParams $ Type.stateFlowGraph efa
  in  Type.OptimiseStateAndSimulate optimalSolution interpolation sim efa sfgSweep

