{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation.NonIO where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Optimisation.Base as Base
import qualified Modules.System as System
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Types as Types

import Modules.Optimisation (EnvResult, external)
import Modules.System (Node)

import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Simulation as AppSim

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Record as SeqRec
import qualified EFA.Flow.Sequence.Index as SeqIdx
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateEqAbs

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Sequence as Sequ
import EFA.Signal.Data (Data(Data), Nil, (:>))


import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Monoid (mempty)

import Control.Functor.HT (for)


quasiStationaryOptimisation ::
  ModSet.Params System.Node [] Sweep UV.Vector Double ->
  Map Idx.State (Map [Double] (EnvResult (Sweep UV.Vector Double))) ->
  Types.QuasiStationary Sweep UV.Vector Double
quasiStationaryOptimisation params perStateSweep =
  let a = Base.optimalObjectivePerState params perStateSweep
      b = Base.selectOptimalState a
  in Types.QuasiStationary perStateSweep a b

simulation ::
  forall sweep vec list.
  (Arith.ZeroTestable (sweep vec Double),
   Arith.Product (sweep vec Double),
   Arith.Sum (sweep vec Double),
   Sweep.SweepVector vec Double,
   Sweep.SweepClass sweep vec Double,
   Sweep.SweepMap sweep vec Double Double) =>
  ModSet.Params System.Node list sweep vec Double ->
  Map (TopoIdx.Position Node) (Sig.PSignal2 Vector Vector Double) ->
  Record.PowerRecord Node Vector Double ->
  Types.Simulation sweep vec Double
simulation params dofsMatrices reqsRec =

  let (prest, plocal) =
        case map (Record.getSig reqsRec) (One.reqs params) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"


      dofsSignals :: Map (TopoIdx.Position Node) (Sig.PSignal Vector Double)
      dofsSignals =
        for dofsMatrices $ \mat ->
          Sig.tzipWith
            (Sig.interp2WingProfile "solveAndCalibrateAvgEffWithGraph"
              ModSet.varRestPower1D ModSet.varLocalPower mat)
            prest plocal

      givenSigs = Record.addSignals (Map.toList dofsSignals) reqsRec


      envSims :: TopoQty.Section Node (Result (Data (Vector :> Nil) Double))
      envSims =
        AppSim.solve
          (One.systemTopology params)
          System.etaAssignMap
          (One.etaMap params)
          givenSigs

      -- Liefert leider nur delta-Zeiten und keinen Zeitvektor
      -- Deshalb merken wir uns den urspruenglichen Zeitvektor
      recZeroCross =
        Chop.addZeroCrossings $ Base.convertRecord $
          (Base.envToPowerRecord envSims)
            { Record.recordTime = Record.recordTime givenSigs }

      (sequencePowers, _) =
        Base.filterPowerRecordList params $ Chop.genSequ recZeroCross


      sequenceFlowsFilt :: Sequ.List (Record.FlowRecord Node [] Double)
      sequenceFlowsFilt = fmap Record.partIntegrate sequencePowers

      sequenceFlowGraphSim ::
        SeqQty.Graph Node
          (Result (Data Nil Double)) (Result (Data ([] :> Nil) Double))
      sequenceFlowGraphSim =
        SeqAbs.solveOpts
          (SeqAbs.independentInOutSums SeqAbs.optionsDefault)
          (SeqRec.flowGraphFromSequence $
            fmap (TopoRecord.flowTopologyFromRecord System.topology) $
            sequenceFlowsFilt)
          (SeqIdx.storage Idx.initial System.Water .= ModSet.initStorageSeq)


      stateFlowGraphSim :: EnvResult (sweep vec Double)
      stateFlowGraphSim =
        StateEqAbs.solveOpts
          Optimisation.options
          (toSweep params $ StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external ModSet.initStorage sequenceFlowGraphSim)
          mempty


  in Types.Simulation stateFlowGraphSim sequenceFlowGraphSim recZeroCross

-- hier nochmal das sweep.map wegmachen!

toSweep ::
  (Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  One.OptimalEnvParams node f sweep vec a ->
  StateQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)) ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a))
toSweep params = StateQty.mapGraph f f
  where one = Sweep.fromRational (One.sweepLength params) Arith.one
        f (Determined (Data x)) = Determined $ Sweep.replicate one x
        f Undetermined = Undetermined


optimiseAndSimulate ::
  ModSet.Params System.Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Map Idx.State (Map [Double] (EnvResult (Sweep UV.Vector Double))) ->
  Types.Optimisation Sweep UV.Vector Double
optimiseAndSimulate params reqsRec perStateSweep =
  let optimalResult = quasiStationaryOptimisation params perStateSweep

      dofsMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices params (Types.optimalState optimalResult)
          (One.dofs params)

  in Types.Optimisation
       optimalResult
       (simulation params dofsMatrices reqsRec)
