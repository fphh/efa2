{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation.NonIO where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Optimisation.Base as Base

import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Types as Types

import Modules.Optimisation (external)

import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Simulation as AppSim
import EFA.Application.Sweep (Sweep)

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Record as SeqRec
import qualified EFA.Flow.Sequence.Index as SeqIdx

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
import Data.Monoid (Monoid, mempty, (<>))

import Control.Functor.HT (for)


quasiStationaryOptimisation ::
  (Show node, Node.C node,
   Ord a, Show a, UV.Unbox a, Arith.Constant a, Fractional a,
   Ord (sweep vec a),
   Monoid (sweep vec Bool),
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepVector vec Bool,
   Sweep.SweepMap sweep vec a a,
   Sweep.SweepMap sweep vec a Bool,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepClass sweep vec Bool) =>
  One.OptimalEnvParams node [] sweep vec a ->
  Map Idx.State (Map [a] (Types.EnvResult node (sweep vec a))) ->
  Types.QuasiStationary node sweep vec a
quasiStationaryOptimisation params perStateSweep =
  let a = Base.optimalObjectivePerState params perStateSweep
      b = Base.expectedValuePerState perStateSweep
      c = Base.selectOptimalState a
  in Types.QuasiStationary perStateSweep a b c

simulation ::
  forall node sweep vec list.
  (Node.C node, Show node, Ord node,
   Arith.ZeroTestable (sweep vec Double),
   Arith.Product (sweep vec Double),
   Arith.Sum (sweep vec Double),
   Sweep.SweepVector vec Double,
   Sweep.SweepClass sweep vec Double,
   Sweep.SweepMap sweep vec Double Double) =>
  One.OptimalEnvParams node list sweep vec Double ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector Vector Double) ->
  Record.PowerRecord node Vector Double ->
  Types.Simulation node sweep vec Double
simulation params dofsMatrices reqsRec =
  let (prest, plocal) =
        case map (Record.getSig reqsRec) (ReqsAndDofs.unReqs $ One.reqsPos params) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"


      dofsSignals :: Map (TopoIdx.Position node) (Sig.PSignal Vector Double)
      dofsSignals = Map.mapWithKey f dofsMatrices
        where f key mat = Sig.tzipWith (Sig.interp2WingProfile ("simulation-interpolate Signals" ++ show key) 
                                        ModSet.varRestPower1D ModSet.varLocalPower mat) prest plocal
{-        for dofsMatrices $ \mat ->
          Sig.tzipWith
            (Sig.interp2WingProfile "simulation-interolateSignals"
              ModSet.varRestPower1D ModSet.varLocalPower mat)
            prest plocal -}

      givenSigs = Record.addSignals (Map.toList dofsSignals) reqsRec


      envSims :: TopoQty.Section node (Result (Data (Vector :> Nil) Double))
      envSims =
        AppSim.solve
          (One.systemTopology params)
          (One.etaAssignMap params)
          (One.etaMap params)
          givenSigs

      -- Liefert leider nur delta-Zeiten und keinen Zeitvektor
      -- Deshalb merken wir uns den urspruenglichen Zeitvektor
      recZeroCross =
        Chop.addZeroCrossings $ Base.convertRecord $
          (Base.envToPowerRecord envSims)
            { Record.recordTime = Record.recordTime givenSigs }


      -- sequencePowers :: Sequ.List (PowerRecord System.Node [] Double)
      sequencePowers = Chop.genSequ recZeroCross

      (_, sequenceFlowsFilt) =
        Sequ.unzip $
        Sequ.filter (Record.major (Sig.toScalar 0) (Sig.toScalar 0.2) . snd) $
        fmap (\x -> (x, Record.partIntegrate x)) sequencePowers


      sequenceFlowGraphSim ::
        SeqQty.Graph node
          (Result (Data Nil Double)) (Result (Data ([] :> Nil) Double))
      sequenceFlowGraphSim =
        SeqAbs.solveOpts
          (SeqAbs.independentInOutSums SeqAbs.optionsDefault)
          (SeqRec.flowGraphFromSequence $
            fmap (TopoRecord.flowTopologyFromRecord (One.systemTopology params)) $
            sequenceFlowsFilt)
          (Map.foldWithKey
            (\st val -> ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>))
            mempty (One.unInitStorageSeq $ One.initStorageSeq params))

      stateFlowGraphSim :: Types.EnvResult node (sweep vec Double)
      stateFlowGraphSim =
        StateEqAbs.solveOpts
          Optimisation.options
          (toSweep params $ StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external (One.initStorageState params) sequenceFlowGraphSim)
          mempty


  in Types.Simulation stateFlowGraphSim sequenceFlowGraphSim givenSigs recZeroCross


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
  (Show node, Node.C node) =>
  One.OptimalEnvParams node [] Sweep UV.Vector Double ->
  Record.PowerRecord node Vector Double ->
  Map Idx.State (Map [Double] (Types.EnvResult node (Sweep UV.Vector Double))) ->
  Types.Optimisation node Sweep UV.Vector Double
optimiseAndSimulate params reqsRec perStateSweep =
  let optimalResult = quasiStationaryOptimisation params perStateSweep

      dofsMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices params (Types.optimalState optimalResult)
          (One.dofsPos params)

  in Types.Optimisation
       optimalResult
       (simulation params dofsMatrices reqsRec)
