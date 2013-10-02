{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Modules.Analysis where

import qualified Modules.System as System
import qualified Modules.Signals as Signals

import qualified EFA.Application.Absolute as EqAbs
import EFA.Application.Absolute ((.=))

import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as Env
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Base as B
import EFA.Signal.Data (Data(Data), Nil, (:>))
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import EFA.Signal.Record (SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord, getTime, newTimeBase)

import EFA.Signal.Chop (addZeroCrossings, genSequ)

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Foldable (fold)


-------------------------------------------------------------------------------------------------
-- ## Preprocessing of Signals

pre ::
   Topo.Topology System.Node ->
   Double ->
   Sig.Scal (Typ A T Tt) Double ->
   Sig.Scal (Typ A F Tt) Double ->
   SignalRecord [] Double ->
   (Sequ.List (PowerRecord System.Node [] Double),
    Sequ.List (FlowRecord System.Node [] Double),
    Sequ.List (Topo.FlowTopology System.Node),
    PowerRecord System.Node [] Double,
    SignalRecord [] Double)

pre topology epsZero epsT epsE rawSignals =
  (sequencePowersFilt, adjustedFlows, flowTopos, powerSignals0, signals0)
  where
    ---------------------------------------------------------------------------------------
    -- * Condition Signals, Calculate Powers, Remove ZeroNoise

    signals = Signals.condition rawSignals
    powerSignals = Record.removeZeroNoise epsZero (Signals.calculatePower signals)

    ---------------------------------------------------------------------------------------
    -- * Add zerocrossings in Powersignals and Signals

    powerSignals0 = addZeroCrossings powerSignals
    signals0 = newTimeBase "pre" signals (getTime powerSignals0)

    ---------------------------------------------------------------------------------------
    -- * Cut Signals and filter on low time sektions


    sequencePowers :: Sequ.List (PowerRecord System.Node [] Double)
    sequencePowers = genSequ powerSignals0

    ---------------------------------------------------------------------------------------
    -- * Integrate Power and Sections on maximum Energyflow
    (sequencePowersFilt, sequenceFlowsFilt) =
      Sequ.unzip $
      Sequ.filter (Record.major epsE epsT . snd) $
      fmap (\x -> (x, Record.partIntegrate x)) sequencePowers

    (flowTopos, adjustedFlows) =
      Sequ.unzip $ fmap (Flow.adjustedTopology topology) sequenceFlowsFilt

-------------------------------------------------------------------------------------------------
-- ## Analyse External Energy Flow

external ::
   (Eq d, Arith.Constant d, Arith.Integrate d, Arith.Scalar d ~ Double, B.BSum d,
    Vec.Storage v d, Vec.Zipper v, Vec.Walker v,
    Vec.Singleton v, Vec.FromList v) =>
   Flow.RangeGraph System.Node ->
   Sequ.List (FlowRecord System.Node v d) ->
   Env.Complete System.Node (Result Double) (Result d)
external sequenceFlowTopology seqFlowRecord =
   EqAbs.solveFromMeasurement
   sequenceFlowTopology $
   makeGivenFromExternal seqFlowRecord

initStorage :: (Arith.Constant a) => a
initStorage = Arith.fromRational $ 0.7*3600*1000

makeGivenFromExternal ::
   (Vec.Zipper v, Vec.Walker v, Vec.Singleton v,
    B.BSum d, Eq d, Arith.Constant d,
    Vec.Storage v d, Vec.FromList v) =>
   Sequ.List (FlowRecord System.Node v d) ->
   EqAbs.EquationSystem System.Node s Double d
makeGivenFromExternal sf =
   (SeqIdx.storage Idx.initial System.Water  .=  initStorage)
   <> fold (Sequ.mapWithSection f sf)
   where f sec (Record t xs) =
           (SeqIdx.dTime sec  .=
              Arith.integrate (Sig.toList $ Sig.delta t)) <>
           fold (Map.mapWithKey g xs)
           where g ppos e =
                    SeqIdx.energyFromPPos sec ppos  .=
                       Arith.integrate (Sig.toList e)

external2 ::
   (Eq a, Eq (v a), Vec.Singleton v, Vec.Storage v a, Vec.Walker v,
    Arith.Constant a, B.BSum a, Vec.Zipper v) =>
   Flow.RangeGraph System.Node ->
   Sequ.List (FlowRecord System.Node v a) ->
   Env.Complete System.Node
      (Result (Data Nil a))
      (Result (Data (v :> Nil) a))
external2 sequenceFlowTopology seqFlowRecord =
   EqAbs.solveFromMeasurement sequenceFlowTopology $
   makeGivenFromExternal2 seqFlowRecord

makeGivenFromExternal2 ::
   (Eq d, Eq (v d), Vec.Singleton v, Vec.Storage v d,
    Vec.Walker v, B.BSum d, Vec.Zipper v, Arith.Constant d) =>
   Sequ.List (FlowRecord System.Node v d) ->
   EqAbs.EquationSystem System.Node s (Data Nil d) (Data (v :> Nil) d)
makeGivenFromExternal2 sf =
   (SeqIdx.storage Idx.initial System.Water .= Data initStorage) <>
   (EqAbs.fromEnvSignal $ EqAbs.envFromFlowRecord (fmap Record.diffTime sf))
