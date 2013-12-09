{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Modules.Analysis where

import qualified Modules.System as System
import qualified Modules.Signals as Signals

import qualified EFA.Flow.Sequence.Absolute as EqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Flow.Topology.Record as TopoRecord

import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data(Data), Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt)
import EFA.Signal.Record (SignalRecord, PowerRecord, getTime, newTimeBase)
import EFA.Signal.Chop (addZeroCrossings, genSequ)


-------------------------------------------------------------------------------------------------
-- ## Preprocessing of Signals

pre ::
   Topo.Topology System.Node ->
   Double ->
   Sig.Scal (Typ A T Tt) Double ->
   Sig.Scal (Typ A F Tt) Double ->
   SignalRecord [] Double ->
   (Sequ.List (PowerRecord System.Node [] Double),
    Sequ.List (TopoRecord.Section System.Node [] Double),
    PowerRecord System.Node [] Double,
    SignalRecord [] Double)

pre topology epsZero epsT epsE rawSignals =
  (sequencePowersFilt, signalTopos, powerSignals0, signals0)
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

    signalTopos =
      fmap (TopoRecord.flowTopologyFromRecord topology) sequenceFlowsFilt

-------------------------------------------------------------------------------------------------
-- ## Analyse External Energy Flow

initStorage :: (Arith.Constant a) => a
initStorage = Arith.fromRational $ 0.7*3600*1000

external ::
   (Vec.Walker c, Vec.Storage c a, Arith.ZeroTestable a,
    Arith.Integrate a, Arith.Constant a, Arith.Scalar a ~ a) =>
   SeqFlow.Graph System.Node (Result (Data Nil a)) (Result (Data (c :> Nil) a)) ->
   SeqFlow.Graph System.Node (Result a) (Result a)
external sequenceFlowGraph =
   EqAbs.solveOpts
      (EqAbs.independentInOutSums EqAbs.optionsDefault)
      (SeqFlow.mapGraph
         (fmap getData)
         (fmap (getData . Arith.integrate))
         sequenceFlowGraph)
      (SeqIdx.storage Idx.initial System.Water  .=  initStorage)

external2 ::
   (Vec.Singleton v, Vec.Walker v, Vec.Zipper v,
    Vec.Storage v a, Vec.Storage v Bool,
    Arith.Constant a, Arith.ZeroTestable a) =>
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data (v :> Nil) a)) ->
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data (v :> Nil) a))
external2 sequenceFlowGraph =
   EqAbs.solveOpts
      (EqAbs.independentInOutSums EqAbs.optionsDefault)
      sequenceFlowGraph
      (SeqIdx.storage Idx.initial System.Water  .=  Data initStorage)

external3 ::
   (Arith.Constant a, Arith.ZeroTestable a) =>
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data Nil a)) ->
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data Nil a))
external3 sequenceFlowGraph =
   EqAbs.solveOpts
      (EqAbs.independentInOutSums EqAbs.optionsDefault)
      sequenceFlowGraph
      (SeqIdx.storage Idx.initial System.Water  .=  Data initStorage)
