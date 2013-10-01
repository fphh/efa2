{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Modules.Analysis where

----------------------------------
-- * Example Specific Imports
import qualified Modules.System as System
import qualified Modules.Signals as Signals

import qualified EFA.Application.Absolute as EqAbs

import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Flow as Flow

-- import qualified EFA.Utility.Stream as Stream
-- import EFA.Utility.Stream (Stream((:~)))
-- import EFA.Utility.Map (checkedLookup)

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
-- import qualified EFA.Equation.Stack as Stack
import EFA.Equation.Result (Result)
import EFA.Equation.Verify (Ignore)
import EFA.Equation.System ((.=))
-- import EFA.Equation.Stack (Stack)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Base as B
import qualified EFA.Signal.Data as D
import EFA.Signal.Signal (TC, Scalar)
import EFA.Signal.Data (Data(Data), Nil)
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

pre :: Topo.Topology System.Node
      -> Double
      -> TC Scalar (Typ A T Tt) (Data Nil Double)
      -> TC Scalar (Typ A F Tt) (Data Nil Double)
      -> SignalRecord [] Double
      -> (Sequ.List (PowerRecord System.Node [] Double),
         Sequ.List (FlowRecord System.Node [] Double),
         Sequ.List (Flow.State System.Node),
         PowerRecord System.Node [] Double,
         SignalRecord [] Double)

pre topology epsZero epsT epsE rawSignals =
  (sequencePowersFilt, adjustedFlows, flowStates, powerSignals0, signals0)
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

    (flowStates, adjustedFlows) =
      Sequ.unzip $
      fmap
      (\state ->
        let flowState = Flow.genFlowState state
        in  (flowState, Flow.adjustSigns topology flowState state))
      sequenceFlowsFilt

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
  Env.completeFMap EqRecord.unAbsolute EqRecord.unAbsolute $
  EqGen.solveFromMeasurement
    sequenceFlowTopology $
    makeGivenFromExternal Idx.Absolute seqFlowRecord

initStorage :: (Arith.Constant a) => a
initStorage = Arith.fromRational $ 0.7*3600*1000

makeGivenFromExternal :: (Vec.Zipper v,
                          Vec.Walker v,
                          Vec.Singleton v,
                          B.BSum d,
                          Eq d,
                          Arith.Constant d,
                          Vec.Storage v d,
                          Vec.FromList v,
                          EqGen.Record rec,
                          idx ~ EqRecord.ToIndex rec) =>
                         idx ->
                         Sequ.List (FlowRecord System.Node v d) ->
                         EqGen.EquationSystem Ignore rec System.Node s Double d

makeGivenFromExternal idx sf =
   (Idx.Record idx (SeqIdx.storage Idx.initial System.Water) .= initStorage)
   <> fold (Sequ.mapWithSection f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.InPart sec Idx.DTime) .=
              Arith.integrate (Sig.toList $ Sig.delta t)) <>
           fold (Map.mapWithKey g xs)
           where g ppos e =
                    Idx.Record idx (SeqIdx.energyFromPPos sec ppos) .=
                       Arith.integrate (Sig.toList e)

external2 ::
   (Eq a, Eq (v a), Vec.Singleton v, Vec.Storage v a, Vec.Walker v,
    Arith.Constant a, B.BSum a, Vec.Zipper v) =>
   Flow.RangeGraph System.Node ->
   Sequ.List
      (Record Sig.Signal Sig.FSignal
          (Typ A T Tt)
          (Typ A F Tt)
          (Idx.PPos System.Node)
          v a a) ->
   Env.Complete System.Node
      (Result (Data Nil a))
      (Result (Data (v D.:> Nil) a))
external2 sequenceFlowTopology seqFlowRecord =
  Env.completeFMap EqRecord.unAbsolute EqRecord.unAbsolute $
  EqGen.solveFromMeasurement sequenceFlowTopology $
    makeGivenFromExternal2 seqFlowRecord -- $ Record.diffTime seqFlowRecord

-- makeGivenFromExternal2 env = EqGen.fromEnvSignal $ (fmap (fmap (D.foldl (+) 0) ) $ EqAbs.envFromFlowRecord env)
makeGivenFromExternal2 ::
   (Eq d, Eq (v d), Vec.Singleton v, Vec.Storage v d,
    Vec.Walker v, B.BSum d, Vec.Zipper v, Arith.Constant d) =>
   Sequ.List (FlowRecord System.Node v d) ->
   EqAbs.EquationSystem System.Node s (Data Nil d) (Data (v D.:> Nil) d)
makeGivenFromExternal2 sf =
      (Idx.absolute (SeqIdx.storage Idx.initial System.Water) .= Data initStorage) <>
      (EqAbs.fromEnvSignal $ EqAbs.envFromFlowRecord (fmap Record.diffTime sf))

---------------------------------------------------------------------------------------------------
-- ## Make Delta

delta :: (Vec.Zipper v1, Vec.Zipper v2,
          Vec.Walker v1, Vec.Walker v2,
          Vec.Singleton v1, Vec.Singleton v2,
          Vec.Storage v1 d,Vec.Storage v2 d,
          Vec.FromList v1,Vec.FromList v2,
          B.BSum d,
          Eq d,
          Arith.Constant d,
          Arith.Integrate d,
          Arith.Scalar d ~ Double) =>
         Flow.RangeGraph System.Node
         -> Sequ.List (FlowRecord System.Node v1 d)
         -> Sequ.List (FlowRecord System.Node v2 d)
         -> Env.Complete
         System.Node
         (EqRecord.Delta (Result Double))
         (EqRecord.Delta (Result d))
delta sequenceFlowTopology sequenceFlow sequenceFlow' =
  EqGen.solveFromMeasurement sequenceFlowTopology $
    ( makeGivenFromExternal Idx.Before sequenceFlow <>
      makeGivenFromExternal Idx.After sequenceFlow')
