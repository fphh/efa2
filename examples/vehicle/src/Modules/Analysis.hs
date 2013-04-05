{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Modules.Analysis where

----------------------------------
-- * Example Specific Imports
import qualified Modules.System as System
import Modules.Signals as Signals

import qualified EFA.Example.Absolute as EqAbs

import qualified EFA.Example.Index as XIdx
import EFA.Example.Utility ((.=), (%=), checkDetermined)

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Result as R
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import EFA.Equation.Result (Result(..))
import EFA.Equation.Stack (Stack)

import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Sequence as Seq
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig

import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord, getTime, newTimeBase)


import EFA.Signal.Sequence (-- genSequenceSignal,
                            addZeroCrossings,
                            genSequ,
                           -- sectionRecordsFromSequence
                           )

--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Base as B

--import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal (TC(..), Scalar)
import EFA.Signal.Data (Data(..), Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

--import qualified EFA.Equation.Stack as Stack
--import EFA.Equation.Stack (Stack)
--import qualified EFA.Report.Report as Rep
--import EFA.Signal.Typ

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified Data.Map as M
import Data.Monoid ((<>),mempty)

import Data.Foldable (fold,
                      foldMap)

--import qualified EFA.Equation.Environment as Env
--import EFA.Equation.Result (Result(..))
--import qualified EFA.Signal.Record as Record
----------------------------------
-- * Example Specific Imports
--import qualified Modules.System as System
--import Modules.Signals as Signals
--import Modules.Plots as Plots
--import qualified EFA.Graph.Topology as TD

--import qualified EFA.Equation.Record as EqRecord


-------------------------------------------------------------------------------------------------

sec2 :: Idx.Section
sec2 = Idx.Section 2

-------------------------------------------------------------------------------------------------
-- ## Preprocessing of Signals

pre :: Monad m =>
     TD.Topology System.Node
     -> SignalRecord [] Double
     -> m (SD.SequData (PowerRecord System.Node [] Double),
           SD.SequData (FlowRecord System.Node [] Double),
           SD.SequData (Record.FlowState System.Node),
           PowerRecord System.Node [] Double,
           SignalRecord [] Double)

pre topology rawSignals =  do

---------------------------------------------------------------------------------------
-- * Condition Signals, Calculate Powers, Remove ZeroNoise

  let signals = Signals.condition rawSignals
  let powerSignals = Record.removeZeroNoise (10^^(-2::Int)) (Signals.calculatePower signals)

---------------------------------------------------------------------------------------
-- * Add zerocrossings in Powersignals and Signals

  let powerSignals0 = addZeroCrossings powerSignals
  let signals0 = newTimeBase signals (getTime powerSignals0)

---------------------------------------------------------------------------------------
-- * Cut Signals and filter on low time sektions


  let sequencePowers :: SD.SequData (PowerRecord System.Node [] Double)
      sequencePowers = genSequ powerSignals0

  -- create sequence signal
  -- let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
  -- let sequenceSignals = sectionRecordsFromSequence signals0 sequ

---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on maximum Energyflow


  let epsT ::  TC Scalar (Typ A T Tt) (Data Nil Double)
      epsT = Sig.toScalar 0

      epsE ::  TC Scalar (Typ A F Tt) (Data Nil Double)
      epsE = Sig.toScalar 0


  let (sequencePowersFilt, sequenceFlowsFilt) =
         SD.unzip $
         SD.filter (Record.major epsE epsT . snd) $
         fmap (\x -> (x, Record.partIntegrate x)) sequencePowers

  let (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
            (\state ->
               let flowState = Flow.genFlowState state
               in  (flowState, Flow.adjustSigns topology flowState state))
            sequenceFlowsFilt

  return (sequencePowersFilt, adjustedFlows, flowStates, powerSignals0, signals0)

-------------------------------------------------------------------------------------------------
-- ## Analyse External Energy Flow

external :: (Eq d, Num d,
             Arith.Product d,
             Arith.Integrate d,
             Vec.Storage v d,
             Vec.Zipper v,
             Vec.Walker v,
             Vec.Singleton v,
             B.BSum d,
             Vec.FromList v, Arith.Scalar d ~ Double) =>
            Flow.RangeGraph System.Node
            -> SD.SequData (FlowRecord System.Node v d)
            -> Env.Complete
            System.Node
            (EqRecord.Absolute (Result Double))
            (EqRecord.Absolute (Result d))
external sequenceFlowTopology sequFlowRecord =  EqGen.solveFromMeasurement sequenceFlowTopology $ makeGivenFromExternal Idx.Absolute sequFlowRecord

initStorage :: (Fractional a) => a
initStorage = 0.7*3600*1000


makeGivenFromExternal :: (Vec.Zipper v,
                          Vec.Walker v,
                          Vec.Singleton v,
                          B.BSum d,
                          Eq d,
                          Num d,
                          Arith.Sum d,
                          Vec.Storage v d,
                          Vec.FromList v,
                          EqGen.Record rec,
                          idx ~ EqRecord.ToIndex rec) =>
                         idx ->
                         SD.SequData (FlowRecord System.Node v d) ->
                         EqGen.EquationSystem rec System.Node s Double d

makeGivenFromExternal idx sf =
   (Idx.Record idx (XIdx.storage Idx.initial System.Battery) .= initStorage)
   <> (Idx.Record idx (XIdx.storage Idx.initial System.VehicleInertia) .= 0)
   <> fold (SD.mapWithSection f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.DTime sec) .=  sum (Sig.toList $ Sig.delta t)) <>
           fold (M.mapWithKey g xs)
           where g (PPosIdx a b) e =
                    Idx.Record idx (XIdx.energy sec a b) .= sum (Sig.toList e)

-------------------------------------------------------------------------------------------------
-- ## Predict Energy Flow

prediction ::
   (Eq v, Eq a,
    Fractional v, Fractional a,
    Arith.Product a, Arith.Product v,
    Arith.Integrate v, Arith.Scalar v ~ a) =>
  Flow.RangeGraph System.Node ->
   Env.Complete System.Node
      (EqRecord.Absolute (Result a))
      (EqRecord.Absolute (Result v)) ->
   Env.Complete System.Node
      (EqRecord.Absolute (Result a))
      (EqRecord.Absolute (Result v))
prediction sequenceFlowTopology env =
   EqGen.solve sequenceFlowTopology (makeGivenForPrediction Idx.Absolute env)

makeGivenForPrediction ::
   (Eq v, Eq a,
    Fractional v, Fractional a,
    Arith.Sum v, Arith.Sum a,
    EqGen.Record rec,
    EqRecord.ToIndex rec ~ idx) =>
   idx ->
   Env.Complete System.Node (rec (Result a)) (rec (Result v)) ->
   EqGen.EquationSystem rec System.Node s a v

makeGivenForPrediction idx env =
    (Idx.Record idx (XIdx.storage Idx.initial System.Battery) .= initStorage)
    <> (Idx.Record idx (XIdx.storage Idx.initial System.VehicleInertia) .= 0)
    <> (foldMap f $ M.toList $ Env.etaMap $ Env.signal env)
    <> (foldMap f $ M.toList $ Env.dtimeMap $ Env.signal env)
    <> (foldMap f $ M.toList $ M.mapWithKey h $ M.filterWithKey g $
                               Env.energyMap $ Env.signal env)
    where f (i, x)  =  i %= fmap (\(EqGen.Determined y) -> y) x
          g (Idx.Energy (Idx.StructureEdge _ x y)) _  =
             case (x,y) of
                (System.Resistance, System.Chassis) -> True
                (System.VehicleInertia, System.Chassis) -> True
                (System.RearBrakes, System.Chassis) -> True
                (System.FrontBrakes, System.ConFrontBrakes) -> True
                (System.ConES, System.ElectricSystem) -> True
                (System.Battery, System.ConBattery) -> True
                _ -> False
          h (Idx.Energy (Idx.StructureEdge _ System.Resistance System.Chassis)) x =
               fmap (fmap (*1.1)) x
          h _ r = r


---------------------------------------------------------------------------------------------------
-- ## Make Delta

delta :: (Vec.Zipper v1, Vec.Zipper v2,
          Vec.Walker v1, Vec.Walker v2,
          Vec.Singleton v1, Vec.Singleton v2,
          Vec.Storage v1 d,Vec.Storage v2 d,
          Vec.FromList v1,Vec.FromList v2,
          B.BSum d,
          Eq d,
          Num d,
          Arith.Product d,
          Arith.Integrate d,
          Arith.Scalar d ~ Double) =>
         Flow.RangeGraph System.Node
         -> SD.SequData (FlowRecord System.Node v1 d)
         -> SD.SequData (FlowRecord System.Node v2 d)
         -> Env.Complete
         System.Node
         (EqRecord.Delta (Result Double))
         (EqRecord.Delta (Result d))
delta sequenceFlowTopology sequenceFlow sequenceFlow'= EqGen.solveFromMeasurement sequenceFlowTopology
                                                       $ (makeGivenFromExternal Idx.Before sequenceFlow <>
                                                          makeGivenFromExternal Idx.After sequenceFlow')

---------------------------------------------------------------------------------------------------
-- ## Make Difference Analysis


type
   EquationSystemNumeric s =
      EqAbs.EquationSystem System.Node s
         (Stack (Var.Any System.Node) Double)
         (Stack (Var.Any System.Node) Double)

type DeltaResult = EqRecord.Delta (R.Result Double)


infix 0 .==

(.==) ::
  (Eq x, Arith.Sum x, x ~ Env.Element idx a v,
   Env.AccessMap idx, Ord (idx node)) =>
   idx node -> x ->
   EqAbs.EquationSystem node s a v
(.==) = (EqAbs..=)

deltaPair ::
   (Ord (idx System.Node), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   (idx System.Node) -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delt =
   idx .== Stack.deltaPair (Var.Signal $ Var.index idx) before delt

difference ::
   Flow.RangeGraph System.Node ->
   Env.Complete System.Node DeltaResult DeltaResult ->
   Env.Complete System.Node
      (EqRecord.Absolute (Result (Stack (Var.Any System.Node) Double)))
      (EqRecord.Absolute (Result (Stack (Var.Any System.Node) Double)))
difference sequenceFlowTopology env =
  EqGen.solve sequenceFlowTopology (makeGivenForDifferentialAnalysis env)


makeGivenForDifferentialAnalysis ::
  Env.Complete System.Node DeltaResult DeltaResult ->
  EquationSystemNumeric s
makeGivenForDifferentialAnalysis (Env.Complete _ sig) =
--  (Idx.DTime sec2 .== 0) <>
  (Idx.Storage (Idx.initBndNode System.Battery) .== initStorage) <>
--  (deltaPair (edgeVar Idx.Energy sec2 System.Tank System.ConBattery) 4 (-0.6)) <>
  (fold $ M.mapWithKey f $ Env.etaMap sig) <>
  (fold $ M.mapWithKey f $ Env.dtimeMap sig) <>
  (fold $ M.filterWithKey g $ M.mapWithKey f $ Env.energyMap sig) <>
  mempty
  where f i rec =
           deltaPair i
              (checkDetermined "before" $ EqRecord.before rec)
              (checkDetermined "delta"  $ EqRecord.delta rec)

        g (Idx.Energy (Idx.StructureEdge _ x y)) _ =
          case (x,y) of
            (System.Resistance, System.Chassis) -> True
            (System.VehicleInertia, System.Chassis) -> True
            (System.RearBrakes, System.Chassis) -> True
            (System.FrontBrakes, System.ConFrontBrakes) -> True
            (System.ConES, System.ElectricSystem) -> True
            (System.Battery, System.ConBattery) -> True
            _ -> False
