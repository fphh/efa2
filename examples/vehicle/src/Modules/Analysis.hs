{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Analysis where

----------------------------------
-- * Example Specific Imports
import qualified Modules.System as System
import Modules.Signals as Signals

import qualified EFA.Example.Absolute as EqAbs

import qualified EFA.Example.Index as XIdx

import EFA.Example.Utility (Ignore, (.=), (%=), checkDetermined)

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
import qualified EFA.Graph.Topology.Node as TDNode

import EFA.Signal.Record (SignalRecord, FlowRecord,
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
import qualified EFA.Signal.Data as D

--import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal (TC(..), Scalar)
import EFA.Signal.Data (Data(..), Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

--import qualified EFA.Equation.Stack as Stack
--import EFA.Equation.Stack (Stack)
--import qualified EFA.Report.Report as Rep
--import EFA.Signal.Typ

import EFA.Report.FormatValue (FormatValue, FormatSignalIndex)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified Data.Map as Map
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

{-
newtype Settings = Settings {filePath :: FileName,
                             fileNames :: [FileName],
                             recordNames :: [RecordName],
                             zeroToleranz :: Double,
                             filterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double),
                             filterEnergy :: TC Scalar (Typ A F Tt) (Data Nil Double),
                             deltaSectionMapping :: [Int]
                            }
-}
-------------------------------------------------------------------------------------------------
{-
sec2 :: Idx.Section
sec2 = Idx.Section 2
-}
-------------------------------------------------------------------------------------------------
-- ## Preprocessing of Signals

pre :: TD.Topology System.Node
      -> Double
      -> TC Scalar (Typ A T Tt) (Data Nil Double)
      -> TC Scalar (Typ A F Tt) (Data Nil Double)
      -> SignalRecord [] Double
      -> (SD.SequData (PowerRecord System.Node [] Double),
         SD.SequData (FlowRecord System.Node [] Double),
         SD.SequData (Record.FlowState System.Node),
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


    sequencePowers :: SD.SequData (PowerRecord System.Node [] Double)
    sequencePowers = genSequ powerSignals0

    -- create sequence signal
    -- let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
    -- let sequenceSignals = sectionRecordsFromSequence signals0 sequ

    ---------------------------------------------------------------------------------------
    -- * Integrate Power and Sections on maximum Energyflow
    (sequencePowersFilt, sequenceFlowsFilt) =
      SD.unzip $
      SD.filter (Record.major epsE epsT . snd) $
      fmap (\x -> (x, Record.partIntegrate x)) sequencePowers

    (flowStates, adjustedFlows) =
      SD.unzip $
      fmap
      (\state ->
        let flowState = Flow.genFlowState state
        in  (flowState, Flow.adjustSigns topology flowState state))
      sequenceFlowsFilt

{-

-- New Approach with Utility-Funktions from HT - the challenges:

1. scalar value are in the moment double / signals are in data container. Best to move both to container
2. switch to dTime with fmap Record.diffTime
2. make delta - Analysis from two envs

external sequenceFlowTopology sequFlowRecord =  EqGen.solveFromMeasurement sequenceFlowTopology $ makeGivenFromExternal Idx.Absolute sequFlowRecord

initStorage :: (Fractional a) => a
initStorage = 0.7*3600*1000

makeGivenFromExternal idx sf = EqGen.fromEnvSignal . EqAbs.envFromFlowRecord $ sf
-}
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
external sequenceFlowTopology sequFlowRecord =
  EqGen.solveFromMeasurement
    sequenceFlowTopology $
    makeGivenFromExternal Idx.Absolute sequFlowRecord

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
                         EqGen.EquationSystem Ignore rec System.Node s Double d

makeGivenFromExternal idx sf =
   (Idx.Record idx (XIdx.storage Idx.initial System.Battery) .= initStorage)
   <> (Idx.Record idx (XIdx.storage Idx.initial System.VehicleInertia) .= 0)
   <> fold (SD.mapWithSection f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.InSection sec Idx.DTime) .= sum (Sig.toList $ Sig.delta t)) <>
           fold (Map.mapWithKey g xs)
           where g (Idx.PPos p) e =
                    Idx.Record idx (Idx.InSection sec (Idx.Energy p)) .= sum (Sig.toList e)
external2 :: (Eq a, Eq (v a), Vec.Singleton v, Vec.Storage v a, Vec.Walker v,
              TDNode.C node, Arith.Constant a, B.BSum a,
              Vec.Zipper v) =>
             Flow.RangeGraph node ->
             SD.SequData 
             (Record
              Sig.Signal
              Sig.FSignal
              (Typ A T Tt)
              (Typ A F Tt)
              (Idx.PPos node)
              v
              a
              a) ->
             Env.Complete 
             node
             (EqRecord.Absolute (Result (Data Nil a)))
             (EqRecord.Absolute (Result (Data (v D.:> Nil) a)))

external2 sequenceFlowTopology sequFlowRecord =
  EqGen.solveFromMeasurement
    sequenceFlowTopology $
    makeGivenFromExternal2 sequFlowRecord -- $ Record.diffTime sequFlowRecord

-- makeGivenFromExternal2 ::
--  makeGivenFromExternal2 env = EqGen.fromEnvSignal $ (fmap (fmap (D.foldl (+) 0) ) $ EqAbs.envFromFlowRecord env)
makeGivenFromExternal2 :: (Eq (v a1), Ord node, Arith.Sum a1, Vec.Zipper v,
                           Vec.Walker v, Vec.Storage v a1, Vec.Singleton v,
                           B.BSum a1) =>
                          SD.SequData (FlowRecord node v a1) ->
                          EqGen.EquationSystem
                          EqRecord.Absolute node s a (Data (v D.:> Nil) a1)
makeGivenFromExternal2 sf = EqGen.fromEnvSignal $ EqAbs.envFromFlowRecord (fmap Record.diffTime sf)

-------------------------------------------------------------------------------------------------
-- ## Predict Energy Flow

prediction ::
   (Eq v, Eq a,
    Fractional v, Fractional a,
    Arith.Constant a, Arith.Product v,
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
   EqGen.EquationSystem Ignore rec System.Node s a v

makeGivenForPrediction idx env =
    (Idx.Record idx (XIdx.storage Idx.initial System.Battery) .= initStorage)
    <> (Idx.Record idx (XIdx.storage Idx.initial System.VehicleInertia) .= 0)
--    <> (foldMap f $ Map.toList $ Env.etaMap $ Env.scalar env) -- hier müssen rote-Kante Gleichungen erzeugt werden
    <> (foldMap f $ Map.toList $ Env.etaMap $ Env.signal env)
    <> (foldMap f $ Map.toList $ Env.dtimeMap $ Env.signal env)
    <> (foldMap f $ Map.toList $ Map.mapWithKey h $ Map.filterWithKey i $ Map.filterWithKey g $
                               Env.energyMap $ Env.signal env)
    where f (j, x)  =  j %= fmap (\(EqGen.Determined y) -> y) x
          g (Idx.InSection _ (Idx.Energy (Idx.StructureEdge x y))) _  =
             case (x,y) of
                (System.Tank, System.ConBattery) -> True
                (System.Resistance, System.Chassis) -> True
                (System.VehicleInertia, System.Chassis) -> True
                (System.RearBrakes, System.Chassis) -> True
                (System.FrontBrakes, System.ConFrontBrakes) -> True
                (System.ConES, System.ElectricSystem) -> True
           --     (System.Battery, System.ConBattery) -> True
                _ -> False
          h (Idx.InSection _ (Idx.Energy (Idx.StructureEdge System.Resistance System.Chassis))) x =
               fmap (fmap (*1.1)) x
          h _ r = r
          i _ _ = True
--         i (Idx.InSection (Idx.Section sec) (Idx.Energy (Idx.StructureEdge x y))) _ | sec == 18 || x == System.Tank || y == System.ConBattery = False
--          i (Idx.InSection (Idx.Section sec) (Idx.Energy (Idx.StructureEdge x y))) _ | otherwise = True


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
delta sequenceFlowTopology sequenceFlow sequenceFlow'=
  EqGen.solveFromMeasurement sequenceFlowTopology $
    ( makeGivenFromExternal Idx.Before sequenceFlow <>
      makeGivenFromExternal Idx.After sequenceFlow')

------------------------------------------------------------------
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
   Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> x ->
   EqAbs.EquationSystem node s a v
(.==) = (EqAbs..=)

deltaPair ::
   (Ord (idx System.Node), Env.AccessSignalMap idx, FormatSignalIndex idx) =>
   Idx.InSection idx System.Node -> Double -> Double -> EquationSystemNumeric s
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
  (XIdx.storage Idx.initial System.Battery .== initStorage) <>
  (fold $ Map.mapWithKey f $ Env.etaMap sig) <>
  (fold $ Map.mapWithKey f $ Env.dtimeMap sig) <>
  (fold $  Map.filterWithKey h $ Map.filterWithKey g $ Map.mapWithKey f $ Env.energyMap sig) <>
  mempty
  where f i rec =
           deltaPair i
              (checkDetermined "before" $ EqRecord.before rec)
              (checkDetermined "delta"  $ EqRecord.delta rec)

        g (Idx.InSection _ (Idx.Energy (Idx.StructureEdge x y))) _ =
          case (x,y) of
            (System.Tank, System.ConBattery) -> True
            (System.Resistance, System.Chassis) -> True
            (System.VehicleInertia, System.Chassis) -> True
            (System.RearBrakes, System.Chassis) -> True
            (System.FrontBrakes, System.ConFrontBrakes) -> True
            (System.ConES, System.ElectricSystem) -> True
--            (System.Battery, System.ConBattery) -> True -- Das sollte nicht angegeben werden müssen !!
            _ -> False

        h _ _ = True
        -- h (Idx.InSection (Idx.Section sec) (Idx.Energy (Idx.StructureEdge x y))) _ | sec == 18 || x == System.Tank || y == System.ConBattery = False
        -- h (Idx.InSection (Idx.Section sec) (Idx.Energy (Idx.StructureEdge x y))) _ | otherwise = True
