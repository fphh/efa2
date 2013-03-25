{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Modules.Analysis where

import EFA.Example.Utility (edgeVar, checkDetermined,
                            (.=),
                            (%=)
                           )
import qualified EFA.Example.Absolute as EqAbs

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Result as R

import qualified EFA.Signal.SequenceData as SD

import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, removeZeroNoise)

import EFA.Signal.Sequence (genSequenceSignal,
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections,
                            genSequ,sectionRecordsFromSequence)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Base as B

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Equation.Stack as Stack
import EFA.Equation.Stack (Stack)

--import qualified EFA.Report.Report as Rep

--import EFA.Signal.Typ
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified Data.Map as M
import Data.Monoid ((<>),mempty)
import Data.Foldable (fold,
                      foldMap)

import qualified EFA.Equation.Environment as Env
import EFA.Equation.Result (Result(..))
import qualified EFA.Signal.Record as Record
----------------------------------
-- * Example Specific Imports
import qualified Modules.System as System
import Modules.Signals as Signals
--import Modules.Plots as Plots
import qualified EFA.Graph.Topology as TD

import qualified EFA.Equation.Record as EqRecord



-------------------------------------------------------------------------------------------------

sec2 :: Idx.Section
sec2 = Idx.Section 2

-------------------------------------------------------------------------------------------------
-- ## Preprocessing of Signals

pre :: Monad m =>
     TD.Topology System.Node
     -> SignalRecord [] Double
     -> m (SD.Sequ,
           SD.SequData (PowerRecord System.Node [] Double),
           SD.SequData (FlowRecord System.Node [] Double),
           SD.SequData (Record.FlowState System.Node),
           PowerRecord System.Node [] Double,
           SignalRecord
           [] Double)
pre topology rawSignals =  do

---------------------------------------------------------------------------------------
-- * Condition Signals, Calculate Powers, Remove ZeroNoise

  let signals = Signals.condition rawSignals
  let powerSignals = removeZeroNoise (Signals.calculatePower signals) (10^^(-2::Int))

---------------------------------------------------------------------------------------
-- * Add zerocrossings in Powersignals and Signals

  let powerSignals0 = addZeroCrossings powerSignals
  let signals0 = newTimeBase signals (getTime powerSignals0)

---------------------------------------------------------------------------------------
-- * Cut Signals and filter on low time sektions

  let sequencePowersRaw :: SD.SequData (PowerRecord System.Node [] Double)
      (sequenceRaw,sequencePowersRaw) = genSequ powerSignals0

  let (sequ,sequencePowers) = removeLowTimeSections(sequenceRaw,sequencePowersRaw) 0

  -- create sequence signal
  let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
  let sequenceSignals = sectionRecordsFromSequence signals0 sequ

---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on maximum Energyflow

  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequ,sequencePowers,sequenceFlows) 0

  let (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
            (\state ->
               let flowState = Flow.genFlowState state
               in  (flowState, Flow.adjustSigns topology flowState state))
            sequenceFlowsFilt

  return (sequenceFilt,sequencePowersFilt,adjustedFlows, flowStates, powerSignals0, signals0)

-------------------------------------------------------------------------------------------------
-- ## Analyse External Energy Flow

external :: (Vec.Zipper v, 
             Vec.Walker v, 
             Vec.Singleton v, 
             Eq d, 
             Num d, 
             B.BSum d,
             Arith.Product d, 
             Arith.Integrate d, 
             Vec.Storage v d,
             Vec.FromList v, 
             Arith.Scalar d ~ Double) =>
            TD.SequFlowGraph System.Node
            -> SD.SequData (FlowRecord System.Node v d) -- (Record s t2 t1 (PPosIdx System.Node)v d)
            -> Env.Complete
            System.Node
            (EqRecord.Absolute (Result Double))
            (EqRecord.Absolute (Result d))
external sequenceFlowTopology sequFlowRecord =  EqGen.solveFromMeasurement sequenceFlowTopology $ makeGivenFromExternal Idx.Absolute sequFlowRecord


initStorage :: (Fractional a, Num a) => a
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
                          EqGen.Record (EqRecord.FromIndex rec),
                          EqRecord.ToIndex (EqRecord.FromIndex rec) ~ rec) =>
                         rec
                         -> SD.SequData (FlowRecord System.Node v d)  -- (Record s t1 t2 (PPosIdx System.Node) v d)
                         -> EqGen.EquationSystem
                         (EqRecord.FromIndex rec) System.Node s1 Double d
makeGivenFromExternal idx sf =
   (Idx.Record idx (Idx.Storage (Idx.initBndNode System.Battery)) .= initStorage)
   <> (Idx.Record idx (Idx.Storage (Idx.initBndNode System.VehicleInertia)) .= 0)
   <> fold (SD.zipWithSecIdxs f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.DTime sec) .= sum (Sig.toList $ Sig.deltaSig t)) <>
           fold (M.mapWithKey g xs)
           where g (PPosIdx a b) e =
                    Idx.Record idx (edgeVar Idx.Energy sec a b) .= sum (Sig.toList e)

-------------------------------------------------------------------------------------------------
-- ## Predict Energy Flow

prediction :: (Eq d, Eq (Arith.Scalar d), Fractional (Arith.Scalar d),
               Fractional d, Arith.Product d, Arith.Product (Arith.Scalar d),
               Arith.Integrate d) =>
              TD.SequFlowGraph System.Node
              -> Env.Complete System.Node b (EqRecord.Absolute (Result d))
              -> Env.Complete
              System.Node
              (EqRecord.Absolute (Result (Arith.Scalar d)))
              (EqRecord.Absolute (Result d))
prediction sequenceFlowTopology env = EqGen.solve sequenceFlowTopology (makeGivenForPrediction Idx.Absolute env)

makeGivenForPrediction ::(Eq d, Eq a, Fractional d, Fractional a, Arith.Sum d,
                          Arith.Sum a, EqGen.Record (EqRecord.FromIndex uf),
                          EqRecord.ToIndex (EqRecord.FromIndex uf) ~ uf) =>
                         uf
                         -> Env.Complete
                         System.Node b (EqRecord.FromIndex uf (Result d))
                         -> EqGen.EquationSystem
                         (EqRecord.FromIndex uf) System.Node s a d
makeGivenForPrediction idx env =
    (Idx.Record idx (Idx.Storage (Idx.initBndNode System.Battery)) .= initStorage)
    <> (Idx.Record idx (Idx.Storage (Idx.initBndNode System.VehicleInertia)) .= 0)
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

delta :: (Vec.Zipper v1,
          Vec.Walker v1,
          Vec.Singleton v1,
          Vec.Zipper v2, 
          Vec.Walker v2, 
          Vec.Singleton v2,
          B.BSum d,
          Eq d, 
          Num d, 
          Arith.Product d, 
          Arith.Integrate d, 
          Vec.Storage v1 d,
          Vec.Storage v2 d, 
          Vec.FromList v1, 
          Vec.FromList v2,
          Arith.Scalar d ~ Double) =>
         TD.SequFlowGraph System.Node
         -> SD.SequData (FlowRecord System.Node v1 d)-- (Record s1 t1 t2 (PPosIdx System.Node) v1 d)
         -> SD.SequData (FlowRecord System.Node v2 d) -- (Record s2 t3 t4 (PPosIdx System.Node) v2 d)
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
  (Eq x, Arith.Sum x,
   EqGen.Element idx EqRecord.Absolute s a v
      ~ EqGen.VariableRecord EqRecord.Absolute s x,
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
   TD.SequFlowGraph System.Node ->
   Env.Complete System.Node DeltaResult DeltaResult ->
   Env.Complete
      System.Node
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


