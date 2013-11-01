{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Analysis where

import qualified Modules.System as System
import qualified Modules.Signals as Signals

import qualified EFA.Flow.Sequence.Absolute as EqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Flow.SequenceState.Variable as Var

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Record as EqRecord
import EFA.Equation.Arithmetic ((~*))
import EFA.Equation.Result (Result(Determined, Undetermined))
import EFA.Equation.Stack (Stack)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig

import EFA.Signal.Record (SignalRecord, PowerRecord, getTime, newTimeBase)

import EFA.Signal.Chop (addZeroCrossings, genSequ)
import EFA.Signal.Data (Data, Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import Data.Monoid ((<>))


{-
newtype Settings = Settings {filePath :: FileName,
                             fileNames :: [FileName],
                             recordNames :: [RecordName],
                             zeroTolerance :: Double,
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

{-

-- New Approach with Utility-Functions from HT - the challenges:

1. scalar value are in the moment double / signals are in data container. Best to move both to container
2. switch to dTime with fmap Record.diffTime
2. make delta - Analysis from two envs

external sequenceFlowTopology seqFlowRecord =  EqSys.solveFromMeasurement sequenceFlowTopology $ makeGivenFromExternal Idx.Absolute seqFlowRecord
-}


-------------------------------------------------------------------------------------------------
-- ## Analyse External Energy Flow

external ::
   (Vec.Walker c, Vec.Storage c a,
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
      ((XIdx.storage Idx.initial System.Battery .= initStorage) <>
       (XIdx.storage Idx.initial System.VehicleInertia .= Arith.zero))

initStorage :: (Arith.Constant a) => a
initStorage = Arith.fromRational $ 0.7*3600*1000

-------------------------------------------------------------------------------------------------
-- ## Predict Energy Flow

prediction ::
   (Eq a, Arith.Constant a,
    Eq v, Arith.Constant v,
    Arith.Integrate v, Arith.Scalar v ~ a) =>
   SeqFlow.Graph System.Node a v ->
   SeqFlow.Graph System.Node (Result a) (Result v)
prediction sequenceFlowTopology =
   EqAbs.solve
      (SeqFlow.mapGraphWithVar
         (const $ const Undetermined) givenForPrediction
         sequenceFlowTopology)
      ((XIdx.storage Idx.initial System.Battery .= initStorage) <>
       (XIdx.storage Idx.initial System.VehicleInertia .= Arith.zero))

givenForPrediction ::
   (Arith.Constant v) =>
   Var.InSectionSignal System.Node -> v -> Result v
givenForPrediction (Idx.InPart _sec var) v =
   case var of
      TopoVar.DTime _ -> Determined v
      TopoVar.Eta _ -> Determined v
      TopoVar.Energy idx ->
         if filterCriterion idx
           then Determined $
              case idx of
                 TopoIdx.Energy (TopoIdx.Edge System.Resistance System.Chassis) ->
                    v ~* Arith.fromRational 1.1
                 _ -> v
           else Undetermined
      _ -> Undetermined


modifyPredictionEnergy ::
   (Arith.Constant a) =>
   Graph.DirEdge System.Node ->
   Result a -> Result a
modifyPredictionEnergy edge energy =
   if filterCriterion $ TopoIdx.Energy $ Topo.topologyEdgeFromDirEdge edge
     then
        case edge of
           Graph.DirEdge System.Resistance System.Chassis ->
              fmap (Arith.fromRational 1.1 ~*) energy
           _ -> energy
     else Undetermined


------------------------------------------------------------------
-- ## Make Difference Analysis


type
   EquationSystemNumeric s =
      EqAbs.EquationSystemIgnore System.Node s StackDouble StackDouble

type DeltaDouble = EqRecord.Delta Double
type StackDouble = Stack (Var.SectionAny System.Node) Double


stackFromDelta ::
   Var.InSectionSignal System.Node -> DeltaDouble -> StackDouble
stackFromDelta idx d =
   Stack.deltaPair (Var.Signal idx)
      (EqRecord.before d) (EqRecord.delta d)

difference ::
   SeqFlow.Graph System.Node DeltaDouble DeltaDouble ->
   SeqFlow.Graph System.Node (Result StackDouble) (Result StackDouble)
difference sequenceFlowTopology =
   EqAbs.solve
      (SeqFlow.mapGraphWithVar
          (\_i -> const Undetermined)
          (\i -> givenForDifferentialAnalysis i . stackFromDelta i)
          sequenceFlowTopology)
      (XIdx.storage Idx.initial System.Battery .= initStorage)

givenForDifferentialAnalysis ::
   Var.InSectionSignal System.Node -> v -> Result v
givenForDifferentialAnalysis (Idx.InPart _sec var) v =
   case var of
      TopoVar.DTime _ -> Determined v
      TopoVar.Eta _ -> Determined v
      TopoVar.Energy idx ->
         if filterCriterion idx
           then Determined v
           else Undetermined
      _ -> Undetermined


filterCriterion :: TopoIdx.Energy System.Node -> Bool
filterCriterion (TopoIdx.Energy (TopoIdx.Edge x y)) =
   -- filterCriterionExtra e &&
   case (x,y) of
      (System.Tank, System.ConBattery) -> True
      (System.Resistance, System.Chassis) -> True
      (System.VehicleInertia, System.Chassis) -> True
      (System.RearBrakes, System.Chassis) -> True
      (System.FrontBrakes, System.ConFrontBrakes) -> True
      (System.ConES, System.ElectricSystem) -> True
--       (System.Battery, System.ConBattery) -> True -- Das sollte nicht angegeben werden müssen !!
      _ -> False

filterCriterionExtra :: XIdx.Energy System.Node -> Bool
filterCriterionExtra
      (Idx.InPart sec (TopoIdx.Energy (TopoIdx.Edge x y))) =
   not $ sec == Idx.Section 18 || x == System.Tank || y == System.ConBattery
