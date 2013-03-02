module Modules.Analysis where

import EFA.Example.Utility (edgeVar, Term, (.=), (%=))
import qualified EFA.Equation.System as EqGen
--import EFA.IO.CSVImport (modelicaCSVImport)
import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, removeZeroNoise,getTimeWindow)
import EFA.Signal.Sequence (makeSeqFlowTopology,genSequenceSignal,chopAtZeroCrossingsPowerRecord,
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections,removeZeroTimeSections, genSequ,sectionRecordsFromSequence)
import qualified EFA.Signal.Signal as Sig -- (toList,UTSigL,setType)
import qualified EFA.Signal.Plot as Pl
import EFA.Signal.Typ

import qualified EFA.Report.Report as Rep
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw

import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import EFA.Graph.Topology(isOriginalEdge)
import EFA.Graph(lefilter)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (fold, foldMap)

import qualified EFA.Equation.Env as Env
import EFA.Equation.Result (Result(..))

----------------------------------
-- * Example Specific Imports


import qualified Modules.System as System

-- Signal Treatment
import Modules.Signals as Signals
-- Plotting
import Modules.Plots as Plot



-- pre rawSignals :: SignalRecord v a ->
pre topology rawSignals =  do

---------------------------------------------------------------------------------------
-- * Condition Signals, Calculate Powers, Remove ZeroNoise
  let signals = Signals.condition rawSignals
  let powerSignals = removeZeroNoise (Signals.calculatePower signals) (10^^(-2::Int))

---------------------------------------------------------------------------------------
-- * Add zerocrossings in Powersignals and Signals
  let powerSignals0 = addZeroCrossings powerSignals
  let signals0 = newTimeBase signals (getTime powerSignals0)

  -- Rep.report [] ("Time",(getTime powerSignals0))
  -- Rep.report [] ("Signals0",signals0)

---------------------------------------------------------------------------------------
-- * Plot Signals
{--
  Plot.vehicle signals0
  Plot.motor signals0
  Plot.generator signals0
  Plot.driveline signals0
  Plot.electric signals0
  Plot.battery signals0

  Rep.report [] ("Signals0",signals0)

---------------------------------------------------------------------------------------
-- * Plot Power Signals

  Plot.genPowers powerSignals0
  Plot.propPowers powerSignals0
  Plot.vehPowers powerSignals0
--}
---------------------------------------------------------------------------------------
-- * Cut Signals and filter on low time sektions

  let sequencePowersRaw :: SD.SequData (PowerRecord System.Node [] Double)
      (sequenceRaw,sequencePowersRaw) = genSequ powerSignals0

-- Rep.report [] ("Sequence", sequ)

  let (sequ,sequencePowers) = removeLowTimeSections(sequenceRaw,sequencePowersRaw) 1
  --  let (sequ,sequencePowers) = removeZeroTimeSections(sequenceRaw,sequencePowersRaw)

  -- create sequence signal
  let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
  let sequenceSignals = sectionRecordsFromSequence signals0 sequ

  --Pl.recordSplitPlus 1 "Mit SektionsSignal" powerSignals0 [(PPosIdx System.Tank System.Tank, Sig.setType sequSig)]
  --Rep.report [Rep.RAll,Rep.RVertical] ("Powers0", powerSignals0)

---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on maximum Energyflow

  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequ,sequencePowers,sequenceFlows) 0

  -- Rep.report [] ("SequenceFilt", sequenceFilt)
  let flowStates = fmap Flow.genFlowState sequenceFlowsFilt
  let adjustedFlows = Flow.adjustSigns topology flowStates sequenceFlowsFilt

  return  (sequenceFilt,sequencePowersFilt,adjustedFlows, flowStates)


initStorage :: Double
initStorage = 0.7*3600*1000

makeGiven ::
   (Env.Record recIdx rec, EqGen.Record rec) =>
   recIdx ->
   SD.SequData (FlowRecord System.Node [] Double)->
   (EqGen.EquationSystem rec System.Node s Double)
makeGiven idx sf =
   (Idx.Record idx (Idx.DTime Idx.initSection) .= 1)
   <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= initStorage)
   <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 0)
   <> fold (SD.zipWithSecIdxs f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.DTime sec) .= sum (Sig.toList t)) <>
           fold (M.mapWithKey g xs)
           where g (PPosIdx a b) e =
                    Idx.Record idx (edgeVar Idx.Energy sec a b) .= sum (Sig.toList e)

makeGivenForPrediction ::
   (Env.Record recIdx rec, EqGen.Record rec) =>
   recIdx ->
   Env.Env System.Node (rec (EqGen.Result Double)) ->
   (EqGen.EquationSystem rec System.Node s Double)
makeGivenForPrediction idx env =
    (Idx.Record idx (Idx.DTime Idx.initSection) .= 1)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= initStorage)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 0)
    <> (foldMap f (M.toList (Env.etaMap env)))
    <> (foldMap f (M.toList (Env.dtimeMap env)))
    <> (foldMap f (M.toList $ M.mapWithKey h $ M.filterWithKey g $ Env.energyMap env))
    where f (i, x)  =  i %= fmap (\(EqGen.Determined y) -> y) x
          g (Idx.Energy (Idx.SecNode _ x) (Idx.SecNode _ y)) _ =
             case (x,y) of
                (System.Resistance, System.Chassis) -> True
                (System.VehicleInertia, System.Chassis) -> True
                (System.RearBrakes, System.Chassis) -> True
                (System.FrontBrakes, System.ConFrontBrakes) -> True
                (System.ConES, System.ElectricSystem) -> True
                (System.Battery, System.ConBattery) -> True
                _ -> False
          h (Idx.Energy (Idx.SecNode _ System.Resistance) (Idx.SecNode _ System.Chassis)) x =
               fmap (fmap (*1.1)) x
          h _ r = r
