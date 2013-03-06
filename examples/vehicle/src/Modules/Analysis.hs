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

import EFA.Graph(lefilter)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (fold, foldMap)

import qualified EFA.Equation.Env as Env
import EFA.Equation.Result (Result(..))

import qualified EFA.Equation.Variable as Var
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Hack.Stack as HSt

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
   (EqGen.Record rec) =>
   Env.RecordIndex rec ->
   SD.SequData (FlowRecord System.Node [] Double)->
   (EqGen.EquationSystem rec System.Node s Double Double)
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
   (EqGen.Record rec) =>
   Env.RecordIndex rec ->
   Env.Complete System.Node
      (rec (EqGen.Result Double)) (rec (EqGen.Result Double)) ->
   (EqGen.EquationSystem rec System.Node s Double Double)
makeGivenForPrediction idx env =
    (Idx.Record idx (Idx.DTime Idx.initSection) .= 1)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= initStorage)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 0)
    <> (foldMap f $ M.toList $ Env.etaMap $ Env.signal env)
    <> (foldMap f $ M.toList $ Env.dtimeMap $ Env.signal env)
    <> (foldMap f $ M.toList $ M.mapWithKey h $ M.filterWithKey g $
                               Env.energyMap $ Env.signal env)
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

-- ###################################
-- Delta Calculation

{-

{- |
Symbol equipped with a numeric value.
-}
data
   Symbol =
      Symbol {
         index :: Idx.Record Idx.Delta (Var.Index System.Node),
         value :: Double
      }

instance Eq Symbol where
   (==)  =  equating index

instance Ord Symbol where
   compare  =  comparing index

-}

{-
infixr 6 =<>

(=<>) ::
   (Eq (term Symbol), Num (term Symbol), Ord (t System.Node),
    Var.MkVarC term, Var.MkIdxC t, Env.AccessMap t) =>
   (Idx.Record Idx.Delta (t System.Node), Double) ->
   EqGen.EquationSystem Env.Delta System.Node s (term Symbol) ->
   EqGen.EquationSystem Env.Delta System.Node s (term Symbol)
(idx, x) =<> eqsys =
   (idx .= Var.mkVarCore (Symbol (fmap Var.mkIdx idx) x)) <> eqsys

-}


makeGivenForDifferentialAnalysis ::  Env.Env System.Node (Env.Delta (EqGen.Result Double)) -> 
                                     EqGen.EquationSystem Env.Delta System.Node s (SumProduct.Term (HSt.Symbol  System.Node))
                                     
makeGivenForDifferentialAnalysis env = (
  Idx.before (Idx.DTime Idx.initSection) .= 1)
  <> (Idx.delta (Idx.DTime Idx.initSection) .= 0)
  <> (Idx.before (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.before (Var.mkIdx $ Idx.Storage (Idx.SecNode Idx.initSection System.Battery)),
                              HSt.value = initStorage}))
  <> (Idx.delta (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.delta (Var.mkIdx $ Idx.Storage (Idx.SecNode Idx.initSection System.Battery)),
                              HSt.value = 0}))
  <> (Idx.before (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.before (Var.mkIdx $ Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)),
                                                                   HSt.value = 0}))
  <> (Idx.delta (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 
     SumProduct.Atom (HSt.Symbol{HSt.index = Idx.delta (Var.mkIdx $ Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)),
                             HSt.value = 0}))
  <> (fold $ concat $ map f (M.toList (Env.etaMap env)))
  <> (fold $ concat $ map f (M.toList (Env.dtimeMap env)))
  <> (fold $ concat $ map f (M.toList $ M.filterWithKey g $ Env.energyMap env))
  where
    f (i, x)  =  [(Idx.before i) .= SumProduct.Atom (HSt.Symbol{HSt.index = (Idx.before $ Var.mkIdx i), 
                                                            HSt.value =  (h $ Env.before x)}),
                  (Idx.delta i) .= SumProduct.Atom (HSt.Symbol{HSt.index = (Idx.delta $ Var.mkIdx i), 
                                                           HSt.value = (h $ Env.delta x)})]
    h (EqGen.Determined x) = x            

    g (Idx.Energy (Idx.SecNode _ x) (Idx.SecNode _ y)) _ =
       case (x,y) of
         (System.Resistance, System.Chassis) -> True
         (System.VehicleInertia, System.Chassis) -> True
         (System.RearBrakes, System.Chassis) -> True
         (System.FrontBrakes, System.ConFrontBrakes) -> True
         (System.ConES, System.ElectricSystem) -> True
         (System.Battery, System.ConBattery) -> True
         _ -> False
    
{-  
given ::
   EqGen.EquationSystem Env.Delta Node.Int s
      (SumProduct.Term Symbol)
given =
   (Idx.delta (Idx.DTime Idx.initSection) .= 0) <>
   (Idx.delta (Idx.DTime sec0) .= 0) <>

   (Idx.before (Idx.DTime Idx.initSection) .= 1) <>
   (Idx.before (Idx.DTime sec0) .= 1) <>

   (Idx.before (edgeVar Idx.Energy sec0 node0 node1), 4) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node0 node1), 0.25) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node1 node2), 0.85) =<>

   (Idx.delta (edgeVar Idx.Energy sec0 node0 node1), -0.6) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node0 node1), 0.1) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node1 node2), 0.05) =<>
-}  
