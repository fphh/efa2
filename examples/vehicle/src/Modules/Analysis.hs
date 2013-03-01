module Modules.Analysis where

import EFA.Example.Utility (edgeVar,Term)
import EFA.Equation.Absolute ((.=))
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

  
-- base ::  -> Env.Env System.Node (Env.Absolute (EqGen.Result Double))
base sequenceFlowTopology sequenceFlows = EqGen.solveFromMeasurement (makeGiven (0.7*3600*1000) sequenceFlows)
                                   sequenceFlowTopology
  where
      makeGiven :: Double -> 
                   SD.SequData (FlowRecord System.Node [] Double)->
                   (EqGen.EquationSystem Env.Absolute System.Node s Double)

      makeGiven initStorage sf =
        (Idx.DTime Idx.initSection .= 1)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.Battery) .= initStorage)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia) .= 0)
        <> fold (SD.zipWithSecIdxs f sf)
        where f sec (Record t xs) =
                (Idx.DTime sec .= sum (Sig.toList t)) <>
                fold (M.mapWithKey g xs)
                where g (PPosIdx a b) e =
                         edgeVar Idx.Energy sec a b .= sum (Sig.toList e)
                         
                         
-- predict ::  Env.Env System.Node (Env.Absolute (EqGen.Result Double))
predict sequenceFlowTopology baseEnvs  = EqGen.solve (makeGivenForPrediction (0.7*3600*1000) baseEnvs)
                      sequenceFlowTopology
  where
    makeGivenForPrediction :: Double ->
                            Env.Env System.Node (Env.Absolute (EqGen.Result Double)) ->
                            (EqGen.EquationSystem Env.Absolute System.Node s Double)
    makeGivenForPrediction initStorage env =
        (Idx.DTime Idx.initSection .= 1)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.Battery) .= initStorage)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia) .= 0)
        <> (foldMap f (M.toList (Env.etaMap env)))
        <> (foldMap f (M.toList (Env.dtimeMap env)))
        <> (foldMap f (map h $ filter g $ M.toList (Env.energyMap env)))
        where -- f ((Env.Absolute (EqGen.Determined i)),x) = i .= x            --   EqGen.Determined 
              f (i,Env.Absolute (EqGen.Determined x)) = i .= x 
              g ((Idx.Energy (Idx.SecNode _ System.Resistance) (Idx.SecNode _ System.Chassis)),_) = True
              g ((Idx.Energy (Idx.SecNode _ System.VehicleInertia) (Idx.SecNode _ System.Chassis)),_) = True
              g ((Idx.Energy (Idx.SecNode _ System.RearBrakes) (Idx.SecNode _ System.Chassis)),_) = True
              g ((Idx.Energy (Idx.SecNode _ System.FrontBrakes) (Idx.SecNode _ System.ConFrontBrakes)),_) = True              
              g ((Idx.Energy (Idx.SecNode _ System.ConES) (Idx.SecNode _ System.ElectricSystem)),_) = True              
              g ((Idx.Energy (Idx.SecNode _ System.Battery) (Idx.SecNode _ System.ConBattery)),_) = True
              g _  = False
              h ((Idx.Energy (Idx.SecNode s1 System.Resistance) (Idx.SecNode s2 System.Chassis)),Env.Absolute (EqGen.Determined x)) = 
                ((Idx.Energy (Idx.SecNode s1 System.Resistance) (Idx.SecNode s2 System.Chassis)),Env.Absolute (EqGen.Determined (x*1.1)))
              h (i,r) = (i,r)

{-
delta sequenceFlowTopology envA envB = EqGen.solve (given) sequenceFlowTopology
  where 

    given:: (EqGen.EquationSystem Env.Delta System.Node s Double)
    given = env2Given envA <>
            env2Given envB
                        
    env2Given :: Env.Env System.Node (Env.Absolute (EqGen.Result Double)) -> (EqGen.EquationSystem Env.Absolute System.Node s Double)
    env2Given env = -- foldMap (f M.toList) env 
                 (Idx.DTime Idx.initSection .= 1)
                 <> (foldMap f (M.toList (Env.dtimeMap env)))
                 <> (foldMap f (M.toList (Env.etaMap env)))
                 <> (foldMap f (M.toList (Env.energyMap env)))
                 <> (foldMap f (M.toList (Env.powerMap env)) )  
                 <> (foldMap f (M.toList (Env.xMap env)))    
                 <> (foldMap f (M.toList (Env.yMap env)))
                 <> (foldMap f (M.toList (Env.maxEnergyMap env)))
                 <> (foldMap f (M.toList (Env.storageMap env)))
                 <> (foldMap f (M.toList (Env.sumMap env)))
                 where    
                   f (i,Env.Absolute (EqGen.Determined x)) = i .= x

-}   