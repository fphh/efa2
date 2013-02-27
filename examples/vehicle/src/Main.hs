{-# LANGUAGE FlexibleContexts #-}

module Main where

---------------------------------------------------------------------------------------
-- * Import other Modules

import EFA.Example.Utility (edgeVar)
import EFA.Equation.Absolute ((.=))
import qualified EFA.Equation.System as EqGen
--import EFA.IO.CSVImport (modelicaCSVImport)
import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, removeZeroNoise,getTimeWindow)
import EFA.Signal.Sequence (makeSeqFlowTopology,genSequenceSignal,chopAtZeroCrossingsPowerRecord,
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections,removeZeroTimeSections, genSequ,sectionRecordsFromSequence)
import qualified EFA.Signal.Signal as Sig -- (toList,UTSigL,setType)
import qualified EFA.Signal.Plot as Pl
import qualified EFA.Report.Report as Rep
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw

import EFA.Graph.Topology(isOriginalEdge)
import EFA.Graph(lefilter)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (fold)


----------------------------------
-- * Example Specific Imports


import qualified Modules.System as System (topology, flowStates,Node(Battery,Tank,VehicleInertia))

-- Signal Treatment
import Modules.Signals as Signals (condition,calculatePower)
-- Plotting
import Modules.Plots as Plot

----------------------------------
-- * Here Starts the Real Program

main :: IO ()
main = do

---------------------------------------------------------------------------------------
-- * Show Topology

  Draw.topology System.topology
  -- Draw.topology2pdf System.topology
 
---------------------------------------------------------------------------------------
-- * Show State Analysis Results

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

--------------------------------------------------------------------------------------- 
-- * Import signals from Csv-file
  
--  rawSignals <- modelicaCSVImport "Vehicle_res.csv" :: IO (SignalRecord [] Double)
  rawSignals <- modelicaPLTImport "Vehicle_res.plt" :: IO (SignalRecord [] Double)
  
--  Plot.recordSplit 9 ("Imported Signals",rawSignals)

--------------------------------------------------------------------------------------- 
-- * Condition Signals and Calculate Powers
  let signals = Signals.condition rawSignals 
--   let powerSignals = removeZeroNoise (Signals.calculatePower signals) (0) --(10^^(-12::Int))    
  let powerSignals = removeZeroNoise (Signals.calculatePower signals) (10^^(-2::Int)) 
      
--------------------------------------------------------------------------------------- 
-- * Add zerocrossings in Powersignals and Signals
  let powerSignals0 = addZeroCrossings powerSignals   

  -- Rep.report [] ("Time",(getTime powerSignals0))   
  let signals0 = newTimeBase signals (getTime powerSignals0) 
  

--------------------------------------------------------------------------------------- 
-- * Plot Signals
{-  
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
-}
---------------------------------------------------------------------------------------
-- * Cut Signals and filter Time Sektions
  
  let sequencePowersRaw :: SD.SequData (PowerRecord System.Node [] Double)      
      (sequenceRaw,sequencePowersRaw) = genSequ powerSignals0
      
  -- Alternative preferred Method -- aktually disabled because sequ isn't part of output    
  -- (_,sequencePowersRaw) =  genSequ powerSignals0 -- chopAtZeroCrossingsPowerRecord powerSignals0 
  --  sequencePowersRaw = chopAtZeroCrossingsPowerRecord powerSignals0
   
  -- Rep.report [] ("SequenceRaw", sequenceRaw)    
  -- putStrLn $ show (fmap getTimeWindow sequencePowersRaw)
      
  -- Rep.report [] ("sequencePowersRaw",  sequencePowersRaw)   
  
  -- filter for Modelica-specific steps or remove time section with low time duration
  let (sequ,sequencePowers) = removeLowTimeSections(sequenceRaw,sequencePowersRaw) 1
  --  let (sequ,sequencePowers) = removeZeroTimeSections(sequenceRaw,sequencePowersRaw)
      
  Rep.report [] ("Sequence", sequ)
  
  let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
      
  let sequenceSignals = sectionRecordsFromSequence signals0 sequ 
  
--  Pl.recordSplitPlus 1 "Mit SektionsSignal" powerSignals0 [(PPosIdx System.Tank System.Tank, Sig.setType sequSig)]   

  -- Rep.report [Rep.RAll,Rep.RVertical] ("Powers0", powerSignals0)
  
---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on Energy
  
  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequ,sequencePowers,sequenceFlows) 0 
        
  Rep.report [] ("SequenceFilt", sequenceFilt)      
  
---------------------------------------------------------------------------------------
-- * Report Record Data
  
  --Rep.report [] ("Sequenz",sequ)    
  -- Rep.report [] ("SequencePowerRecord", sequencePowers)
  -- Rep.report [] ("SequencePowerRecord", sequenceFlowsFilt)

---------------------------------------------------------------------------------------
-- *  Provide solver with Given Variables, Start Solver and generate Sequence Flow Graph

  let makeGiven initStorage sequenceFlwsFilt =
        (Idx.DTime Idx.initSection .= 1)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.Battery) .= initStorage)
        <> (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia) .= 0)
        <> fold (SD.zipWithSecIdxs f sequenceFlwsFilt)
        where f sec (Record t xs) =
                (Idx.DTime sec .= sum (Sig.toList t)) <>
                fold (M.mapWithKey g xs)
                where g (PPosIdx a b) e =
                         edgeVar Idx.Energy sec a b .= sum (Sig.toList e)

---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph

  let flowStates = fmap Flow.genFlowState sequenceFlowsFilt
  let flowTopos = Flow.genSequFlowTops System.topology flowStates
  let adjustedFlows = Flow.adjustSigns System.topology flowStates sequenceFlowsFilt
  let sequenceFlowTopology = makeSeqFlowTopology flowTopos

  -- Draw.sequFlowGraph sequenceFlowTopology

---------------------------------------------------------------------------------------
-- *  Solve System
      
      
  -- solve complete system from given variables 
  -- let solverResult =
  --       EqGen.solve (makeGiven 12.34567 adjustedFlows)
  --                   sequenceFlowTopology

  -- analyse Measurement
  let solverMeasurements =
        EqGen.solveFromMeasurement (makeGiven (0.7*3600*1000) adjustedFlows)
                                   sequenceFlowTopology
  let sectionTopos =  lefilter (isOriginalEdge .fst) sequenceFlowTopology

  -- draw various plots
  concurrentlyMany_ [
    -- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverResult,
    -- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverMeasurements
    Draw.sequFlowGraphAbsWithEnv sectionTopos solverMeasurements
    ]


  

