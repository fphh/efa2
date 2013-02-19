{-# LANGUAGE FlexibleContexts #-}

module Main where

---------------------------------------------------------------------------------------
-- * Import other Modules

import Data.Foldable (foldMap)
import qualified Data.Map as M
import EFA.Example.Utility (edgeVar, (.=))
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.System as EqGen
--import EFA.IO.CSVImport (modelicaCSVImport)
import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, removeZeroNoise)
import EFA.Signal.Sequence (makeSeqFlowTopology,genSequenceSignal,chopAtZeroCrossingsPowerRecord, 
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections, genSequ,sectionRecordsFromSequence)
import qualified EFA.Signal.Signal as Sig -- (toList,UTSigL,setType)
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Report.Report as Rep
import qualified EFA.Graph.Draw as Draw
import Data.Monoid ((<>))
import qualified EFA.Graph.Flow as Flow

import EFA.Graph.Topology(isOriginalEdge)
import EFA.Graph(lefilter)


----------------------------------
-- * Example Specific Imports


import qualified Modules.System as System (topology, flowStates,Node(Battery,Tank))

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

--  Draw.topology System.topology
 
---------------------------------------------------------------------------------------
-- * Show State Analysis Results

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

--------------------------------------------------------------------------------------- 
-- * Import signals from Csv-file
  
--  rawSignals <- modelicaCSVImport "Vehicle_res.csv" :: IO (SignalRecord [] Double)
  rawSignals <- modelicaPLTImport "Vehicle_res_noInertias.plt" :: IO (SignalRecord [] Double)
  
--  Plot.recordSplit 9 ("Imported Signals",rawSignals)

--------------------------------------------------------------------------------------- 
-- * Condition Signals and Calculate Powers
  let signals = Signals.condition rawSignals 
  let powerSignals = removeZeroNoise (Signals.calculatePower signals) (0) --(10^^(-12::Int)) 
      
--------------------------------------------------------------------------------------- 
-- * Add zerocrossings in Powersignals and Signals
  let powerSignals0 = addZeroCrossings powerSignals   

  -- Rep.report [] ("Time",(getTime powerSignals0))   
  let signals0 = newTimeBase signals (getTime powerSignals0) 
  
{-
--------------------------------------------------------------------------------------- 
-- * Plot Signals
  
  Plot.vehicle signals0
  Plot.motor signals0
  Plot.generator signals0
  Plot.driveline signals0
  Plot.electric signals0
  Plot.battery signals0
  
--------------------------------------------------------------------------------------- 
-- * Plot Power Signals
  
  Plot.genPowers powerSignals0   
  Plot.propPowers powerSignals0
  Plot.vehPowers powerSignals0
-}
---------------------------------------------------------------------------------------
-- * Cut Signals and filter Time Sektions
  
  let (sequenceRaw,sequencePowersRawB) = genSequ powerSignals0
      
  let sequencePowersRaw :: SD.SequData (PowerRecord System.Node [] Double)      
      sequencePowersRaw = chopAtZeroCrossingsPowerRecord powerSignals0 
      
  
  -- filter for Modelica-specific steps or remove time section with low time duration
  let (sequ,sequencePowers) = removeLowTimeSections(sequenceRaw,sequencePowersRaw) 0
  let sequSig = Sig.scale (genSequenceSignal sequ) (10  ^^ (-12::Int)) :: Sig.UTSigL   
      
  let sequenceSignals = sectionRecordsFromSequence signals0 sequ 
--  let powerSignals0' = addSignal powerSignals0 (PPosIdx System.Tank System.Tank, Sig.setType sequSig)    
  
-- PL.rPlotSplitPlus 1 ("Mit SektionsSignal",powerSignals0) [(PPosIdx System.Tank System.Tank, Sig.setType sequSig)]   

  -- Rep.report [Rep.RAll,Rep.RVertical] ("Powers0", powerSignals0)
  
---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on Energy
  
  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequ,sequencePowers,sequenceFlows) 0 

  
---------------------------------------------------------------------------------------
-- * Report Record Data
  
  --Rep.report [] ("Sequenz",sequ)    
  -- Rep.report [] ("SequencePowerRecord", sequencePowers)
  -- Rep.report [] ("SequencePowerRecord", sequenceFlowsFilt)

---------------------------------------------------------------------------------------
-- *  Provide solver with Given Variables, Start Solver and generate Sequence Flow Graph     
   
  let makeGiven initStorage sequenceFlwsFilt =
        (EqGen.dtime Idx.initSection .= 1)  
        <> (EqGen.storage (Idx.SecNode Idx.initSection System.Battery) .= initStorage) 
        <> foldMap f (zip [Idx.Section 0 ..] ds)
        where SD.SequData ds =  fmap f2 sequenceFlwsFilt
              f2 (Record t xs) =
                (sum $ Sig.toList t, M.toList $ M.map (sum . Sig.toList) xs)      
              f (sec, (dt, es)) =
                (EqGen.dtime sec .= dt) <> foldMap g es
                where g (PPosIdx a b, e) = (edgeVar EqGen.energy sec a b .= e)
      
---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph     
   
  let flowStates = fmap Flow.genFlowState sequenceFlowsFilt
  let flowTopos = Flow.genSequFlowTops System.topology flowStates
  let adjustedFlows = Flow.adjustSigns System.topology flowStates sequenceFlowsFilt
  let sequenceFlowTopology = makeSeqFlowTopology flowTopos
        
  -- Draw.sequFlowGraph sequenceFlowTopology
       
---------------------------------------------------------------------------------------
-- *  Solve System
      
  -- let solverResult =
  --       EqGen.solve (makeGiven 12.34567 adjustedFlows)
  --                   sequenceFlowTopology

  let solverMeasurements =
        EqGen.solveFromMeasurement (makeGiven 12.34567 adjustedFlows)
                                   sequenceFlowTopology
  let sectionTopos =  lefilter (isOriginalEdge .fst) sequenceFlowTopology

  concurrentlyMany_ [
    -- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverResult,
    
    -- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverMeasurements
    Draw.sequFlowGraphAbsWithEnv sectionTopos solverMeasurements
    --Draw.flowTopologies ((\(SD.SequData fs) -> fs) flowTopos)
    ]

  

