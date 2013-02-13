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
import EFA.IO.CSVImport (modelicaCSVImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord,
                          Record(Record), 
                          SignalRecord)
import EFA.Signal.Sequence (makeSequenceRaw, makeSeqFlowTopology,
                            removeZeroTimeSections, 
                            removeLowEnergySections, genSequFlow)
import qualified EFA.Signal.Signal as Sig (toList)
import qualified EFA.Report.Report as Rep
import qualified EFA.Graph.Draw as Draw
import Data.Monoid ((<>))
import qualified EFA.Graph.Flow as Flow

----------------------------------
-- * Example Specific Imports

import qualified Modules.System as System (topology, flowStates,Nodes(Battery))
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
 
---------------------------------------------------------------------------------------
-- * Show State Analysis Results

  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
  Draw.flowTopologies (take 20 System.flowStates)

--------------------------------------------------------------------------------------- 
-- * Import signals from Csv-file
  
  rawSignals <- modelicaCSVImport "Vehicle_res.csv" :: IO (SignalRecord [] Double)
  
--  PL.rPlotSplit 9 ("Imported Signals",rawSignals)

--------------------------------------------------------------------------------------- 
-- * Condition Signals
  let signals = Signals.condition rawSignals 
  let powerSignals = Signals.calculatePower signals 
      
--------------------------------------------------------------------------------------- 
-- * Add zerocrossings in Powersignals and Signals
  let powerSignals0 =     
      
      
  
--  PL.rPlotSplit 9 ("Conditioned Signals", signals)
  
  
  Plot.vehicle signals
--  Plot.engine signals
  Plot.motor signals
  Plot.generator signals
  Plot.driveline signals
  Plot.electric signals
  Plot.battery signals
  
--------------------------------------------------------------------------------------- 
-- * Calculate Powers
  
  Plot.genPowers powerSignals   
  Plot.propPowers powerSignals
  Plot.vehPowers powerSignals
  
--  PL.rPlotSplit 9 ("Conditioned Signals", signals)
  
---------------------------------------------------------------------------------------
-- * Cut Signals and filter Time Sektions
  
  let (sequenceRaw,sequencePowersRaw) = makeSequenceRaw powerSignals
  
  let (sequenc,sequencePowers) =
        removeZeroTimeSections (sequenceRaw,sequencePowersRaw)
      
---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on Energy
  
  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequenc,sequencePowers,sequenceFlows) (0) 

  --Rep.report [] ("Sequenz",sequenc)    
  -- Rep.report [] ("SequencePowerRecord", sequencePowers)
  Rep.report [] ("SequencePowerRecord", sequenceFlowsFilt)

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
      


  concurrentlyMany_ [
    -- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverResult,
    Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverMeasurements
    --Draw.flowTopologies ((\(SD.SequData fs) -> fs) flowTopos)
    ]

  

