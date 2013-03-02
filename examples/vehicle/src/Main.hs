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
import Modules.Signals as Signals 
import Modules.Plots as Plot 
import Modules.Analysis as Analysis

----------------------------------
-- * Here Starts the Real Program

main :: IO ()
main = do

---------------------------------------------------------------------------------------
-- * Show Topology

  Draw.topology System.topology
  -- Draw.topology2pdf System.topology
 
---------------------------------------------------------------------------------------
-- * State Analysis

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

--------------------------------------------------------------------------------------- 
-- * Import signals from Csv-file
  
--  rawSignals <- modelicaCSVImport "Vehicle_res.csv" :: IO (SignalRecord [] Double)
  rawSignals <- modelicaPLTImport "Vehicle_res.plt" :: IO (SignalRecord [] Double)
  rawSignalsB <- modelicaPLTImport "Vehicle_mass1200kg_res.plt" :: IO (SignalRecord [] Double)
  
--------------------------------------------------------------------------------------- 
-- * Conditioning, Sequencing and Integration
  (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt,flowStates) <- Analysis.pre System.topology rawSignals 
  (sequenceFiltB,sequencePowersFiltB,sequenceFlowsFiltB,flowStatesB) <- Analysis.pre System.topology rawSignalsB 

  ---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph

  let flowTopos = Flow.genSequFlowTops System.topology flowStates
  let flowToposB = Flow.genSequFlowTops System.topology flowStatesB

  let sequenceFlowTopology = makeSeqFlowTopology flowTopos
  let sequenceFlowTopologyB = makeSeqFlowTopology flowToposB
  
  let sectionTopos =  lefilter (isOriginalEdge .fst) sequenceFlowTopology    
  let sectionToposB =  lefilter (isOriginalEdge .fst) sequenceFlowTopologyB    

  let simulation = Analysis.base sequenceFlowTopology sequenceFlowsFilt
  let simulationB = Analysis.base sequenceFlowTopology sequenceFlowsFiltB
      
  let prediction = Analysis.predict sequenceFlowTopology simulation

  --  let predictionB = Analysis.predict sequenceFlowTopology simulationB
      
--  let deltaSimulation_AB = Analysis.delta sequenceFlowTopology simulation simulationB


  -- draw various diagrams
  concurrentlyMany_ [
-- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverResult,
--    Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverMeasurements,
    Draw.sequFlowGraphAbsWithEnv sectionTopos simulation,
    Draw.sequFlowGraphAbsWithEnv sectionTopos simulationB

--    Draw.sequFlowGraphAbsWithEnv sectionTopos prediction
    
    ]


  

