{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Main where

---------------------------------------------------------------------------------------
-- * Import other Modules

-- import EFA.Example.Utility (edgeVar)
-- import EFA.Example.Absolute ((.=))
-- import qualified EFA.Equation.System as EqGen
import EFA.IO.PLTImport (modelicaPLTImport)
import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)
import EFA.Graph.Topology(isStructureEdge)
import EFA.Graph(lefilter)
import EFA.Utility.Async (concurrentlyMany_)


import qualified EFA.Hack.Plot as HPlot

----------------------------------
-- * Example Specific Imports

import qualified Modules.System as System
import qualified Modules.Analysis as Analysis
-- import qualified Modules.Plots as Plots
import qualified Modules.Signals as Signals

import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Plot.Options as O
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Environment as Env

import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Variable as Var
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Signal.Record as Record

import System.IO

import qualified Data.Map as M
import Data.Tuple.HT (mapFst)

dataset, datasetB :: FilePath
dataset =  "/home/felix/data/examples/vehicle/Vehicle_res.plt"
datasetB = "/home/felix/data/examples/vehicle/Vehicle_mass1050kg_res.plt"

main :: IO ()
main = do
  
  hSetEncoding stdout utf8


---------------------------------------------------------------------------------------
-- * State Analysis

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

---------------------------------------------------------------------------------------
-- * Import signals from Csv-file

  rawSignals <- modelicaPLTImport dataset 
  rawSignalsB <- modelicaPLTImport datasetB 
  
--------------------------------------------------------------------------------------- 
-- * Conditioning, Sequencing and Integration
  
  (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt,flowStates,powerSignals,signals) <- Analysis.pre System.topology rawSignals
  (sequenceFiltB,sequencePowersFiltB,sequenceFlowsFiltB,flowStatesB,powerSignalsB,signalsB) <- Analysis.pre System.topology rawSignalsB
  
  let allSignals = Record.combinePowerAndSignal powerSignals signals
  let allSignalsB = Record.combinePowerAndSignal powerSignalsB signalsB

---------------------------------------------------------------------------------------
-- *  Generate Flow States as Graphs
  
  let flowTopos = Flow.genSequFlowTops System.topology flowStates
  let flowToposB = Flow.genSequFlowTops System.topology flowStatesB

---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph
      
  let sequenceFlowTopology = makeSeqFlowTopology flowTopos
  let sequenceFlowTopologyB = makeSeqFlowTopology flowToposB

---------------------------------------------------------------------------------------
-- *  Section Flow States as Graphs
  
  let sectionTopos =  lefilter (isStructureEdge .fst) sequenceFlowTopology
  let sectionToposB =  lefilter (isStructureEdge .fst) sequenceFlowTopologyB
      
---------------------------------------------------------------------------------------
-- *  Make Base Analysis on external Data

  let simulation = Analysis.external sequenceFlowTopology sequenceFlowsFilt
  let simulationB = Analysis.external sequenceFlowTopologyB sequenceFlowsFiltB
      
---------------------------------------------------------------------------------------
-- *  Make the Delta Dataset
      
  let simulationDelta = Analysis.delta sequenceFlowTopology sequenceFlowsFilt sequenceFlowsFiltB  
      
 ---------------------------------------------------------------------------------------
-- *  Make the Prediction
     
  let prediction = Analysis.prediction  sequenceFlowTopology simulation

---------------------------------------------------------------------------------------
-- *  Make difference Analysis

  let difference = Analysis.difference sequenceFlowTopology simulationDelta
   
  let (Env.Complete scalEnv sigEnv) = difference
      
{-  
  Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env
-}
  
  let eout =  Idx.Energy (Idx.StructureEdge (Idx.Section 4) System.Tank System.ConBattery)
  let stack = (Env.energyMap (sigEnv)) M.! eout
      
  case M.lookup eout (Env.energyMap sigEnv) of
    Nothing -> error "undefined E"
    Just d ->
      case EqRecord.unAbsolute d of
        Result.Undetermined -> error "undetermined E"
        Result.Determined x -> do
          let assigns =
                fmap (mapFst (foldl (\p i -> p * SumProduct.Atom i) 1)) $
                NonEmpty.tail $
                Stack.assigns x
          Fold.forM_ assigns $ \(term,val) -> do
            putStrLn $ Format.unUnicode $
              Format.assign (formatValue term) (formatValue val)
          Plot.stackIO "Decomposition of total output energy"
              (Idx.delta $ Var.index eout) assigns
                    
                    
              
              
  -- Plot.stackIO  "Test" stack formatValue

  -- putStrLn $ Format.unUnicode $ formatValue difference
  -- putStrLn $ Format.unUnicode $ formatValue stack

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- * Show Topology

--  Draw.topology System.topology
--  Draw.topology2pdf System.topology
--  Draw.topologyWithEdgeLabels System.edgeNames System.topology
     
       
--  Plots.plotPowers System.powerPositonNames ["A","B"] [powerSignals,powerSignalsB] Signals.vehPowers

{-  
  HPlot.record2 (O.title "Record-Split" . 
                O.split (O.Split 9) . 
                O.showId System.showPowerId .
                O.pointSize 2) (powerSignals)

-}
  HPlot.record2 (O.title "VehicleSignals" . 
                -- O.split (O.Split 9) . 
--                O.showId System.showPowerId .
                O.extract Signals.vehicle .
--                O.norm .
                O.leadSignalsMax (Record.RangeFromAll, Record.ToModify [Record.SigId "speedsensor1.v"]) .
                O.pointSize 1) (HPlot.RecList [allSignals, allSignalsB])

  HPlot.record2 (O.title "Record-AB" . 
                -- O.split (O.Split 9) . 
                O.showId System.showPowerId .
                O.extract Signals.vehPowers .
--                O.norm .
                O.leadSignalsMax (Record.RangeFromAll, Record.ToModify [Record.PPosIdx System.Tank System.ConBattery]) .
                O.pointSize 1) (HPlot.RecList [powerSignals, powerSignalsB])


  HPlot.record2 (O.title "Vehicle Signals" . 
                -- O.split (O.Split 9) . 
--                O.showId System.showPowerId .
                O.extract Signals.vehicle .
                O.norm True .
--                O.leadSignals (Record.RangeFromAll, Record.ToModify [Record.SigId "speedsensor1.v"]) .
                O.pointSize 1) (HPlot.RecList [signals, signalsB])

{-

  HPlot.record2 (O.title "Sequence" . 
               O.split (O.Split 5)) (HPlot.Sq sequencePowersFiltB) 

  
  HPlot.record2 (O.title "SequenceList" . O.extract Signals.vehPowers) 
               (HPlot.SqList [sequencePowersFilt, sequencePowersFiltB]) 

  
  
  HPlot.record2 (O.title "Sequence vs Record" . O.extract Signals.vehPowers) 
             (HPlot.RecSq powerSignals sequencePowersFilt)
-}   

  -- draw various diagrams
  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv dataset sectionTopos simulation,
--    Draw.sequFlowGraphAbsWithEnv datasetB sectionToposB simulationB
    Draw.sequFlowGraphDeltaWithEnv "delta" sectionTopos simulationDelta,
    Draw.sequFlowGraphAbsWithEnv "Prediction" sectionTopos prediction
    ]


