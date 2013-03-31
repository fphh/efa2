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

-- import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Plot.Options as O


import System.IO

import Data.Tuple.HT (mapSnd)


dataset, datasetB :: FilePath
dataset = "/home/felix/data/examples/vehicle/Vehicle_res.plt"
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
  
  (sequencePowersFilt,sequenceFlowsFilt,flowStates,powerSignals,signals) <- Analysis.pre System.topology rawSignals
  (sequencePowersFiltB,sequenceFlowsFiltB,flowStatesB,powerSignalsB,signalsB) <- Analysis.pre System.topology rawSignalsB

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
  
  let sectionTopos = mapSnd (lefilter (isStructureEdge .fst)) sequenceFlowTopology
  let sectionToposB =  mapSnd (lefilter (isStructureEdge .fst)) sequenceFlowTopologyB
      
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

  --  @Henning -- please help here

  {- 
  let difference = Analysis.difference sequenceFlowTopology env     

  Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env

  let eout :: Idx.Energy System.Node
      eout = edgeVar Idx.Energy (Idx.Section 4)  System.ConBattery System.Battery

  HPl.histogrammIO (HSt.evaluate $ HEn.lookupStack difference eout) eout

-}
      
      
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
  HPlot.record2 (O.title "Record-AB" . 
                -- O.split (O.Split 9) . 
                O.showId System.showPowerId .
                O.extract Signals.vehPowers .
                O.pointSize 1) (HPlot.RecList [powerSignals,powerSignalsB])

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


