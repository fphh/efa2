{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Main where

---------------------------------------------------------------------------------------
-- * Import other Modules

import EFA.Example.Utility (edgeVar)
import EFA.Equation.Absolute ((.=))
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Arithmetic as Arith
--import EFA.IO.CSVImport (modelicaCSVImport)
import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, getTimeWindow)
import EFA.Signal.Sequence (makeSeqFlowTopology,genSequenceSignal,chopAtZeroCrossingsPowerRecord,
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections,removeZeroTimeSections, genSequ,sectionRecordsFromSequence)
import qualified EFA.Signal.Signal as Sig -- (toList,UTSigL,setType)
import qualified EFA.Signal.Plot as Pl
import EFA.Signal.Typ

import qualified EFA.Report.Report as Rep
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw

import EFA.Graph.Topology(isStructureEdge)
import EFA.Graph(lefilter)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (fold, foldMap)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Result as Result
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Equation.Variable as Var

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import Control.Functor.HT (void)
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Data.Foldable as Fold
import qualified EFA.Utility as Ut

import qualified EFA.Hack.Plot as HPl
import qualified EFA.Hack.Stack as HSt
import qualified EFA.Hack.Env as HEn
-- import qualified EFA.Hack.Draw as HDr



----------------------------------
-- * Example Specific Imports

import qualified Modules.System as System
import Modules.Signals as Signals
import Modules.Plots as Plot
import Modules.Analysis as Analysis

import System.IO

dataset = "Vehicle_res_5000sampels.plt"
datasetB = "Vehicle_res.plt"

main :: IO ()
main = do
  
  hSetEncoding stdout utf8

---------------------------------------------------------------------------------------
-- * Show Topology

  -- Draw.topology System.topology
  -- Draw.topology2pdf System.topology

---------------------------------------------------------------------------------------
-- * State Analysis

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

---------------------------------------------------------------------------------------
-- * Import signals from Csv-file

--  rawSignals <- modelicaCSVImport "Vehicle_res.csv" :: IO (SignalRecord [] Double)

--  rawSignals <- modelicaPLTImport "Vehicle_res.plt" :: IO (SignalRecord [] Double)
--  rawSignalsB <- modelicaPLTImport "Vehicle_mass1200kg_res.plt" :: IO (SignalRecord [] Double)

  rawSignals <- modelicaPLTImport dataset -- :: IO (SignalRecord [] Double)
  rawSignalsB <- modelicaPLTImport datasetB -- :: IO (SignalRecord [] Double) -- "Vehicle_mass1200kg_res.plt" :: IO (SignalRecord [] Double)
  
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

  let sectionTopos =  lefilter (isStructureEdge .fst) sequenceFlowTopology
  let sectionToposB =  lefilter (isStructureEdge .fst) sequenceFlowTopologyB

  let solve ::
         (Eq a, Arith.Product a, Arith.Integrate a, Arith.Scalar a ~ a,
          EqGen.Record rec) =>
         (forall s. EqGen.EquationSystem rec System.Node s a a) ->
         Env.Complete System.Node (rec (Result.Result a)) (rec (Result.Result a))
      solve = EqGen.solveFromMeasurement sequenceFlowTopology
      solveB = EqGen.solveFromMeasurement sequenceFlowTopologyB

  let simulation =
         solve $ Analysis.makeGiven Idx.Absolute sequenceFlowsFilt
  let simulationB =
         solveB $ Analysis.makeGiven Idx.Absolute sequenceFlowsFiltB
  let simulationDelta =
         solve $
            (Analysis.makeGiven Idx.Before sequenceFlowsFilt <>
             Analysis.makeGiven Idx.After sequenceFlowsFiltB)

  let prediction =
         EqGen.solve sequenceFlowTopology (Analysis.makeGivenForPrediction Idx.Absolute simulation) 



--  @Henning -- please help here
 
  let difference = 
        EqGen.solve sequenceFlowTopology (Analysis.makeGivenForDifferentialAnalysis simulationDelta) 
        
  Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env

  let eout :: Idx.Energy System.Node
      eout = edgeVar Idx.Energy (Idx.Section 4)  System.ConBattery System.Battery

  HPl.histogrammIO (HSt.evaluate $ HEn.lookupStack difference eout) eout

       

  Draw.topologyWithEdgeLabels System.edgeNames System.topology
  -- Draw.topology System.topology




  -- draw various diagrams
  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv dataset sectionTopos simulation,
    Draw.sequFlowGraphAbsWithEnv datasetB sectionToposB simulationB,
    Draw.sequFlowGraphDeltaWithEnv "delta" sectionTopos simulationDelta
--    Draw.sequFlowGraphAbsWithEnv sectionTopos prediction
    ]

