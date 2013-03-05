{-# LANGUAGE Rank2Types #-}
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

----------------------------------
-- * Example Specific Imports

import qualified Modules.System as System 
import Modules.Signals as Signals 
import Modules.Plots as Plot 
import Modules.Analysis as Analysis

import System.IO
----------------------------------
-- * Here Starts the Real Program

histogram ::
   (Fold.Foldable f, FormatValue term) =>
   f (term, Double) -> Frame.T (Graph2D.T Int Double)
histogram =
   Frame.cons (
      Opts.title "Decomposition of total output energy" $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.xTicks2d [(Format.unASCII $ formatValue $
                      Idx.delta $ Var.mkIdx eout, 0)] $
      Opts.xRange2d (-1,3) $
      Opts.deflt) .
   foldMap (\(term,val) ->
      fmap (Graph2D.lineSpec
              (LineSpec.title (Format.unASCII $ formatValue term) LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms [val])


-- Energy (SecNode (Section 1) Chassis) (SecNode (Section 1) Resistance)

eout :: Idx.Energy System.Node
eout = edgeVar Idx.Energy (Idx.Section 4)  System.Tank System.ConBattery


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
  rawSignals <- modelicaPLTImport "Vehicle_res.plt" :: IO (SignalRecord [] Double)
  rawSignalsB <- modelicaPLTImport "Vehicle_res.plt" :: IO (SignalRecord [] Double) -- "Vehicle_mass1200kg_res.plt" :: IO (SignalRecord [] Double)
  
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

  let solve ::
         (Eq a, Fractional a, EqGen.Record rec) =>
         (forall s. EqGen.EquationSystem rec System.Node s a) ->
         Env.Env System.Node (rec (Result.Result a))
      solve = flip EqGen.solveFromMeasurement sequenceFlowTopology

  let simulation =
         solve $ Analysis.makeGiven Idx.Absolute sequenceFlowsFilt
  let simulationB =
         solve $ Analysis.makeGiven Idx.Absolute sequenceFlowsFiltB
  let simulationDelta =
         solve $
            (Analysis.makeGiven Idx.Before sequenceFlowsFilt <>
             Analysis.makeGiven Idx.After sequenceFlowsFiltB)

--  let prediction =
--         solve $ Analysis.makeGivenForPrediction Idx.Absolute simulation
      
  let prediction =
         EqGen.solve (Analysis.makeGivenForPrediction Idx.Absolute simulation) sequenceFlowTopology
  
  let difference = 
        EqGen.solve (Analysis.makeGivenForDifferentialAnalysis simulationDelta) sequenceFlowTopology
        
  --  let predictionB = Analysis.predict sequenceFlowTopology simulationB


    

  
  case M.lookup eout (Env.energyMap difference) of
      Nothing -> error ("undefined E_0_1" ++ Ut.myShowList (M.keys (Env.energyMap difference)))
      Just d ->
         case Env.delta d of
            Result.Undetermined -> error "undetermined E_0_1"
            Result.Determined x -> do
               let assigns =
                      fmap
                         (\symbol ->
                            (fmap index symbol,
                             Op.evaluate value symbol)) $
                      Op.group $ Op.expand $ Op.fromNormalTerm x
               Fold.forM_ assigns $ \(term,val) -> do
                  putStrLn $
                     (Format.unUnicode $ formatValue term) ++ " = " ++ show val
               void $ GP.plotDefault $ histogram assigns
{-
   Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env
-}

  -- draw various diagrams
  concurrentlyMany_ [
-- Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverResult,
--    Draw.sequFlowGraphAbsWithEnv sequenceFlowTopology solverMeasurements,
    Draw.sequFlowGraphAbsWithEnv sectionTopos simulation,
    Draw.sequFlowGraphAbsWithEnv sectionTopos simulationB,
    Draw.sequFlowGraphDeltaWithEnv sectionTopos simulationDelta

--    Draw.sequFlowGraphAbsWithEnv sectionTopos prediction

    ]



