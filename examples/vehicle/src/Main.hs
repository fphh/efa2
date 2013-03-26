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
-- import qualified EFA.Report.Format as Format
-- import EFA.Report.FormatValue (formatValue)
import EFA.Graph.Topology(isStructureEdge)
import EFA.Graph(lefilter)
import EFA.Utility.Async (concurrentlyMany_)

----------------------------------
-- * Example Specific Imports

import qualified Modules.System as System
import qualified Modules.Analysis as Analysis
import qualified Modules.Plots as Plots
import qualified Modules.Signals as Signals

-- import qualified EFA.Signal.Plot as Plot
import qualified EFA.Graph.Topology.Index as Idx
-- import qualified EFA.Equation.Environment as Env

-- import qualified EFA.Equation.Record as EqRecord
--import qualified EFA.Equation.Result as Result

--import qualified EFA.Equation.Stack as Stack
--import qualified EFA.Equation.Variable as Var
--import qualified Data.Foldable as Fold
--import qualified Data.NonEmpty as NonEmpty
--import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Signal.Record as Record

import System.IO

--import qualified Data.Map as M
import qualified Data.List as L
--import Data.Tuple.HT (mapFst)


path :: FilePath
path = "../../../../../data/examples/vehicle/"

datasetsX:: [FilePath]
datasetsX = [path ++ "Vehicle_res.plt",
             path ++ "Vehicle_mass1050kg_res.plt"]


deltasets :: [String]  ->   [String]
deltasets xs = zipWith (\x y -> y ++ "_vs_" ++ x) xs (tail xs)


zipWith3M_ :: Monad m => (t -> t1 -> t2 -> m b) -> [t] -> [t1] -> [t2] -> m ()
zipWith3M_ f x y z = mapM_ (\(x',y',z') -> f x' y' z') (zip3 x y z)


main :: IO ()
main = do

  hSetEncoding stdout utf8

---------------------------------------------------------------------------------------
-- * State Analysis

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

---------------------------------------------------------------------------------------
-- * Import signals from Csv-file

  rawSignalsX <- mapM modelicaPLTImport datasetsX

---------------------------------------------------------------------------------------
-- * Conditioning, Sequencing and Integration

  preProcessedDataX <- mapM (Analysis.pre System.topology) rawSignalsX

  let (_,_,sequenceFlowsFiltX,flowStatesX,powerSignalsX,signalsX) = L.unzip6 preProcessedDataX
--  let (sequenceFiltX,sequencePowersFiltX,sequenceFlowsFiltX,flowStatesX,powerSignalsX,signalsX) = L.unzip6 preProcessedDataX

  let allSignalsX = zipWith Record.combinePowerAndSignal powerSignalsX signalsX

---------------------------------------------------------------------------------------
-- *  Generate Flow States as Graphs

  let flowToposX = map (Flow.genSequFlowTops System.topology) flowStatesX

---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph

  let sequenceFlowTopologyX = map makeSeqFlowTopology flowToposX

---------------------------------------------------------------------------------------
-- *  Section Flow States as Graphs

  let sectionToposX =  map (lefilter (isStructureEdge .fst)) sequenceFlowTopologyX

---------------------------------------------------------------------------------------
-- *  Make Base Analysis on external Data

  let externalEnvX = zipWith Analysis.external  sequenceFlowTopologyX sequenceFlowsFiltX

---------------------------------------------------------------------------------------
-- *  Make the Deltas for subsequent Datasets

  let externalDeltaEnvX = zipWith (flip Analysis.delta  (head sequenceFlowsFiltX)) sequenceFlowTopologyX $ tail sequenceFlowsFiltX

 ---------------------------------------------------------------------------------------
-- *  Make the Prediction

  let prediction = Analysis.prediction (head sequenceFlowTopologyX) (head externalEnvX)

---------------------------------------------------------------------------------------
-- *  Make difference Analysis

  let differenceExtEnvs = zipWith Analysis.difference sequenceFlowTopologyX externalDeltaEnvX

---------------------------------------------------------------------------------------
-- * Plot Stacks
 
  mapM_ (Plots.stack  "Energy Flow Change at Tank in Section 4"
         (Idx.Energy (Idx.StructureEdge (Idx.Section 4) System.Tank System.ConBattery))) 
    (zip (deltasets datasetsX) differenceExtEnvs)

---------------------------------------------------------------------------------------
-- * Plot Time Signals


  let plotList = [
                  ("Vehicle", Signals.vehicle),
                  ("DriveLine", Signals.driveline),
                  ("Electric", Signals.electric),
                  ("Motor", Signals.motor),
                  ("Generator", Signals.generator),
                  ("Battery", Signals.battery)
                 ]

  mapM_ (Plots.sigsWithSpeed allSignalsX) plotList


---------------------------------------------------------------------------------------
-- * Plot Operation Points



---------------------------------------------------------------------------------------
-- * Draw Diagrams

  concurrentlyMany_ [
    -- Topologie
--  Draw.topology System.topology --  Draw.topology2pdf System.topology
--  Draw.topologyWithEdgeLabels System.edgeNames System.topology

    -- Sectionen
    zipWith3M_ Draw.sequFlowGraphAbsWithEnv datasetsX sectionToposX externalEnvX,

    -- Sections-Deltadiagramme
    zipWith3M_ Draw.sequFlowGraphDeltaWithEnv datasetsX sectionToposX externalDeltaEnvX,

    -- Vorhersage
    Draw.sequFlowGraphAbsWithEnv "Prediction" (head sectionToposX) prediction
    ]


