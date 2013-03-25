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
import qualified Data.List as L
import Data.Tuple.HT (mapFst)


path :: FilePath
path = "../../../../../data/examples/vehicle/"

datasetsX:: [FilePath]
datasetsX = [path ++ "Vehicle_res.plt",
             path ++ "Vehicle_mass1050kg_res.plt"]

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
  
  let (sequenceFiltX,sequencePowersFiltX,sequenceFlowsFiltX,flowStatesX,powerSignalsX,signalsX) = L.unzip6 preProcessedDataX

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
  
  
  let (Env.Complete scalEnv sigEnv) = head differenceExtEnvs

{-
  Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env
-}

  let eout =  Idx.Energy (Idx.StructureEdge (Idx.Section 10) System.Tank System.ConBattery)

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

          let  assignsFilt = filter (\(_,x)-> (abs x) >= 1) assigns
--          print assignsFilt


          Fold.forM_ assignsFilt $ \(term,val) -> do
            putStrLn $ Format.unUnicode $
              Format.assign (formatValue term) (formatValue val)

          Plot.stackIO "Decomposition of total output energy at Tank Section 4"
              (Idx.delta $ Var.index eout) $ assignsFilt



---------------------------------------------------------------------------------------
-- * Plot Signals



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
                O.leadSignalsMax (Record.RangeFrom Signals.vehicle, Record.ToModify [Record.SigId "speedsensor1.v"]) .
                O.pointSize 1) (HPlot.RecList allSignalsX)
{-
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
-}
{-

  HPlot.record2 (O.title "Sequence" .
               O.split (O.Split 5)) (HPlot.Sq sequencePowersFiltB)


  HPlot.record2 (O.title "SequenceList" . O.extract Signals.vehPowers)
               (HPlot.SqList [sequencePowersFilt, sequencePowersFiltB])



  HPlot.record2 (O.title "Sequence vs Record" . O.extract Signals.vehPowers)
             (HPlot.RecSq powerSignals sequencePowersFilt)
-}

---------------------------------------------------------------------------------------
-- * Draw Diagrams
  

  concurrentlyMany_ [
--  Draw.topology System.topology
--  Draw.topology2pdf System.topology
--  Draw.topologyWithEdgeLabels System.edgeNames System.topology
    zipWith3M_ Draw.sequFlowGraphAbsWithEnv datasetsX sectionToposX externalEnvX,
--    Draw.sequFlowGraphAbsWithEnv datasetB sectionToposB simulationB
    zipWith3M_ Draw.sequFlowGraphDeltaWithEnv datasetsX sectionToposX externalDeltaEnvX,
    Draw.sequFlowGraphAbsWithEnv "Prediction" (head sectionToposX) prediction
    ]


