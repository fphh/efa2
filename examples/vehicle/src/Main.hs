{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Main where

---------------------------------------------------------------------------------------
-- * Import other Modules

-- import EFA.Example.Utility (edgeVar)
-- import EFA.Example.Absolute ((.=))
-- import qualified EFA.Equation.Sstem as EqGen
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
-- import qualified Modules.Signals as Signals

--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Example.AssignMap as AssignMap
-- import qualified EFA.Signal.Plot as Plot
import qualified EFA.Graph.Topology.Index as Idx
--import qualified EFA.Equation.Environment as Env

-- import qualified EFA.Equation.Record as EqRecord
--import qualified EFA.Equation.Result as Result

--import qualified EFA.Equation.Stack as Stack
--import qualified EFA.Equation.Variable as Var
--import qualified Data.Foldable as Fold
--import qualified Data.NonEmpty as NonEmpty
--import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Signal.Record as Record
import EFA.Signal.Signal (TC(..), Scalar,toScalar)
import EFA.Signal.Data (Data(..), Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import qualified System.IO as IO
import System.Environment (getEnv)
import System.FilePath ((</>))

--import qualified Data.Map as M
import qualified Data.List as L
import Data.Tuple.HT (mapSnd)
--import qualified EFA.Example.Index as XIdx

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import EFA.Report.FormatValue(RecordName(..),
                              DeltaName(..), 
                              deltaName
                              --         ,                              
                              --formatValue
                             )

examplePath :: FilePath
examplePath = "examples/vehicle"


fileNamesX :: [FilePath]
fileNamesX = [-- "Vehicle_mass900kg_res.plt",
             "Vehicle_mass1000kg_res.plt",
             "Vehicle_mass1100kg_res.plt"]

datasetsX ::  [RecordName]
datasetsX = map RecordName [--"900kg",
                            "1000kg",
                            "1100kg"]

deltasetsX :: [DeltaName]
deltasetsX = zipWith deltaName datasetsX (tail datasetsX)

zipWith3M_ ::
  Monad m =>
  (t -> t1 -> t2 -> m b) -> [t] -> [t1] -> [t2] -> m ()
zipWith3M_ f x y z = mapM_ (\(x',y',z') -> f x' y' z') (zip3 x y z)

main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8

---------------------------------------------------------------------------------------
-- * State Analysis

--  putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))
--  Draw.flowTopologies (take 20 System.flowStates)

---------------------------------------------------------------------------------------
-- * Import signals from Csv-file

  path <- fmap (</> examplePath) $ getEnv "EFADATA"
  rawSignalsX <- mapM modelicaPLTImport $ map (path </>) fileNamesX

---------------------------------------------------------------------------------------
-- * Conditioning, Sequencing and Integration
  
  let epsT ::  TC Scalar (Typ A T Tt) (Data Nil Double)
      epsT = toScalar 0

      epsE ::  TC Scalar (Typ A F Tt) (Data Nil Double)
      epsE = toScalar 1000

  let preProcessedDataX = map (Analysis.pre System.topology epsT epsE) rawSignalsX

  let (_,sequenceFlowsFiltX,flowStatesX,powerSignalsX,signalsX) = L.unzip5 preProcessedDataX
--  let (sequencePowersFiltX,sequenceFlowsFiltX,flowStatesX,powerSignalsX,signalsX) = L.unzip5 preProcessedDataX

  let allSignalsX = zipWith Record.combinePowerAndSignal powerSignalsX signalsX

---------------------------------------------------------------------------------------
-- *  Generate Flow States as Graphs

  let flowToposX = map (Flow.genSequFlowTops System.topology) flowStatesX

---------------------------------------------------------------------------------------
-- *  Generate Sequence Flow Graph

  let sequenceFlowTopologyX = map makeSeqFlowTopology flowToposX

---------------------------------------------------------------------------------------
-- *  Section Flow States as Graphs

  let sectionToposX =  map (mapSnd (lefilter (isStructureEdge .fst))) sequenceFlowTopologyX

---------------------------------------------------------------------------------------
-- *  Make Base Analysis on external Data

  let externalEnvX = zipWith Analysis.external  sequenceFlowTopologyX sequenceFlowsFiltX

---------------------------------------------------------------------------------------
-- *  Make the Deltas for subsequent Datasets

--  let externalDeltaEnvX =
--        zipWith (flip Analysis.delta (head sequenceFlowsFiltX))
--                      sequenceFlowTopologyX $ tail sequenceFlowsFiltX


  let externalDeltaEnvX =
        L.zipWith3  Analysis.delta sequenceFlowTopologyX
        sequenceFlowsFiltX
        (tail sequenceFlowsFiltX)


 ---------------------------------------------------------------------------------------
-- *  Make the Prediction

  let prediction = Analysis.prediction (head sequenceFlowTopologyX) (head externalEnvX)

---------------------------------------------------------------------------------------
-- *  Make difference Analysis

  let differenceExtEnvs = zipWith Analysis.difference sequenceFlowTopologyX externalDeltaEnvX

---------------------------------------------------------------------------------------
-- * Plot Stacks

{-
  mapM_ (Plots.stack  "Energy Flow Change at Tank in Section 6"
         (XIdx.energy (Idx.Section 6) System.Tank System.ConBattery) 1 )
    (zip (deltasets datasetsX) differenceExtEnvs)
-}




  let energyIndex7 = Idx.InSection (Idx.Section 7) energyIndex
      energyIndex  = Idx.Energy $ Idx.StructureEdge System.Tank System.ConBattery

--  print $ Plots.lookupStack energyIndex7 (last differenceExtEnvs)

  Plots.recordStackRow
    "Energy Flow Change at Tank in Section 7"
    deltasetsX
    energyIndex7
    (1^^(-6::Integer))
    differenceExtEnvs
    
  Plots.sectionStackRow
    "Energy Flow Change at Tank in all Sections 1100 vs 1000"
    energyIndex
    (10^(5::Integer))
    (last differenceExtEnvs)


--  print $ Plots.lookupAllStacks energyIndex (last differenceExtEnvs)


     
  Plots.cumStack
    "Cumulative Flow Change at Tank"
    energyIndex
    (1^^(-1::Integer))
    (head differenceExtEnvs)
   
    
  print $    -- AssignMap.threshold 0.001 $
--             M.mapKeys AssignMap.deltaIndexSet $
--             Stack.assignDeltaMap $    
             Plots.lookupCumStack energyIndex (last differenceExtEnvs)
  
---------------------------------------------------------------------------------------
-- * Plot Time Signals

{-
  let plotList = [
                  ("Vehicle", Signals.vehicle),
                  ("DriveLine", Signals.driveline),
                  ("Electric", Signals.electric),
                  ("Motor", Signals.motor),
                  ("Generator", Signals.generator),
                  ("Battery", Signals.battery)
                 ]

  mapM_ (Plots.sigsWithSpeed allSignalsX) plotList

  -- Plots.sigsWithSpeed allSignalsX (head plotList)
  Plot.recordIO "Test" (head allSignalsX)
-}

---------------------------------------------------------------------------------------
-- * Plot Operation Points
{-
  let xyList = [
                  ("Engine", Signals.xyEngine),
                  ("Generator", Signals.xyGenerator),
                  ("Motor", Signals.xyMotor)
                 ]

  mapM_ (Plots.operation "Operation Points -" (zip datasetsX allSignalsX)) xyList

-}
---------------------------------------------------------------------------------------
-- * Plot Efficiency Curves and Distributions




---------------------------------------------------------------------------------------
-- * Draw Diagrams
  
  let -- drawDelta :: RecordName -> 
      drawDelta ti topo env c = 
          Draw.xterm $
          Draw.title  ((\(DeltaName x) -> x) ti) $
          Draw.bgcolour c $
          Draw.sequFlowGraphDeltaWithEnv topo env
      drawAbs ti topo env c = 
        Draw.xterm $
          Draw.title ((\(RecordName x) -> x) ti) $
          Draw.bgcolour c $
          Draw.sequFlowGraphAbsWithEnv topo env

      colours = [ Colors.White,	 
                  Colors.Gray90,	 
                  Colors.Gray80,	 
                  Colors.Gray70 ]


  concurrentlyMany_ $ [
    -- Topologie
--    Draw.topology System.topology, --  Draw.topology2pdf System.topology
--    Draw.topologyWithEdgeLabels System.edgeNames System.topology,

    -- Sectionen
--    zipWith3M_ Draw.sequFlowGraphAbsWithEnv datasetsX sequenceFlowTopologyX externalEnvX,
--    concurrentlyMany_ $
--      L.zipWith3 Draw.sequFlowGraphAbsWithEnv
--                 (L.zipWith3 Draw.xterm datasetsX sectionToposX externalEnvX)

    -- Sections-Deltadiagramme
--    zipWith3M_ Draw.sequFlowGraphDeltaWithEnv (deltasets datasetsX) sequenceFlowTopologyX externalDeltaEnvX
{-
    concurrentlyMany_ $
      L.zipWith3 Draw.sequFlowGraphDeltaWithEnv 
                 (deltasets datasetsX) sectionToposX externalDeltaEnvX
-}
    -- Vorhersage
--    Draw.sequFlowGraphAbsWithEnv "Prediction" (head sequenceFlowTopologyX) prediction
    ]

    ++ L.zipWith4 drawAbs
         datasetsX
         sectionToposX
         externalEnvX
         colours
    ++ L.zipWith4 drawDelta
         deltasetsX
         sectionToposX
         externalDeltaEnvX
         (tail colours)
