{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}


module Main where

import qualified Modules.System as System
import qualified Modules.Analysis as Analysis
import qualified Modules.Signals as Signals

import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Absolute as EqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Record as RecSeq
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Sequence as Sequ
import EFA.Signal.Signal (TC, Scalar, toScalar)
import EFA.Signal.Data (Data, Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.Record as EqRecord

import EFA.IO.PLTImport (modelicaPLTImport)
import EFA.Utility.Async (concurrentlyMany_)


--import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO
import System.Environment (getEnv)
import System.FilePath ((</>))

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Control.Monad (when)
import Data.Monoid (mempty)


-- | O. Generelle Settings

examplePath :: FilePath
examplePath = "examples/vehicle"

colours :: [Colors.X11Color]
colours = [ Colors.White,
            Colors.Gray90,
            Colors.Gray80,
            Colors.Gray70 ]

zeroNoiseTolerance :: Double
zeroNoiseTolerance = 10^^(-2::Int)


-- List of Operation Point Plots
xyList :: [(String,(Record.SigId,Record.SigId))]
xyList = [
  ("Engine", Signals.xyEngine),
  ("Generator", Signals.xyGenerator),
  ("Motor", Signals.xyMotor)
  ]

-- List of Signal Plots
plotList :: [(String,[Record.SigId])]
plotList = [
  ("Vehicle", Signals.vehicle),
  ("DriveLine", Signals.driveline),
  ("Electric", Signals.electric),
  ("Motor", Signals.motor),
  ("Engine", Signals.engine),
  ("Generator", Signals.generator),
  ("Battery", Signals.battery)
  ]

-- List of Operation Point Plots
etaList :: [(String,(TopoIdx.Position System.Node,TopoIdx.Position System.Node,TopoIdx.Position System.Node))]
etaList = [
  ("Engine and Generator", Signals.etaEngineGenerator),
  ("Motor and Gearbox", Signals.etaMotorGearbox)
 ]

-- plotTerm :: X11.T
-- plotTerm = X11.cons

plotTerm :: WXT.T
plotTerm = WXT.cons

-- | A. Generator steht

{-
fileNamesX :: [FilePath]
fileNamesX = ["Vehicle_mass900kg_res.plt",
             "Vehicle_mass1000kg_res.plt",
             "Vehicle_mass1100kg_res.plt"]

datasetsX ::  [Record.Name]
datasetsX = map Record.Name ["900kg",
                            "1000kg",
                            "1100kg"]

deltasetsX :: [Record.DeltaName]
deltasetsX = zipWith Record.deltaName datasetsX (tail datasetsX)


sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
sectionFilterTime = toScalar 0

sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
sectionFilterEnergy = toScalar 1000

recordStackRow_filterEnergy :: Double
recordStackRow_filterEnergy = (1^^(-6::Integer))

sectionStackRow_filterEnergy :: Double
sectionStackRow_filterEnergy = (10^(5::Integer))

cumStack_filterEnergy :: Double
cumStack_filterEnergy = (1^^(-1::Integer))

energyIndexSec :: SeqIdx.Energy System.Node
energyIndexSec = Idx.InPart (Idx.Section 7) energyIndex

energyIndex :: TopoIdx.Energy System.Node
energyIndex  = TopoIdx.energy System.Tank System.ConBattery

sectionMapping :: [Sequ.List a] -> [Sequ.List a]
sectionMapping = map (Sequ.reIndex [1,2,7,8,16,17,19::Int])


-- | B. Generator läuft am Anfang kurz

fileNamesX :: [FilePath]
fileNamesX = ["Vehicle_mass900kg_V2_res.plt",
             "Vehicle_mass1000kg_V2_res.plt",
             "Vehicle_mass1100kg_V2_res.plt"]

datasetsX ::  [Record.Name]
datasetsX = map Record.Name ["900kg",
                            "1000kg",
                            "1100kg"]

deltasetsX :: [Record.DeltaName]
deltasetsX = zipWith Record.deltaName datasetsX (tail datasetsX)


sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
sectionFilterTime = toScalar 0.1

sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
sectionFilterEnergy = toScalar 1000

recordStackRow_filterEnergy :: Double
recordStackRow_filterEnergy = (1^^(-6::Integer))

sectionStackRow_filterEnergy :: Double
sectionStackRow_filterEnergy = (10^(5::Integer))

cumStack_filterEnergy :: Double
cumStack_filterEnergy = (1^^(-1::Integer))

energyIndexSec :: SeqIdx.Energy System.Node
energyIndexSec = Idx.InPart (Idx.Section 8) energyIndex

energyIndex :: TopoIdx.Energy System.Node
energyIndex  = TopoIdx.energy System.Tank System.ConBattery

{-
sectionMapping :: [Int]
sectionMapping = [1,2,7,8,16,17,19]
-}

sectionMapping :: [Sequ.List a] -> [Sequ.List a]
sectionMapping = id


-}
-- | C. Generator läuft am Anfang kurz und startet dann wieder mitten drin

fileNamesX :: [FilePath]
fileNamesX = ["Vehicle_mass900kg_V3_res.plt",
             "Vehicle_mass1000kg_V3_res.plt",
             "Vehicle_mass1100kg_V3_res.plt"]

datasetsX ::  [Record.Name]
datasetsX = map Record.Name ["900kg",
                            "1000kg",
                            "1100kg"]

deltasetsX :: [Record.DeltaName]
deltasetsX = zipWith Record.deltaName datasetsX (tail datasetsX)


sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
sectionFilterTime = toScalar 0.1

sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
sectionFilterEnergy = toScalar 1000

recordStackRow_filterEnergy :: Double
recordStackRow_filterEnergy = (1^^(-6::Integer))

sectionStackRow_filterEnergy :: Double
sectionStackRow_filterEnergy = (10^(5::Integer))

cumStack_filterEnergy :: Double
cumStack_filterEnergy = (1^^(-1::Integer))

energyIndexSec :: SeqIdx.Energy System.Node
energyIndexSec = Idx.InPart (Idx.Section 18) energyIndex

energyIndex :: TopoIdx.Energy System.Node
--energyIndex  = TopoIdx.energy System.Tank System.ConBattery
energyIndex  = TopoIdx.energy System.Battery System.ConBattery

sectionMapping :: [Sequ.List a] -> [Sequ.List a]
sectionMapping = map (Sequ.reIndex [8,11,13,14,18,32,37::Int])


--------------------------------------------------------------------

main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8

---------------------------------------------------------------------------------------
-- * State Analysis

  when False $ do
    let flowStates :: [Topo.FlowTopology System.Node]
        flowStates = StateAnalysis.advanced System.topology
    putStrLn ("Number of possible flow states: " ++ show (length flowStates))
    Draw.xterm $ Draw.flowTopologies $ take 20 flowStates

---------------------------------------------------------------------------------------
-- * Import signals from Csv-file

  path <- fmap (</> examplePath) $ getEnv "EFADATA"
  rawSignalsX <- mapM modelicaPLTImport $ map (path </>) fileNamesX

---------------------------------------------------------------------------------------
-- * Conditioning, Sequencing and Integration

  let (_,flowToposUnmappedX,powerSignalsX,signalsX) =
        List.unzip4 $
        map (Analysis.pre System.topology zeroNoiseTolerance sectionFilterTime sectionFilterEnergy) rawSignalsX

  let _allSignalsX = zipWith (Record.combinePowerAndSignalWithFunction System.convertPowerId) powerSignalsX signalsX

---------------------------------------------------------------------------------------
-- *  ReIndex Sequences to allow Sequence Matching

  let flowToposX = sectionMapping flowToposUnmappedX

---------------------------------------------------------------------------------------
-- *  Make Base Analysis on external Data

  let externalEnvX =
         map
            (SeqFlow.mapGraph
               (checkDetermined "external scalar")
               (checkDetermined "external signal") .
             Analysis.external .
             RecSeq.flowGraphFromSequence)
            flowToposX
  let _externalSignalEnvX =
         map
            (\flowTopos ->
               EqAbs.solveOpts
                  (EqAbs.independentInOutSums EqAbs.optionsDefault)
                  (RecSeq.flowGraphFromSequence flowTopos) mempty)
            flowToposX

  ---------------------------------------------------------------------------------------
-- *  Make the Deltas for subsequent Datasets

  let externalDeltaEnvX =
        ListHT.mapAdjacent
           (SeqFlow.checkedZipWithGraph "externalDeltaEnvX"
              EqRecord.deltaCons EqRecord.deltaCons)
           externalEnvX
 ---------------------------------------------------------------------------------------
-- *  Make the Prediction

  let prediction = Analysis.prediction (head externalEnvX)

  -- Hier gehts schief, wenn ich mit Signalen rechnen will
--  let prediction2 = Analysis.prediction (head externalSignalEnvX)

---------------------------------------------------------------------------------------
-- *  Make difference Analysis

  let differenceExtEnvs = map Analysis.difference externalDeltaEnvX

---------------------------------------------------------------------------------------
-- * Draw Diagrams

  let -- drawDelta :: RecordName ->
      drawDelta (Record.DeltaName ti) c =
          Draw.dot (ti ++ "vehicle_delta.dot") .
          Draw.title ti .
          Draw.bgcolour c .
          Draw.seqFlowGraph
             (Draw.absoluteVariable Draw.optionsDefault)
      drawAbs (Record.Name ti) c =
          Draw.dot (ti++"vehicle.dot").
          Draw.title ti .
          Draw.bgcolour c .
          Draw.seqFlowGraph
             (Draw.deltaVariable Draw.optionsDefault)



  concurrentlyMany_ $ [
    -- Topologie
    --Draw.topology System.topology,
    --  Draw.topology2pdf System.topology
    --Draw.labeledTopology System.labeledTopology,
    ]

{-
--  ++ [putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))]
    ++ [Draw.xterm $ Draw.flowTopologies (take 20 System.flowStates)]

---------------------------------------------------------------------------------------
-- * Plot Time Signals

    ++ [mapM_ (Plots.sigsWithSpeed plotTerm (zip datasetsX allSignalsX)) plotList]

  -- Plots.sigsWithSpeed allSignalsX (head plotList)
  --  Plot.recordIO "Test" (head allSignalsX)
-}
---------------------------------------------------------------------------------------
-- * Plot Operation Points

    -- ++ [mapM_ (Plots.operation "Operation Points -" plotTerm id (zip datasetsX allSignalsX)) xyList]

---------------------------------------------------------------------------------------
-- * Plot Efficiency Curves and Distributions

    ++ [mapM_ (PlotIO.etaDistr1DimfromRecordList "Average Efficiency Curve -" 10000 5000
               (zip datasetsX (map (Record.diffTime . Record.partIntegrate) powerSignalsX))) etaList]

---------------------------------------------------------------------------------------
-- * Draw Diagrams

    -- Section flow
    ++ List.zipWith3 drawAbs
         datasetsX
         colours
         externalEnvX

    -- Delta Section Flow
    ++ List.zipWith3 drawDelta
         deltasetsX
         (tail colours)
         externalDeltaEnvX

---------------------------------------------------------------------------------------
-- * Plot Stacks
{-
    -- Record Stack Row at specific position
    ++ [Plot.recordStackRowIO
         ("Energy Flow Change at " ++ show energyIndexSec)
         deltasetsX
         energyIndexSec
         recordStackRow_filterEnergy
         differenceExtEnvs]

    -- Section stack row at given ppos for a defined record
    ++ [Plot.sectionStackRowIO
        "Energy Flow Change at Tank in all Sections 1100 vs 1000"
        energyIndex
        sectionStackRow_filterEnergy
        (last differenceExtEnvs)]

--    ++ [print $ Plots.lookupAllStacks energyIndex (last differenceExtEnvs)]
--    ++ [print $ Plots.lookupStack energyIndexSec (last differenceExtEnvs)]
-}
    -- overall stack at given position
    ++ [PlotIO.aggregatedStack
           ("Cumulative Flow Change at  " ++ show energyIndex)
           energyIndex
           cumStack_filterEnergy
           (head differenceExtEnvs)]


{-     ++ [mapM_ (Plot.stackIOfromEnv  "Energy Flow Change at Tank in Section 6"
         (SeqIdx.energy (Idx.Section 6) System.Tank System.ConBattery) 1)
         (zip deltasetsX differenceExtEnvs)]-}
{-
     ++ [print $    -- AssignMap.threshold 0.001 $
--             Map.mapKeys AssignMap.deltaIndexSet $
--             Stack.assignDeltaMap $
             Plots.lookupCumStack energyIndex (last differenceExtEnvs)]
-}


---------------------------------------------------------------------------------------
-- * Draw Predicted Diagram

    -- Prediction Based on a specific Record
    ++ [drawAbs (Record.Name "Prediction 900kg") Colors.Yellow prediction]
