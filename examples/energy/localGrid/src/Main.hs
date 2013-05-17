module Main where

import qualified Modules.Signals as Signals
import qualified Modules.System as System

import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
import EFA.IO.PLTImport (modelicaPLTImport)
import EFA.Signal.Signal (TC(..), Scalar,toScalar)
import EFA.Signal.Data (Data(..), Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)
import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
import EFA.Graph(lefilter)
import qualified EFA.Signal.Plot as Plot
import EFA.Graph.Topology(isStructureEdge)
import qualified EFA.Example.Index as XIdx
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as Env

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO
import System.Environment (getEnv)
import System.FilePath ((</>))


import qualified Data.List as L
--import qualified Data.Map as M
import qualified Modules.Analysis as Analysis
import Data.Tuple.HT (mapSnd)


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

examplePath :: FilePath
examplePath = "examples/energy/localGrid"

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

zeroNoiseToleranz :: Double
zeroNoiseToleranz = 10^^(-2::Int)

fileNamesX :: [FilePath]

fileNamesX = ["powerNetwork_res.plt",
              "powerNetworkETA_res.plt"]

datasetsX ::  [Record.Name]
datasetsX = map Record.Name ["Base",
                             "ETA"]

deltasetsX :: [Record.DeltaName]
deltasetsX = zipWith Record.deltaName datasetsX (tail datasetsX)

sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
sectionFilterTime = toScalar 0.1

sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
sectionFilterEnergy = toScalar 1000

-- List of Signal Plots
plotList :: [(String,[Record.SigId])]
plotList = [
  ("coal", Signals.coal),
  ("gas", Signals.gas),
  ("wind", Signals.wind),
  ("water", Signals.water),
  ("Solar", Signals.solar),
  ("industry", Signals.industry),
  ("house", Signals.house)
  ]

etaList :: [(String,(XIdx.PPos System.Node,XIdx.PPos System.Node,XIdx.PPos System.Node))]
etaList = [
  ("Coal", Signals.etaCoal),
  ("Gas", Signals.etaGas),
  ("Wind", Signals.etaWind),
  ("Water", Signals.etaWater),
  ("Solar", Signals.etaSolar),
  ("Transformer", Signals.etaTransformer)
 ]

sectionMapping :: [SD.SequData a] -> [SD.SequData a]
sectionMapping = id -- map (SD.reIndex [8,11,13,14,18,32,37::Int])


main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8

  path <- fmap (</> examplePath) $ getEnv "EFADATA"
  rawSignalsX <- mapM modelicaPLTImport $ map (path </>) fileNamesX :: IO ([Record.SignalRecord [] Double])

  let preProcessedDataX =
        map (Analysis.pre System.topology zeroNoiseToleranz sectionFilterTime sectionFilterEnergy) rawSignalsX

      (_,sequenceFlowsFiltUnmappedX,flowStatesUnmappedX,powerSignalsX,signalsX) =
        L.unzip5 preProcessedDataX

      allSignalsX = zipWith (Record.combinePowerAndSignalWithFunction System.convertPowerId) powerSignalsX signalsX

      sequenceFlowsFiltX = sectionMapping sequenceFlowsFiltUnmappedX

      flowStatesX = sectionMapping flowStatesUnmappedX

      flowToposX = map (Flow.genSequFlowTops System.topology) flowStatesX

      sequenceFlowTopologyX = map makeSeqFlowTopology flowToposX

      sectionToposX =  map (mapSnd (lefilter (isStructureEdge .fst))) sequenceFlowTopologyX

      externalEnvX = zipWith Analysis.external  sequenceFlowTopologyX sequenceFlowsFiltX
      externalSignalEnvX = zipWith Analysis.external2  sequenceFlowTopologyX sequenceFlowsFiltX

      externalDeltaEnvX =
        L.zipWith3  Analysis.delta sequenceFlowTopologyX
        sequenceFlowsFiltX
        (tail sequenceFlowsFiltX)

---------------------------------------------------------------------------------------
-- * State Analysis
  let Record.Record _ sigs = head rawSignalsX
--  putStrLn $ Utility.myShowList $ M.keys sigs

  let -- drawDelta :: RecordName ->
      drawDelta ti topo env c =
          Draw.xterm $
          Draw.title  ((\(Record.DeltaName x) -> x) ti) $
          Draw.bgcolour c $
          Draw.sequFlowGraphDeltaWithEnv topo env
      drawAbs ti topo env c =
        Draw.xterm $
          Draw.title ((\(Record.Name x) -> x) ti) $
          Draw.bgcolour c $
          Draw.sequFlowGraphAbsWithEnv topo env

  concurrentlyMany_ $ [
    -- Topologie
    Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNames System.topology
    ]

---------------------------------------------------------------------------------------
-- * Draw flow states
{-
    ++ [putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))]
    ++ [Draw.xterm $ Draw.flowTopologies (take 20 System.flowStates)]

---------------------------------------------------------------------------------------
-- * Plot Time Signals

--    ++ Plots.sigsWithSpeed allSignalsX (head plotList)
    ++ [Plot.recordIO "Test" plotTerm show id (head rawSignalsX)]

    ++ [mapM_ (\(x,y) -> Plot.recordIOList_extract ("Signals of Component " ++ x)
                         plotTerm show id (zip datasetsX allSignalsX) y) plotList]

---------------------------------------------------------------------------------------
-- * Plot Efficiency Curves and Distributions

    ++ [mapM_ (Plot.etaDistr1DimIOfromRecordList "Average Efficiency Curve -" 10000 5000
               (zip datasetsX (map (Record.diffTime . Record.partIntegrate) powerSignalsX))) etaList]
-}
---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [head $ L.zipWith4 drawAbs
         datasetsX
         sectionToposX
         externalEnvX
         colours]
{-
---------------------------------------------------------------------------------------
-- * Draw Delta Section flows

    ++ L.zipWith4 drawDelta
         deltasetsX
         sectionToposX
         externalDeltaEnvX
         (tail colours)
-}
---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [head $ L.zipWith4 drawAbs
         [Record.Name "Signal"]
         sectionToposX
         (map (\(Env.Complete scal sig) -> Env.Complete scal (fmap (fmap (fmap Arith.integrate)) sig)) externalSignalEnvX)
         colours]
