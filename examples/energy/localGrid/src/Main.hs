module Main where

import qualified Modules.Signals as Signals
import qualified Modules.System as System
import qualified Modules.Analysis as Analysis

import qualified EFA.Example.Index as XIdx
import EFA.Example.Utility (checkDetermined)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.PlotIO as PlotIO
import EFA.Signal.Signal (TC(..), Scalar,toScalar)
import EFA.Signal.Data (Data(..), Nil)
import EFA.Signal.Typ (Typ, F, T, A, Tt)
import EFA.Signal.Sequence (makeSeqFlowTopology)

import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow
import EFA.Graph (lefilter)
import EFA.Graph.Topology (isStructureEdge)

import qualified EFA.Utility as Utility
import EFA.Utility.Async (concurrentlyMany_)

import EFA.IO.PLTImport (modelicaPLTImport)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO
import System.Environment (getEnv)
import System.FilePath ((</>))

import qualified Data.List.HT as ListHT
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Tuple.HT (mapSnd)
import Control.Monad (when)


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

examplePath :: FilePath
examplePath = "examples/energy/localGrid"

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

zeroNoiseTolerance :: Double
zeroNoiseTolerance = 10^^(-2::Int)

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

ignore :: [a] -> [a]
ignore _ = []


main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8

  path <- fmap (</> examplePath) $ getEnv "EFADATA"
  rawSignalsX <- mapM modelicaPLTImport $ map (path </>) fileNamesX :: IO ([Record.SignalRecord [] Double])

  let preProcessedDataX =
        map (Analysis.pre System.topology zeroNoiseTolerance sectionFilterTime sectionFilterEnergy) rawSignalsX

      (_,sequenceFlowsFiltUnmappedX,flowStatesUnmappedX,powerSignalsX,signalsX) =
        L.unzip5 preProcessedDataX

      allSignalsX = zipWith (Record.combinePowerAndSignalWithFunction System.convertPowerId) powerSignalsX signalsX

      sequenceFlowsFiltX = sectionMapping sequenceFlowsFiltUnmappedX

      flowStatesX = sectionMapping flowStatesUnmappedX

      flowToposX = map (Flow.genSequFlowTops System.topology) flowStatesX

      sequenceFlowTopologyX = map makeSeqFlowTopology flowToposX

      sectionToposX =  map (mapSnd (lefilter (isStructureEdge .fst))) sequenceFlowTopologyX

      externalEnvX =
         map (Env.completeFMap
                (checkDetermined "external scalar")
                (checkDetermined "external signal")) $
         zipWith Analysis.external  sequenceFlowTopologyX sequenceFlowsFiltX
      externalSignalEnvX =
         zipWith Analysis.external2 sequenceFlowTopologyX sequenceFlowsFiltX

      externalDeltaEnvX =
        ListHT.mapAdjacent
           (Env.intersectionWith EqRecord.deltaCons EqRecord.deltaCons)
           externalEnvX

---------------------------------------------------------------------------------------
-- * State Analysis
  when False $ do
    let Record.Record _ sigs = head rawSignalsX
    putStrLn $ Utility.myShowList $ Map.keys sigs

  let drawDelta (Record.DeltaName ti) topo env c =
          Draw.xterm $
          Draw.title ti $
          Draw.bgcolour c $
          Draw.sequFlowGraphDeltaWithEnv topo env
      drawAbs (Record.Name ti) topo env c =
          Draw.xterm $
          Draw.title ti $
          Draw.bgcolour c $
          Draw.sequFlowGraphAbsWithEnv topo env

  concurrentlyMany_ $ [
    -- Topologie
    Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNames System.topology
    ]

---------------------------------------------------------------------------------------
-- * Draw flow states
    ++ ignore [putStrLn ("Number of possible flow states: " ++ show (length System.flowStates))]
    ++ ignore [Draw.xterm $ Draw.flowTopologies (take 20 System.flowStates)]

---------------------------------------------------------------------------------------
-- * Plot Time Signals

--    ++ Plots.sigsWithSpeed allSignalsX (head plotList)
    ++ ignore [PlotIO.record "Test" plotTerm show id (head rawSignalsX)]

    ++ ignore
          [mapM_
              (\(x,y) ->
                  PlotIO.recordList_extract ("Signals of Component " ++ x)
                     plotTerm show id (zip datasetsX allSignalsX) y) plotList]

---------------------------------------------------------------------------------------
-- * Plot Efficiency Curves and Distributions

    ++ ignore
       [mapM_ (PlotIO.etaDistr1DimfromRecordList "Average Efficiency Curve -" 10000 5000
               (zip datasetsX (map (Record.diffTime . Record.partIntegrate) powerSignalsX))) etaList]

---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [head $ L.zipWith4 drawAbs
         datasetsX
         sectionToposX
         externalEnvX
         colours]

---------------------------------------------------------------------------------------
-- * Draw Delta Section flows

    ++ ignore
       (L.zipWith4 drawDelta
         deltasetsX
         sectionToposX
         externalDeltaEnvX
         (tail colours))

---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [head $ L.zipWith4 drawAbs
         [Record.Name "Signal"]
         sectionToposX
         (map (Env.completeFMap id (fmap (fmap Arith.integrate))) externalSignalEnvX)
         colours]
