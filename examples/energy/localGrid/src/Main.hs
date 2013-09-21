{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Modules.Signals as Signals
import qualified Modules.System as System
import qualified Modules.Analysis as Analysis

import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import EFA.Signal.Signal (TC, Scalar, toScalar)
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow
import EFA.Graph.Topology (isStructureEdge)
import EFA.Graph (lefilter)

import qualified EFA.Utility as Utility
import EFA.Utility.Async (concurrentlyMany_)

import EFA.IO.PLTImport (modelicaPLTImport)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO
import System.Environment (getEnv)
import System.FilePath ((</>))

import qualified Data.Map as Map
import Data.Tuple.HT (mapSnd)
import Control.Monad (when)


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

examplePath :: FilePath
examplePath = "examples/energy/localGrid"

zeroNoiseTolerance :: Double
zeroNoiseTolerance = 10^^(-2::Int)

data
  Dataset =
    Dataset {
      datasetPath  :: FilePath,
      datasetName  :: Record.Name,
      datasetColor :: Colors.X11Color
    }

dataset :: FilePath -> String -> Colors.X11Color -> Dataset
dataset fn name = Dataset fn (Record.Name name)

datasetA, datasetB :: Dataset
datasetA = dataset "powerNetwork_res.plt" "Base" Colors.White
datasetB = dataset "powerNetworkETA_res.plt" "ETA" Colors.Gray90

deltasetName :: Record.DeltaName
deltasetName = Record.deltaName (datasetName datasetA) (datasetName datasetB)

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

sectionMapping :: Sequ.List a -> Sequ.List a
sectionMapping = id -- Sequ.reIndex [8,11,13,14,18,32,37::Int])

ignore :: [a] -> [a]
ignore _ = []


type Env = Env.Complete System.Node

process ::
   Record.SignalRecord [] Double ->
   (Record.SignalRecord [] Double,
    Env Double Double,
    Env (Result (Data Nil Double)) (Result (Data ([] :> Nil) Double)),
    Record.PowerRecord System.Node [] Double,
    Flow.RangeGraph System.Node)

process rawSignals =
  let (_, sequenceFlowsFiltUnmapped, flowStatesUnmapped, powerSignals, signal) =
        Analysis.pre System.topology
          zeroNoiseTolerance sectionFilterTime sectionFilterEnergy
          rawSignals

      allSignals =
        Record.combinePowerAndSignalWithFunction System.convertPowerId
          powerSignals signal

      sequenceFlowsFilt = sectionMapping sequenceFlowsFiltUnmapped

      sequenceFlowTopology =
        Flow.sequenceGraph $
        Flow.genSequFlowTops System.topology $
        sectionMapping flowStatesUnmapped

      sectionTopos =
        mapSnd (lefilter (isStructureEdge .fst)) sequenceFlowTopology

      externalEnv =
        Env.completeFMap
          (checkDetermined "external scalar")
          (checkDetermined "external signal") $
        Analysis.external  sequenceFlowTopology sequenceFlowsFilt
      externalSignalEnv =
        Analysis.external2 sequenceFlowTopology sequenceFlowsFilt

  in  (allSignals, externalEnv, externalSignalEnv,
       powerSignals, sectionTopos)


importDataset :: FilePath -> IO (Record.SignalRecord [] Double)
importDataset fileName = do
  path <- fmap (</> examplePath) $ getEnv "EFADATA"
  modelicaPLTImport $ path </> fileName

main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8
  rawSignalsA <- importDataset $ datasetPath datasetA
  rawSignalsB <- importDataset $ datasetPath datasetB

  let (allSignalsA, externalEnvA, externalSignalEnvA,
       powerSignalsA, sectionToposA) = process rawSignalsA

      (allSignalsB, externalEnvB, _externalSignalEnvB,
       powerSignalsB, _sectionToposB) = process rawSignalsB

      externalDeltaEnv =
        Env.intersectionWith
          EqRecord.deltaCons EqRecord.deltaCons
          externalEnvA externalEnvB


---------------------------------------------------------------------------------------
-- * State Analysis
  when False $ do
    let Record.Record _ sigs = rawSignalsA
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
    ++ ignore [PlotIO.record "Test" plotTerm show id rawSignalsA]

    ++ ignore
          [mapM_
              (\(x,y) ->
                  PlotIO.recordList_extract ("Signals of Component " ++ x)
                     plotTerm show id
                     [(datasetName datasetA, allSignalsA),
                      (datasetName datasetB, allSignalsB)]
                     y)
              plotList]

---------------------------------------------------------------------------------------
-- * Plot Efficiency Curves and Distributions

    ++ ignore
       [mapM_
           (PlotIO.etaDistr1DimfromRecordList
               "Average Efficiency Curve -" 10000 5000
               (let g ds powerRec =
                       (datasetName ds,
                        Record.diffTime . Record.partIntegrate $ powerRec)
                in  [g datasetA powerSignalsA,
                     g datasetB powerSignalsB]))
           etaList]

---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [drawAbs (datasetName datasetA)
          sectionToposA
          externalEnvA
          (datasetColor datasetA)]

---------------------------------------------------------------------------------------
-- * Draw Delta Section flows

    ++ ignore
       [drawDelta deltasetName
          sectionToposA
          externalDeltaEnv
          Colors.Gray90]

---------------------------------------------------------------------------------------
-- * Draw Section flows

    ++ [drawAbs (Record.Name "Signal")
         sectionToposA
         (Env.completeFMap id (fmap (fmap Arith.integrate)) externalSignalEnvA)
         (datasetColor datasetA)]
