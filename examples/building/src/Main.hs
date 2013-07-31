{-# LANGUAGE TypeOperators #-}

module Main where


import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation
--import Modules.Optimisation(Env(..),
                      --      envGetData,
                       --     doubleSweep,
import Modules.Optimisation(EnvResult)
import Modules.System(Node(..))

import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Application.IndexState as XIdxState

import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Simulation as AppSim
import qualified EFA.Application.Utility as AppUt
import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Signal.Signal as Sig
-- import EFA.Signal.Data(Data(..),Nil) --,Nil,(:>))

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Sequence as Seq
import qualified EFA.Graph.Topology.Index as TIdx

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Graphics.Gnuplot.Terminal.PostScript as PostScript
--import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Plot as Plot

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as UV

-- | Graphik Optionen
plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

frameOpts ::
  Opts.T (Graph3D.T Double Double Double) ->
  Opts.T (Graph3D.T Double Double Double)
frameOpts =
--  Plot.heatmap .
  Plot.xyzrange3d (0.2, 2) (0.3, 3.3) (0, 1) .
  -- Plot.cbrange (0.2, 1) .
  Plot.xyzlabel "Load I Power [W]" "Load II Power [W]" "" .
  Plot.paletteGH

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

{-
-- | Skalierung des Modells
scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("water",     (1, 0.8)) :
  ("battery",         (1, 0.4)) :
  ("trafo", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("constOne",       (1, 1)) :
  ("solar",       (1, 1)) :
  []
-}

scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (1, 0.8)) :
  ("gas",         (1, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []



restPowerScale :: Double
restPowerScale = 1

localPowerScale :: Double
localPowerScale = 1.2

-- | varying the network loads
varLocal1D :: Sig.PSignal V.Vector Double
varLocal1D = Sig.fromList [0.3, 0.4 .. 3.3]

varRest1D :: Sig.PSignal V.Vector Double
varRest1D = Sig.fromList [0.2, 0.3 .. 2]

varLocal, varRest :: Sig.PSignal2 V.Vector V.Vector Double
(varLocal, varRest) = Sig.variation2D varLocal1D varRest1D

-- | varying degrees of freedom
waterPower :: Sig.PSignal V.Vector Double
waterPower = Sig.fromList [0.3, 0.4 .. 3.3]

gasPower :: Sig.PSignal V.Vector Double
gasPower = Sig.fromList [0.2, 0.3 .. 2]

varWater, varGas :: Sig.PSignal2 V.Vector V.Vector Double
(varWater, varGas) = Sig.variation2D waterPower gasPower

main :: IO()
main = do

    -- | Import Maps and power demand profiles

    tabEta <- Table.read "../maps/eta.txt"
    tabPower <- Table.read "../maps/power.txt"

    let etaFunctionMap = CT.makeEtaFunctions2D scaleTableEta tabEta

  {-      -- @HT - warum war das nochmal gefährlich ? Wie gehts besser ?
        (time, [powerSignalHouse, powerSignalNetLoad, powerSignalSun])
           = CT.getPowerSignalsWithSameTime tabPower ["house", "netload", "sun"] -}

         -- | Import Power Curves
        (time, [powerSignalWind,
               powerSignalSolar,
               powerSignalHouse,
               powerSignalIndustry]) = CT.getPowerSignalsWithSameTime tabPower
                                       ["wind", "solar", "house", "industry"]


        powerSignalRest = Sig.scale powerSignalWind restPowerScale
        powerSignalLocal = Sig.offset
                          (Sig.scale  (powerSignalSolar Sig..+
                                      Sig.makeDelta (powerSignalHouse Sig..+
                                                     (Sig.makeDelta powerSignalIndustry)))
                          localPowerScale) 0.5

    -- | Optimisation of Operation Points

    let

        state = TIdx.State 0

        envAverage = AppOpt.initialEnv System.Water System.stateFlowGraph

        testEnv :: EnvResult (Double)
        testEnv = --Optimisation.envGetData $
          Optimisation.solve System.stateFlowGraph
            envAverage state System.etaAssignState etaFunctionMap
                                       (1::Double) (1) (1) (1)

        -- solve the system for all combinations for a selected section
        -- TODO sun power needs to become a parameter or optimal setup needs to be changed
        envsSweep :: Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector (EnvResult Double))
        envsSweep = Sweep.doubleSweep (Optimisation.solve System.stateFlowGraph
                                       envAverage state System.etaAssignState etaFunctionMap)
             varWater varGas varLocal varRest

        force = Optimisation.forcing $ Optimisation.ChargeDrive 0

        ensMaybeOpt = Sig.map (Sweep.optimalSolution2DState (\x -> True)
                               force System.stateFlowGraph) envsSweep

        optFunct :: Sig.NSignal2 V.Vector V.Vector Double
        envsOpt :: Sig.UTSignal2 V.Vector V.Vector (EnvResult Double)
        (optFunct,envsOpt) = (Sig.setType $ Sig.map (fst .f) ensMaybeOpt,Sig.map (snd .f) ensMaybeOpt)
          where f =  maybe (error "Optimal Solution in at least one load condition not available") id

        optWaterPowerMap :: Sig.PSignal2 V.Vector V.Vector Double
        optWaterPowerMap = Sig.setType $ Sig.map (AppUt.lookupDetPowerState $
                                                    XIdxState.power state Network Water) envsOpt

        optGasPowerMap :: Sig.PSignal2 V.Vector V.Vector Double
        optGasPowerMap = Sig.setType $ Sig.map (AppUt.lookupDetPowerState $
                                                    XIdxState.power state LocalNetwork Gas) envsOpt

    -- Simulate optimal Solution
    let

       powerSignalWaterOpt = Sig.interp2WingProfileWithSignal "Main/powerSignalWaterOpt"
           varLocal1D varRest optWaterPowerMap
           powerSignalLocal powerSignalRest

       powerSignalBatteryOpt = Sig.interp2WingProfileWithSignal "Main/powerSignalBatteryOpt"
           varLocal1D varRest optGasPowerMap
           powerSignalLocal powerSignalRest


       givenSignals :: Record.PowerRecord Node [] Double
       givenSignals = Seq.addZeroCrossings $
           Record.Record time $
           Map.fromList [(TIdx.PPos (TIdx.StructureEdge Network Water), powerSignalWaterOpt),
                       (TIdx.PPos (TIdx.StructureEdge LocalNetwork Gas), powerSignalBatteryOpt),
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalRest),
                       (TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalLocal)
                      ]

       -- | Build Sequenceflow graph for simulation
       seqTopoSim = Flow.sequenceGraph (AppUt.select System.flowStates [0])

       envSim = AppSim.solve System.topology System.etaAssignState etaFunctionMap givenSignals



    concurrentlyMany_ [
      --Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNames System.topology,

      Draw.xterm $ Draw.flowTopologies System.flowStates,
      Draw.xterm $ Draw.stateFlowGraphWithEnv Draw.optionsDefault System.stateFlowGraph testEnv ]

{-
    PlotIO.surfaceWithOpts "Variation" DefaultTerm.cons id frameOpts noLegend
      varLoadHouse varLoadNet varLoadNet

    PlotIO.surfaceWithOpts "Variation" DefaultTerm.cons id frameOpts noLegend
      varLoadHouse varLoadNet optFunct
-}



