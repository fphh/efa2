{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

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

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.EtaSys as EtaSys
import qualified EFA.Application.Utility as AppUt
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Simulation as AppSim

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Signal.Signal as Sig
-- import EFA.Signal.Data(Data(..),Nil) --,Nil,(:>))

import qualified EFA.Graph.Topology.Index as TIdx

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Graphics.Gnuplot.Terminal.PostScript as PostScript
--import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Sequence as Seq

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.List as List

import Data.Monoid (mempty)
import Data.Tuple (swap)
import Data.Function (on)


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

scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (1, 0.8)) :
  ("gas",         (1, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []

restScale, localScale :: Double
restScale = 1
localScale = 1.0

local, rest, water, gas :: [Double]
--local = [0.3, 0.5 .. 3.3]
--rest =  [0.2, 0.4 .. 2]
--water = [0.3, 0.5 .. 1]
--gas =   [0.2, 0.4 .. 1]

local = [0.5, 0.7]
rest =  [0.2, 0.4]
water = [0.3, 1]
gas =   [0.2, 0.4]


interpolate :: Map [Double] Double -> [[Double]] -> [Double]
interpolate m xs = map (snd . findNearest m') xs'
  where f = sum . map (^2)
        xs' = map f xs
        m' = Map.toList $ Map.mapKeys f m
        findNearest xs x =
          snd $ List.minimumBy (compare `on` fst)
              $ map (\y -> (abs $ x - fst y, y)) xs


main :: IO()
main = do

  -- | Import Maps and power demand profiles

  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt"



  let etaFunctionMap = CT.makeEtaFunctions2D scaleTableEta tabEta

      initEnv = AppOpt.initialEnv System.Water System.stateFlowGraph

      --- ( von hier       -- pro State ein envsSweep erzeugen (chargem, discharge)
      
      solveFunc =
        Optimisation.solve
          System.stateFlowGraph
          initEnv
          state0
          System.etaAssignState
          etaFunctionMap

      envsSweep :: Map [Double] [EnvResult Double]

      envsSweep =
        Sweep.doubleSweep solveFunc ([local, rest], [water, gas])


      force = Optimisation.forcing $ Optimisation.ChargeDrive 0

      maxEtaEnv = Map.map
        (snd . Sweep.optimalSolutionState 
          (const True)
          force
          System.stateFlowGraph)
          envsSweep

      state0 = TIdx.State 0
      restToWaterPower = XIdxState.power state0 Network Water
      localToGasPower = XIdxState.power state0 LocalNetwork Gas

      optWaterPower =
        Map.map (AppUt.lookupDetPowerState restToWaterPower) maxEtaEnv

      optGasPower =
        Map.map (AppUt.lookupDetPowerState localToGasPower) maxEtaEnv
        
      -- ) bis hier kapseln und pro state ausführen  


  let
      (time', [pwind', psolar', phouse', pindustry']) =
        CT.getPowerSignalsWithSameTime tabPower
          ["wind", "solar", "house", "industry"]

      (time, [pwind, psolar, phouse, pindustry]) =
        (Sig.toList time', map Sig.toList [pwind', psolar', phouse', pindustry'])

      prest = map (restScale*) pwind
      plocal = map ((0.5+) . (localScale*)) $
        zipWith3 (\x y z -> x+y+z) psolar phouse pindustry


      pwater = interpolate optWaterPower (zipWith (\x y -> [x, y]) plocal prest)
      pgas = interpolate optGasPower (zipWith (\x y -> [x, y]) plocal prest)

      fromto = (TIdx.PPos .) . TIdx.StructureEdge

      givenSignals :: Record.PowerRecord Node [] Double
      givenSignals = 
        Seq.addZeroCrossings $ Record.Record (Sig.fromList time) $ Map.fromList $
          (fromto Network Water, Sig.fromList pwater) :
          (fromto LocalNetwork Gas, Sig.fromList pgas) :
          (fromto LocalRest LocalNetwork, Sig.fromList prest): 
          (fromto Rest Network, Sig.fromList plocal) : []

      envSim =
        AppSim.solve
          System.topology
          System.etaAssignState
          etaFunctionMap
          givenSignals
          
      -- 1. envSim zu Record machen und dann die ganz normale efa-Analyse laufen lasen -> Sequenzflussgraph
      -- 2. Sequenzflussgraph zu Zustandsflussgraph
      -- 3. Initenv ersetzen => erste Iterationsschleife (brauch ein Akzeptanz-Kriterium für X und eta), 
      -- 3q. Akzeptanzfunktion schreiben mit Euklidscher oder Prozentpunkte abweichung oder Delta
      -- 4. Äußere Schleife -- SOC-Faktor (Balance-Funktion) -- (SOC-ZielSOC)*Force
      -- 4a -- visualisierung der Iteration
      
  print envsSweep
  concurrentlyMany_ [
    --Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNames System.topology,

    -- Draw.xterm $ Draw.flowTopologies System.flowStates,
 
   Draw.xterm $
      Draw.stateFlowGraphWithEnv Draw.optionsDefault System.stateFlowGraph envSim
    ]

