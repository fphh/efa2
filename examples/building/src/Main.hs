module Main where


import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation
--import Modules.Optimisation(Env(..),
                      --      envGetData,
                       --     doubleSweep,
import Modules.Optimisation(EnvResult)


import qualified EFA.Application.Optimisation as AppOpt
--import qualified EFA.Application.Utility as AppUt

import qualified EFA.Graph.Draw as Draw
import qualified EFA.Signal.Signal as Sig

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


-- | Skalierung des Modells
scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (1, 0.8)) :
  ("gas",         (1, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []


-- | varying the network loads
loadHousePower :: Sig.PSignal V.Vector Double
loadHousePower = Sig.fromList $ [0.3, 0.4 .. 3.3]

loadNetPower :: Sig.PSignal V.Vector Double
loadNetPower = Sig.fromList $ [0.2, 0.3 .. 2]

varLoadHouse, varLoadNet :: Sig.PSignal2 V.Vector V.Vector Double
(varLoadHouse, varLoadNet) = Sig.variation2D loadHousePower loadNetPower


-- | varying degrees of freedom

batteryPower :: Sig.PSignal V.Vector Double
batteryPower = Sig.fromList $ [0.3, 0.4 .. 3.3]

storagePower :: Sig.PSignal V.Vector Double
storagePower = Sig.fromList $ [0.2, 0.3 .. 2]

varBattery, varStorage :: Sig.PSignal2 V.Vector V.Vector Double
(varBattery, varStorage) = Sig.variation2D batteryPower storagePower

main :: IO()
main = do

    -- tabEta <- Table.read "../maps/eta.txt"
--    tabPower <- Table.read "../maps/power.txt"
    tabEta <- Table.read "../../energy/localGrid/simulation/maps/eta.txt"

   -- |Import Efficiency Curves
    let etaFunctionMap = CT.makeEtaFunctions2D scaleTableEta tabEta

        section = undefined

        envAverage = undefined

        -- | solve the system for all kombinations for a selected section -- muss auf n sektionen erweitert werden
        envs :: Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector (EnvResult Double))
        envs = AppOpt.doubleSweep (Optimisation.solve System.seqTopology envAverage section etaFunctionMap)
             varBattery varStorage varLoadHouse varLoadNet

    Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNames System.topology

    PlotIO.surfaceWithOpts "Variation" DefaultTerm.cons id frameOpts noLegend varLoadHouse varLoadNet varLoadNet




