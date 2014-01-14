{-# LANGUAGE FlexibleContexts #-}


{-

TODO

* Generisches Forcing für Speicherkanten.

* Maybes sollen mit defaultwert in plot geplottet werden.

* powerrecord aus eingelesenen Signalen bauen.

* interpolation in kombination mit nan prüfen

* addZeroCrossings funktioniert mit Listen

* Funktion um Section zu plotten.

* inaktive Kanten schon in extractOptimalPowerMatricesPerState erkennen
  sollen 0 Leistung liefern

* findZeroCrossing verstehen

* fromSequenceFlow Wirkung von allStEdges ???

* wiki-Artikel wg forall verbessern

* to2DMatrix, head entfernen

-}

module Main where

import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO
import qualified Modules.Plot as ModPlot
import qualified Modules.Types as Types

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.DoubleSweep as DoubleSweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt
import EFA.Application.OneStorage (Name(Name))

import qualified EFA.Flow.Draw as Draw

import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Part.Index as Idx


import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.IO.TableParser as Table

import qualified EFA.Equation.Arithmetic as Arith

import EFA.Utility.Async (concurrentlyMany)


import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV


import Control.Monad (void)



iterateBalanceIO ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Types.EnvResult Node (Sweep UV.Vector Double) ->
  IO (Types.EnvResult Node (Sweep UV.Vector Double))
iterateBalanceIO params reqsRec stateFlowGraphOpt = do

  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt


      opt = NonIO.optimiseAndSimulate params reqsRec perStateSweep




  let
      powerPos = StateIdx.x (Idx.State 3) System.Network System.Water
      len = One.sweepLength params

  void $ concurrentlyMany [
    ModPlot.plotMaxEta opt,
    ModPlot.plotMaxObj opt,
    ModPlot.plotMaxState opt ]

  ModPlot.plotMaxEta opt

  ModPlot.plotMaxStateContour opt

  void $ concurrentlyMany [
    ModPlot.plotMaxEtaPerState opt,
    ModPlot.plotMaxPosPerState powerPos opt,
    ModPlot.plotMaxObjPerState opt ]


  ModPlot.plotPerStateSweep len "Sweep per State" opt

  ModPlot.plotSimulationSignalsPerEdge params opt
  ModPlot.plotSimulationSignals params opt

  ModPlot.plotOptimalObjectivePerState opt
  ModPlot.plotSimulationGraphs (const Draw.xterm) opt


  --ModPlot.plotSimulationGraphs (Draw.pdf . ("pdf/"++) . (++ (timeStr ++ ".pdf"))) opt


  return $ Types.stateFlowGraph $ Types.simulation opt

initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams Node list sweep vec a->
  Types.EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph





main :: IO()
main = do

  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt.bak"

  let etaMap =
         Map.mapKeys Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys (\(Name str) -> str) ModSet.scaleTableEta)
            tabEta

      (time,
       NonEmpty.Cons r
          (NonEmpty.Cons l Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      transform = Sig.offset 0.1 . Sig.scale 2.9
      -- transform = id

      prest, plocal :: Sig.PSignal Vector Double
      prest = Sig.convert $ transform r
      plocal = Sig.convert $ transform l

      reqsRec :: Record.PowerRecord Node Vector Double
      reqsRec =
        Record.Record (Sig.convert time)
                      (Map.fromList (zip ModSet.reqs [prest, plocal]))

      pts = DoubleSweep.mkPts2 ModSet.sweepPts

      optParams :: One.OptimalEnvParams Node [] Sweep UV.Vector Double
      optParams =
        One.OptimalEnvParams
          System.topology
          ModSet.initStorageState
          ModSet.initStorageSeq
          etaMap
          System.etaAssignMap
          pts
          ModSet.forcingMap
          ModSet.dofs
          ModSet.reqs
          ModSet.sweepLength

  putStrLn $ "Steps\tBalance\t\t\tEta\t"

  void $
     ModUt.nestM 5
        (iterateBalanceIO optParams reqsRec)
        ( AppOpt.storageEdgeXFactors optParams 3 3
          $ AppOpt.initialEnv optParams System.stateFlowGraph)
