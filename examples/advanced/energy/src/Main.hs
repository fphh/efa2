{-# LANGUAGE FlexibleContexts #-}


{-

TODO

* Generisches Forcing für Speicherkanten.

* Maybes sollen mit defaultwert in plot geplottet werden.

* powerrecord aus eingelesenen Signalen bauen.

* interpolation in kombination mit nan prüfen

* addZeroCrossings funktioniert mit Listen

* Funktion um Section zu plotten.

* simulationIO soll generisch für n reqs werden.

* inaktive Kanten schon in extractOptimalPowerMatricesPerState erkennen
  sollen 0 Leistung liefern

* findZeroCrossing verstehen

* fromSequenceFlow Wirkung von allStEdges ???

* optimalSolutionGeneric nicht richtig, Email Henning 11.12.2013 12:03

* wiki-Artikel wg forall verbessern

* to2DMatrix, head entfernen

-}

module Main where

import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Setting as ModSet
-- import qualified Modules.Plot as ModPlot

import qualified Modules.Optimisation.Loop as ModLoop

import EFA.Application.Type (EnvResult)
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.IO.TableParser as Table

--import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Equation.Arithmetic as Arith

--import qualified EFA.Flow.Draw as Draw

-- import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV



import Text.Printf (printf)



initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams Node list sweep vec a->
  EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph


main1 :: IO()
main1 = do

  tabEta <- Table.read "../maps/eta.txt"

  tabPower <- Table.read "../maps/power.txt.bak"

  let etaMap =
         Map.mapKeys One.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys (\(One.Name str) -> str) ModSet.scaleTableEta)
            tabEta

  let

      (time,
       NonEmpty.Cons r
          (NonEmpty.Cons l Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      -- transform = Sig.offset 0.1 . Sig.scale 2.9
      transformRest1 = Sig.offset 2 . Sig.scale 0.9
      transformRest2 = Sig.offset 2.2 . Sig.scale 0.6


      transformLocal1 = Sig.offset 0.2 . Sig.scale 0.7
      transformLocal2 = Sig.offset 0.4 . Sig.scale 0.5

      prest1, prest2, plocal1, plocal2 :: Sig.PSignal Vector Double
      prest1 = Sig.convert $ transformRest1 r
      prest2 = Sig.convert $ transformRest2 r

      plocal1 = Sig.convert $ transformLocal1 l
      plocal2 = Sig.convert $ transformLocal2 l

-- ACHTUNG ACHTUNG hieran muessen wir uns orientieren !!! 26.01.2014

      reqsPos = ReqsAndDofs.unReqs $ ReqsAndDofs.reqsPos ModSet.reqs

      la = case Sig.viewR time of
                Just (_, x) -> x
                _ -> error "Sig.viewR time"

      ctime = Sig.convert time

      t = Sig.map (/2) (ctime Sig..++ Sig.offset (Sig.fromSample la) ctime)

      prest = prest1 Sig..++ prest2
      plocal = plocal1 Sig..++ plocal2

-- ACHTUNG ACHTUNG wir haben local und rest verwechselt !!! 26.01.2014
      reqsRec :: Record.PowerRecord Node Vector Double
      reqsRec =
        Record.Record t (Map.fromList (zip reqsPos [prest, plocal]))

{-
  concurrentlyMany_ [
    Draw.xterm $ Draw.labeledTopology $ System.labeledTopology,
    Draw.xterm $
      Draw.flowTopologies $
      StateAnalysis.advanced System.topology ]

  concurrentlyMany_ [
    ModPlot.record ModPlot.gpXTerm "Requirement Signals" reqsRec,
    ModPlot.requirements ModPlot.gpXTerm prest plocal ]
-}

  let
      optParams :: One.OptimalEnvParams Node [] Sweep UV.Vector Double
      optParams =
        One.OptimalEnvParams
          System.topology
          ModSet.initStorageState
          ModSet.initStorageSeq
          etaMap
          System.etaAssignMap
          ModSet.sweepPts
          ModSet.forcingMap
          (ReqsAndDofs.reqsPos ModSet.reqs)
          (ReqsAndDofs.dofsPos ModSet.dofs)
          ModSet.sweepLength


{-
  void $ forcingSweep optParams reqsRec
       $ AppOpt.storageEdgeXFactors optParams 3 3
       $ AppOpt.initialEnv optParams System.stateFlowGraph
-}



  putStrLn $ printf "%6s%24s%24s%24s%24s" 
                    "Step" "StepSize" "Forcing" "Balance" "Eta"

  let ol = --ModLoop.uniqueInnerLoopX
           ModLoop.outerLoop
             (ModLoop.iterateBalance optParams reqsRec)
             ( AppOpt.storageEdgeXFactors optParams 3 3
               $ AppOpt.initialEnv optParams System.stateFlowGraph )

  let numberOfLoops = 5

  mapM_ putStrLn (ModLoop.showOuterLoop numberOfLoops ol)

  -- sequence_ (ModLoop.printOuterLoop optParams numberOfLoops ol)

  return ()

main :: IO ()
main = main1

