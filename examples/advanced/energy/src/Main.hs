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
--import qualified Modules.Plot as ModPlot

import qualified Modules.Optimisation.Loop as ModLoop

import EFA.Application.Type (EnvResult)
import qualified EFA.Application.Type as Type
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt

--import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.State.Quantity as StateQty

--import qualified EFA.Graph.Topology as Topology
--import qualified EFA.Graph as Graph

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.IO.TableParser as Table

--import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Flow.Draw as Draw

--import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vhead)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
--import qualified Data.List as List

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV

import qualified EFA.Flow.Topology.Index as TopoIdx

import Text.Printf (printf)


initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams Node list sweep vec vec2 a->
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

      prest = Sig.convert $ prest1 Sig..++ prest2
      plocal = Sig.convert $ plocal1 Sig..++ plocal2

-- ACHTUNG ACHTUNG wir haben local und rest verwechselt !!! 26.01.2014
      reqsRec :: Record.PowerRecord Node UV.Vector Double
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
      ienv = AppOpt.storageEdgeXFactors optParams 4 4
               $ AppOpt.initialEnv optParams System.stateFlowGraph

      optParams :: One.OptimalEnvParams Node [] Sweep UV.Vector UV.Vector Double
      optParams =
        One.OptimalEnvParams
          System.topology
          ienv
          reqsRec
          ModSet.initStorageState
          ModSet.initStorageSeq
          etaMap
          System.etaAssignMap
          ModSet.sweepPts
          (ReqsAndDofs.reqsPos ModSet.reqs)
          (ReqsAndDofs.dofsPos ModSet.dofs)
          Nothing
          ModSet.sweepLength
          (One.MaxInnerLoopIterations 10)
          (Map.fromList [(System.Water, One.DischargeDrive 1)])
          (Map.fromList [(System.Water, One.ChargeDrive 0.1)])
          ([TopoIdx.ppos System.Water System.Network])
          (One.MaxEtaIterations 3)
          (One.MaxBalanceIterations 100)
          (One.MaxStateIterations 100)
          (One.BalanceThreshold 0.1)
          (One.StateTimeThreshold 0.1)
          (One.EtaThreshold 0.1)
          (One.StateForcing 0.01)
          (One.ChargeDrive 0.01)
          ModSet.varRestPower1D
          ModSet.varLocalPower

  --print (map (Topology.flowNumber $ One.systemTopology optParams) System.flowStates)


{-
  void $ forcingSweep optParams reqsRec
       $ AppOpt.storageEdgeXFactors optParams 3 3
       $ AppOpt.initialEnv optParams System.stateFlowGraph
-}



  let ienv = AppOpt.storageEdgeXFactors optParams 4 4
               $ AppOpt.initialEnv optParams System.stateFlowGraph

      ol = --ModLoop.uniqueInnerLoopX
           ModLoop.iterateEtaWhile optParams
             (ModLoop.iterateInnerLoopWhile optParams)
             (Type.stateFlowGraphSweep . Type.simulation . ModLoop.sResult . vhead "Main" . ModLoop.stateLoop )

--      g =  Type.stateFlowGraphSweep . Type.simulation . vhead "Main" . ModLoop.stateLoop


{-
      opt = ModLoop.withChargeDrive optParams reqsRec ienv (-5.2837473962302475e-3)
      opt2 = ModLoop.withChargeDrive optParams reqsRec (Type.stateFlowGraph $ Type.simulation opt) 0
      opt3 = ModLoop.withChargeDrive optParams reqsRec (Type.stateFlowGraph $ Type.simulation opt2) 0
-}

{-
  let g = fmap (vhead "simulationGraphs" . Sweep.toList)


  Draw.xterm
    $ Draw.title "State Flow Graph from Simulation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g ienv
-}

  putStrLn $ printf "%8s%8s%24s%24s%24s%24s" 
                    "States" "Step" "Forcing" "Balance" "Eta" "StepSize"
{-
  mapM_ putStrLn (ModLoop.showEtaLoop (One.maxEtaIterations optParams) ol)

  sequence_ (ModLoop.printEtaLoop optParams ol)
-}
{-
  concurrentlyMany_ [
    ModPlot.maxEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaDifferencePerState ModPlot.gpXTerm opt2 ]

-}

  return ()



main :: IO ()
main = main1

