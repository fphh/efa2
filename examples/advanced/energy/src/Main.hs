{-# LANGUAGE FlexibleContexts #-}


module Main where

import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Setting as ModSet
import qualified Modules.Plot as ModPlot
import qualified Modules.Utility as ModUt
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.Loop as ModLoop

import EFA.Application.Type (EnvResult)
--import qualified EFA.Application.Type as Type
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt

--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.State.Quantity as StateQty

--import qualified EFA.Graph.Topology as Topology
--import qualified EFA.Graph as Graph

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.IO.TableParser as Table

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Flow.Draw as Draw

import EFA.Utility.Async (concurrentlyMany_)
--import EFA.Utility.List (vhead)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
--import qualified Data.List as List
--import Control.Monad(void)

--import qualified EFA.Signal.Plot as Plot

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV

import qualified EFA.Flow.Topology.Index as TopoIdx

--import Text.Printf (printf)

import qualified System.Random as Random 


initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimisationParams Node list sweep vec a->
  EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph


main1 :: IO()
main1 = do

  tabEta <- Table.read "../maps/eta.txt"

  tabPower <- Table.read "../maps/power.txt.bak"
  
  rndGen <- Random.getStdGen

  let etaMap =
         Map.mapKeys One.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys (\(One.Name str) -> str) ModSet.scaleTableEta)
            tabEta

  let

      (time,
       NonEmpty.Cons local
          (NonEmpty.Cons rest Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      reqsPos = ReqsAndDofs.unReqs $ ReqsAndDofs.reqsPos ModSet.reqs

      ctime = Sig.convert time

      pLocal = Sig.offset 1.5 . Sig.scale 1 $ Sig.convert $ local
      pRest = Sig.offset 0.3 . Sig.scale 0.9 $ Sig.convert $  rest

      reqsRec :: Record.PowerRecord Node UV.Vector Double
--      reqsRec = Record.scatterRnd rndGen 10 0.3 $ Record.Record ctime (Map.fromList (zip reqsPos [pLocal,pRest]))
      reqsRec = Record.Record ctime (Map.fromList (zip reqsPos [pLocal,pRest]))
      
      reqsRecStep :: Record.PowerRecord Node UV.Vector Double
      reqsRecStep = Record.makeStepped reqsRec



{-  concurrentlyMany_ [
      Draw.xterm $ Draw.labeledTopology $ System.labeledTopology,
      Draw.xterm $
      Draw.flowTopologies $
      StateAnalysis.advanced System.topology ]-}





  let
      ienv = AppOpt.storageEdgeXFactors optParams 3 3
               $ AppOpt.initialEnv optParams System.stateFlowGraph

      sysParams = One.SystemParams {
         One.systemTopology = System.topology,
         One.etaAssignMap = System.etaAssignMap,
         One.etaMap = etaMap,
         One.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
         One.initStorageState = ModSet.initStorageState,
         One.initStorageSeq = ModSet.initStorageSeq}

      optParams :: One.OptimisationParams Node [] Sweep UV.Vector Double
      optParams = One.OptimisationParams {
          One.stateFlowGraphOpt = ienv,
          One.reqsPos = (ReqsAndDofs.reqsPos ModSet.reqs),
          One.dofsPos = (ReqsAndDofs.dofsPos ModSet.dofs),
          One.points = ModSet.sweepPts,
          One.sweepLength = ModSet.sweepLength,
          One.etaToOptimise = Nothing,
          One.maxEtaIterations = One.MaxEtaIterations 2,
          One.maxInnerLoopIterations = One.MaxInnerLoopIterations 3,
          One.maxBalanceIterations = One.MaxBalanceIterations 20,
          One.maxStateIterations = One.MaxStateIterations 100,
          One.initialBattForcing = Map.fromList [(System.Water, One.DischargeDrive 0)],
          One.initialBattForceStep = Map.fromList [(System.Water, One.ChargeDrive 0.1)],
          One.etaThreshold = One.EtaThreshold 0.2,
          One.balanceThreshold = One.BalanceThreshold 0.2,
          One.stateTimeUpperThreshold = One.StateTimeThreshold 3,
          One.stateTimeLowerThreshold = One.StateTimeThreshold 0,
          One.stateForcingSeed = One.StateForcingStep 0.05,
          One.balanceForcingSeed = One.ChargeDrive 0.01}

      simParams :: One.SimulationParams Node [] Double
      simParams = One.SimulationParams {
          One.varReqRoomPower1D = Sig.convert $ ModSet.varRestPower1D,
          One.varReqRoomPower2D = Sig.convert $ ModSet.varLocalPower ,
          One.reqsRec = Base.convertRecord reqsRecStep, 
          One.sequFilterTime=0.01,
          One.sequFilterEnergy=0 }

  print (map (ModUt.absoluteStateIndex $ One.systemTopology sysParams) System.flowStates)


{-
  void $ forcingSweep optParams reqsRec
       $ AppOpt.storageEdgeXFactors optParams 3 3
       $ AppOpt.initialEnv optParams System.stateFlowGraph
-}



  let
      ol = --ModLoop.uniqueInnerLoopX
           ModLoop.iterateEtaWhile sysParams optParams simParams



{-      opt = ModLoop.withChargeDrive optParams reqsRec ienv (-5.2837473962302475e-3)
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



--  mapM_ putStrLn (ModLoop.showEtaLoop optParams ol)
  concurrentlyMany_ [
--    ModPlot.record ModPlot.gpXTerm "Requirement Signals" reqsRec, 
--    ModPlot.record ModPlot.gpXTerm "Requirement Signals Stepped" reqsRecStep,
--    ModPlot.reqsRec ModPlot.gpXTerm reqsRec, 
    mapM_ putStrLn (ModLoop.showEtaLoop optParams ol)] 
--    sequence_ (ModLoop.printEtaLoop optParams ol)]

  

{-
  concurrentlyMany_ [
    ModPlot.maxEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaDifferencePerState ModPlot.gpXTerm opt2 ]

-}

  return ()



main :: IO ()
main = main1

