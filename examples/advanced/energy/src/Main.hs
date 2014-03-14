{-# LANGUAGE FlexibleContexts #-}


module Main where

{-

* Alle Map.toList ersetzen durch Map.toAscList oder Map.findMin ?

-}

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

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vhead)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
--import qualified Data.List as List
--import Control.Monad(void)

--import qualified EFA.Signal.Plot as Plot

--import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV

import qualified EFA.Flow.Topology.Index as TopoIdx

--import Text.Printf (printf)

--import qualified System.Random as Random


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

  --rndGen <- Random.getStdGen

  let etaMap =
         Map.mapKeys One.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys One.unName ModSet.scaleTableEta)
            tabEta

  let

      (time,
       NonEmpty.Cons local
          (NonEmpty.Cons rest Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      reqsPos = ReqsAndDofs.unReqs $ ReqsAndDofs.reqsPos ModSet.reqs

      ctime = Sig.convert time

      pLocal = Sig.offset 0.1 . Sig.scale 3 $ Sig.convert $ local
--      pLocal = Sig.offset 0.2 . Sig.scale 1.3 $ Sig.convert $  local
      pRest = Sig.offset 0.2 . Sig.scale 1.3 $ Sig.convert $  rest

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
               
      supportPoints =
        Base.supportPoints
          [ TopoIdx.ppos System.LocalRest System.LocalNetwork,
            TopoIdx.ppos System.Rest System.Network ]
          (Base.convertRecord reqsRecStep)
          (map (Sig.findSupportPoints. Sig.untype) 
               [ ModSet.varLocalPower1D, ModSet.varRestPower1D])

      sysParams = One.SystemParams {
         One.systemTopology = System.topology,
         One.etaAssignMap = System.etaAssignMap,
         One.etaMap = etaMap,
         One.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
         One.initStorageState = ModSet.initStorageState,
         One.initStorageSeq = ModSet.initStorageSeq }

      optParams :: One.OptimisationParams Node [] Sweep UV.Vector Double
      optParams = One.OptimisationParams {
          One.stateFlowGraphOpt = ienv,
          One.reqsPos = (ReqsAndDofs.reqsPos ModSet.reqs),
          One.dofsPos = (ReqsAndDofs.dofsPos ModSet.dofs),
          One.points = ModSet.sweepPts,
          One.sweepLength = ModSet.sweepLength,
          One.etaToOptimise = Nothing,
          One.maxEtaIterations = One.MaxEtaIterations 1,
          One.maxInnerLoopIterations = One.MaxInnerLoopIterations 3,
          One.maxBalanceIterations = One.MaxBalanceIterations 100,
          One.maxStateIterations = One.MaxStateIterations 1,

          One.initialBattForcing =
            One.BalanceForcingMap
            $ Map.fromList [(System.Water, One.DischargeDrive 1)],

          One.initialBattForceStep =
            One.BalanceForcingMap
            $ Map.fromList [(System.Water, One.ChargeDrive 0.1)],

          One.etaThreshold = One.EtaThreshold 0.2,
          One.balanceThreshold = One.BalanceThreshold 0.5,
          One.stateTimeUpperThreshold = One.StateTimeThreshold 3,
          One.stateTimeLowerThreshold = One.StateTimeThreshold 0,
          One.stateForcingSeed = One.StateForcingStep 0.05,
          One.balanceForcingSeed = One.ChargeDrive 0.01 }

      simParams :: One.SimulationParams Node [] Double
      simParams = One.SimulationParams {
          One.varReqRoomPower1D = Sig.convert $ ModSet.varLocalPower1D,
          One.varReqRoomPower2D = Sig.convert $ ModSet.varRestPower ,
          One.reqsRec = Base.convertRecord reqsRecStep,
          One.requirementGrid = [ Sig.convert $ ModSet.varLocalPower1D,
                                  Sig.convert $ ModSet.varRestPower1D ],
          One.activeSupportPoints =  supportPoints, 
          One.sequFilterTime=0.01,
          One.sequFilterEnergy=0 }

  print $ map (ModUt.absoluteStateIndex (One.systemTopology sysParams)) System.flowStates


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


  -- let g = fmap (vhead "simulationGraphs" . Sweep.toList)

{-
  Draw.xterm
    $ Draw.title "State Flow Graph from Simulation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g ienv
-}

--  print reqsRecStep 
--  print supportPoints


--  mapM_ putStrLn (ModLoop.showEtaLoop optParams ol)
  concurrentlyMany_ [
    --ModPlot.record ModPlot.gpXTerm "Requirement Signals" reqsRec,
    --ModPlot.record ModPlot.gpXTerm "Requirement Signals Stepped" reqsRecStep,
    --ModPlot.reqsRec ModPlot.gpXTerm reqsRec,
--    ModLoop.checkRangeIO sysParams optParams simParams]
--    mapM_ putStrLn (ModLoop.showEtaLoop optParams ol)]
    sequence_ (ModLoop.printEtaLoop optParams ol)]


{-
  concurrentlyMany_ [
    ModPlot.maxEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaDifferencePerState ModPlot.gpXTerm opt2 ]

-}

  return ()



main :: IO ()
main = main1

{-
oldRecord: Record {recordTime = TC (Data [0.0,1.0,1.0,2.0,2.0,3.0,3.0,4.0,4.0,5.0,5.0,6.0,6.0,7.0,7.0,8.0,8.0,9.0,9.0,10.0,10.0,11.0,11.0,12.0,12.0,13.0,13.0,14.0,14.0,15.0,15.0,16.0,16.0,17.0,17.0,18.0,18.0,19.0,19.0,20.0,20.0,21.0,21.0,22.0,22.0,23.0]), 

recordSignalMap = fromList [
(Position Network Water,TC (Data [0.19999999999999998,0.19999999999999998,NaN,NaN,0.2,0.2,0.2,0.2,0.19999999999999998,0.19999999999999998,0.2,0.2,0.19999999999999998,0.19999999999999998,0.2,0.2,0.19999999999999998,0.19999999999999998,0.2,0.2,0.2,0.2,0.19999999999999998,0.19999999999999998,0.20000000000000004,0.20000000000000004,0.19999999999999998,0.19999999999999998,0.2,0.2,0.2,0.2,0.19999999999999998,0.19999999999999998,0.2,0.2,0.19999999999999998,0.19999999999999998,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2])),

(Position LocalNetwork Gas,TC (Data [0.4630000000000001,0.4630000000000001,NaN,NaN,0.5000000000000001,0.5000000000000001,0.5480000000000005,0.5480000000000005,0.16000000000000006,0.16000000000000006,0.3400000000000006,0.3400000000000006,0.2700000000000002,0.2700000000000002,0.30800000000000044,0.30800000000000044,0.20500000000000013,0.20500000000000013,0.8,0.8,0.8,0.8,0.27499999999999986,0.27499999999999986,0.5000000000000007,0.5000000000000007,0.20500000000000013,0.20500000000000013,0.37999999999999967,0.37999999999999967,0.1600000000000001,0.1600000000000001,0.5600000000000005,0.5600000000000005,0.8,0.8,0.27499999999999986,0.27499999999999986,0.7999999999999995,0.7999999999999995,0.6830000000000003,0.6830000000000003,0.30800000000000044,0.30800000000000044,0.4560000000000001,0.4560000000000001])),

(Position Rest Network,TC (Data [0.33,0.33,0.5900000000000001,0.5900000000000001,0.8500000000000001,0.8500000000000001,0.46,0.46,0.33,0.33,0.72,0.72,1.24,1.24,0.98,0.98,1.1099999999999999,1.1099999999999999,0.8500000000000001,0.8500000000000001,0.5900000000000001,0.5900000000000001,0.8500000000000001,0.8500000000000001,1.5,1.5,1.1099999999999999,1.1099999999999999,0.8500000000000001,0.8500000000000001,0.46,0.46,0.33,0.33,0.98,0.98,0.8500000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,1.1099999999999999,1.1099999999999999,0.98,0.98,0.5900000000000001,0.5900000000000001])),

(Position LocalRest LocalNetwork,TC (Data [0.7000000000000001,0.7000000000000001,0.4,0.4,0.9999999999999999,0.9999999999999999,1.3000000000000003,1.3000000000000003,2.1999999999999997,2.1999999999999997,2.5000000000000004,2.5000000000000004,0.7000000000000001,0.7000000000000001,2.5000000000000004,2.5000000000000004,0.7000000000000001,0.7000000000000001,3.1,3.1,3.1,3.1,0.7000000000000001,0.7000000000000001,2.8000000000000003,2.8000000000000003,0.7000000000000001,0.7000000000000001,1.9,1.9,2.1999999999999997,2.1999999999999997,2.8000000000000003,2.8000000000000003,3.1,3.1,0.7000000000000001,0.7000000000000001,1.6,1.6,1.3000000000000003,1.3000000000000003,2.5000000000000004,2.5000000000000004,0.9999999999999999,0.9999999999999999]))]}
-}


{-
newRecord: Record {recordTime = TC (Data [0.0,1.0,1.0,2.0,2.0,3.0,3.0,4.0,4.0,5.0,5.0,6.0,6.0,7.0,7.0,8.0,8.0,9.0,9.0,10.0,10.0,11.0,11.0,11.5,12.0,12.0,12.0,13.0,13.0,14.0,14.0,15.0,15.0,16.0,16.0,17.0,17.0,18.0,18.0,18.5,19.0,19.0,19.0,20.0,20.0,21.0,21.0,22.0,22.0,23.0]), 

recordSignalMap = fromList [

(Position Network Water,TC (Data [-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.8,-0.8,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,0.2,0.2,0.2,0.2,-0.6000000000000001,0.19999999999999998,-0.6000000000000001,0.19999999999999998,0.20000000000000004,0.20000000000000004,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,0.19999999999999998,0.19999999999999998,0.2,0.2,-0.6000000000000001,0.19999999999999998,-0.6000000000000001,0.19999999999999998,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001,-0.6000000000000001])),

(Position LocalNetwork Gas,TC (Data [0.0,0.0,0.0,0.522,0.522,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.8,0.8,0.8,0.8,0.0,0.0,0.0,0.0,0.5000000000000007,0.5000000000000007,0.0,0.0,0.0,0.0,0.0,0.0,0.5600000000000005,0.5600000000000005,0.8,0.8,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0])),(Position Rest Network,TC (Data [0.33,0.33,0.33,0.5900000000000001,0.5900000000000001,0.8500000000000001,0.8500000000000001,0.46,0.46,0.33,0.33,0.72,0.72,1.24,1.24,0.98,0.98,1.1099999999999999,1.1099999999999999,0.8500000000000001,0.8500000000000001,0.5900000000000001,0.5900000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,1.5,1.5,1.1099999999999999,1.1099999999999999,0.8500000000000001,0.8500000000000001,0.46,0.46,0.33,0.33,0.98,0.98,0.8500000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,0.8500000000000001,1.1099999999999999,1.1099999999999999,0.98,0.98,0.5900000000000001,0.5900000000000001])),

(Position LocalRest LocalNetwork,TC (Data [0.7000000000000001,0.7000000000000001,0.7000000000000001,0.4,0.4,0.9999999999999999,0.9999999999999999,1.3000000000000003,1.3000000000000003,2.1999999999999997,2.1999999999999997,2.5000000000000004,2.5000000000000004,0.7000000000000001,0.7000000000000001,2.5000000000000004,2.5000000000000004,0.7000000000000001,0.7000000000000001,3.1,3.1,3.1,3.1,0.7000000000000001,0.7000000000000001,0.7000000000000001,0.7000000000000001,2.8000000000000003,2.8000000000000003,0.7000000000000001,0.7000000000000001,1.9,1.9,2.1999999999999997,2.1999999999999997,2.8000000000000003,2.8000000000000003,3.1,3.1,0.7000000000000001,0.7000000000000001,0.7000000000000001,0.7000000000000001,1.6,1.6,1.3000000000000003,1.3000000000000003,2.5000000000000004,2.5000000000000004,0.9999999999999999,0.9999999999999999]))]}


 BL:  0 |    | Sto:   Water   | F: -1.000000000000000   | B: -4.590400000000003


Part I:
t1 0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 7.0, 8.0, 8.0, 9.0, 9.0,10.0,10.0,11.0,11.0,
t2 0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 7.0, 8.0, 8.0, 9.0, 9.0,10.0,10.0,11.0,11.0,
 
x1 0.7, 0.7, 0.4, 0.4, 0.9, 0.9, 1.3, 1.3, 2.19, 2.19, 2.5, 2.5, 0.7, 0.7, 2.5, 2.5, 0.7, 0.7, 3.1, 3.1, 3.1, 3.1, 0.7, 
x2 0.7, 0.7, 0.7, 0.4, 0.4, 0.9, 0.9, 1.3, 1.3, 2.19, 2.19, 2.5, 2.5, 0.7, 0.7, 2.5, 2.5, 0.7, 0.7, 3.1, 3.1, 3.1, 3.1, 


Part II:
t1 12.0,12.0,13.0,13.0,14.0,14.0,15.0,15.0,16.0,16.0,17.0,17.0,18.0,18.0,19.0,19.0,20.0,20.0,21.0,21.0,22.0,22.0,23.0
t2 11.5,12.0,12.0,12.0,13.0,13.0,14.0,14.0,15.0,15.0,16.0,16.0,17.0,17.0,18.0,18.0,18.5,19.0,19.0,19.0,20.0,20.0,21.0,21.0,22.0,22.0,23.0

x1 0.7, 2.8, 2.8, 0.7, 0.7, 1.9, 1.9, 2.19, 2.19, 2.8, 2.8, 3.1, 3.1, 0.7, 0.7, 1.6, 1.6, 1.3, 1.3, 2.5, 2.5, 0.9, 0.9
x2 0.7, 0.7, 0.7, 0.7, 2.8, 2.8, 0.7, 0.7, 1.9, 1.9, 2.1, 2.19, 2.8, 2.8, 3.1, 3.1, 0.7, 0.7, 0.7, 0.7, 1.6, 1.6, 1.3, 1.3, 2.5, 2.5, 0.9, 0.9





-}