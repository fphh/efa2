{-# LANGUAGE FlexibleContexts #-}


module Main where

import qualified Modules.Input.System as System;
import Modules.Input.System (Node)
import qualified Modules.Input.Setting as ModSet
--import qualified Modules.Output.Plot as ModPlot
import qualified EFA.Application.Utility as AppUt
import qualified EFA.Application.Optimisation.Base as Base
import qualified EFA.Application.Optimisation.Loop as Loop

import EFA.Application.Type (EnvResult)
import qualified EFA.Application.Type as Type
import qualified EFA.Application.Optimisation.Balance as Balance
import qualified EFA.Application.Optimisation.Params as Params

import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs

import EFA.Application.Optimisation.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt

--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.State.Quantity as StateQty

--import qualified EFA.Graph.Topology as Topology
--import qualified EFA.Graph as Graph

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.IO.TableParser as Table

--import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Equation.Arithmetic as Arith

--import qualified EFA.Flow.Draw as Draw

--import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vlast)

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

{-

initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  Params.Optimisation Node list sweep vec a->
  EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph
-}

main1 :: IO()
main1 = do

  tabEta <- Table.read "../maps/eta.txt"

  tabPower <- Table.read "../maps/power.txt.bak"

  --rndGen <- Random.getStdGen

  let etaMap =
         Map.mapKeys Params.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys Params.unName ModSet.scaleTableEta)
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
      initEnv = AppOpt.storageEdgeXFactors optParams 3 3
             $ AppOpt.initialEnv optParams System.stateFlowGraph

      supportPoints =
        Base.supportPoints
          [ TopoIdx.ppos System.LocalRest System.LocalNetwork,
            TopoIdx.ppos System.Rest System.Network ]
          (Base.convertRecord reqsRecStep)
          (map (Sig.findSupportPoints. Sig.untype)
               [ ModSet.varLocalPower1D, ModSet.varRestPower1D])

      sysParams = Params.System {
         Params.systemTopology = System.topology,
         Params.etaAssignMap = System.etaAssignMap,
         Params.etaMap = etaMap,
         Params.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
         Params.initStorageState = ModSet.initStorageState,
         Params.initStorageSeq = ModSet.initStorageSeq }

      optParams :: Params.Optimisation Node [] Sweep UV.Vector Double
      optParams = Params.Optimisation {
--          Params.stateFlowGraphOpt = ienv,
          Params.reqsPos = (ReqsAndDofs.reqsPos ModSet.reqs),
          Params.dofsPos = (ReqsAndDofs.dofsPos ModSet.dofs),
          Params.points = ModSet.sweepPts,
          Params.sweepLength = ModSet.sweepLength,
          Params.etaToOptimise = Nothing,
          Params.maxEtaIterations = Params.MaxEtaIterations 5,
          Params.maxBalanceIterations = Params.MaxBalanceIterations 100,

          Params.initialBattForcing =
            Balance.ForcingMap
            $ Map.fromList [(System.Water, Balance.DischargeDrive 1)],
          Params.initialBattForceStep =
            Balance.ForcingMap
            $ Map.fromList [(System.Water, Balance.ChargeDrive 0.1)],
          Params.etaThreshold = Params.EtaThreshold 0.2,
          Params.balanceThreshold = Params.BalanceThreshold 0.5,
          Params.balanceForcingSeed = Balance.ChargeDrive 0.01 }

      simParams :: Params.Simulation Node [] Double
      simParams = Params.Simulation {
          Params.varReqRoomPower1D = Sig.convert $ ModSet.varLocalPower1D,
          Params.varReqRoomPower2D = Sig.convert $ ModSet.varRestPower ,
          Params.reqsRec = Base.convertRecord reqsRecStep,
          Params.requirementGrid = [ Sig.convert $ ModSet.varLocalPower1D,
                                  Sig.convert $ ModSet.varRestPower1D ],
          Params.activeSupportPoints =  supportPoints,
          Params.sequFilterTime=0.01,
          Params.sequFilterEnergy=0 }

  print $ map (AppUt.absoluteStateIndex (Params.systemTopology sysParams)) System.flowStates


{-
  void $ forcingSweep optParams reqsRec
       $ AppOpt.storageEdgeXFactors optParams 3 3
       $ AppOpt.initialEnv optParams System.stateFlowGraph
-}



  let


      ol = -- Loop.condition optParams
           take 5 $ Loop.iterateEtaWhile
               sysParams optParams simParams initEnv Balance.StateForcingOn

      initEnv2 = -- Loop.stateFlowOut $
                 Type.stateFlowGraphSweep $ Loop.bResult $ -- vlast "Main" $
                 vlast "Main" $ (Loop.balanceLoop $ vlast "Main" ol)

      ol2 = Loop.iterateEtaWhile sysParams optParams simParams initEnv2 Balance.StateForcingOff

  -- print ol
  mapM_ putStrLn (Loop.showEtaLoop optParams ol)
  mapM_ putStrLn (Loop.showEtaLoop optParams ol2)

  -- let g = fmap (vhead "simulationGraphs" . Sweep.toList)

{-
  Draw.xterm
    $ Draw.title "Initial State Flow Graph for Optimisation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g ienv
-}

--  print reqsRecStep
--  print supportPoints


--  mapM_ putStrLn (Loop.showEtaLoop optParams ol)
  --concurrentlyMany_ [
    --ModPlot.record ModPlot.gpXTerm "Requirement Signals" reqsRec,
    --ModPlot.record ModPlot.gpXTerm "Requirement Signals Stepped" reqsRecStep,
    --ModPlot.reqsRec ModPlot.gpXTerm reqsRec,
--    Loop.checkRangeIO sysParams optParams simParams]
    -- sequence_ (Loop.printEtaLoop optParams ol)]


--  concurrentlyMany_ [sequence_ (Loop.printEtaLoop optParams ol2)]

{-
  concurrentlyMany_ [
    ModPlot.maxEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaPerState ModPlot.gpXTerm opt2,
    ModPlot.expectedEtaDifferencePerState ModPlot.gpXTerm opt2 ]

-}

  return ()



main :: IO ()
main = main1
