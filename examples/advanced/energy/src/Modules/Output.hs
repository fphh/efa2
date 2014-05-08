module Modules.Output where


import qualified Modules.Input.System as System
-- import qualified Modules.Output.Loop as Loop
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Draw as Draw
import Modules.Output.Plot as ModPlot
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import EFA.Application.Optimisation.Sweep (Sweep)
import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Application.Optimisation.Loop as Loop
import EFA.Application.Optimisation.Loop(BalanceLoopItem(BalanceLoopItem),bFStep)
import qualified EFA.Flow.Part.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx
import EFA.Utility.Async (concurrentlyMany_)
import Text.Printf (printf)

import EFA.Utility.List (vlast, vhead)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

dir (_n, BalanceLoopItem _bForcing _bFStep _bal _opt) = printf "outer-loop-%6.6d" _n

topologyWithStates term params = concurrentlyMany_ [
  term $ Draw.labeledTopology $ Params.labeledTopology params,
  term $ Draw.flowTopologies $ StateAnalysis.advanced $ Params.systemTopology params ]

initialStateFlow sfg = Draw.xterm
    $ Draw.title "Initial State Flow Graph for Optimisation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g sfg
    where g = fmap (vhead "simulationGraphs" . Sweep.toList)

requirement reqsRec =
  concurrentlyMany_ [
    ModPlot.record ModPlot.gpXTerm "Requirement Signals" reqsRec,
--    ModPlot.record ModPlot.gpXTerm "Requirement Signals Stepped" reqsRecStep,
    ModPlot.reqsRec ModPlot.gpXTerm reqsRec]

sweepStack term params _sweep balanceForcing = concurrentlyMany_ [
  ModPlot.sweepStackPerStateEta term params _sweep,
  ModPlot.sweepStackPerStateStoragePower term params System.Water _sweep,
  ModPlot.sweepStackPerStateOpt term params balanceForcing _sweep,
  ModPlot.sweepStackPerStateCondition term params  _sweep
  ]

sweepStackGraphs sweep = concurrentlyMany_ [
  ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 0 sweep,
  ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 1 sweep,
  ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 2 sweep,
  ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 3 sweep
  ]

maxPerState _term (_n, BalanceLoopItem _bForcing _bFStep _bal _opt) =
  concurrentlyMany_ [
  ModPlot.maxIndexPerState (_term _n) _opt,
  ModPlot.maxEtaPerState (_term _n) _opt,
  ModPlot.maxPosPerState (_term _n) (StateIdx.power (Idx.State 0) System.Water System.Network) _opt]

simulation _term (_n, BalanceLoopItem _bForcing _bFStep _bal _opt)=
  concurrentlyMany_ [
 --   ModPlot.givenSignals (_term _n) _opt,
--    ModPlot.simulationSignals _term _opt,
    ModPlot.simulationGraphs _term _opt]


    -- das aktiviert das schreiben der zustandsflussgraphen
    -- pro parzelle (Achtung, ziemlich viel!!!)
    -- ModPlot.optimalObjectivePerState (ModPlot.dotPNG dir bStep) opt
 --    ModPlot.simulationSignals term opt2

iterationLoop optParams xs = mapM_ putStrLn (Loop.showEtaLoop optParams xs)


plotOptEtavsAvg opt =  concurrentlyMany_ [
  ModPlot.maxEtaPerState ModPlot.gpXTerm opt,
  ModPlot.expectedEtaPerState ModPlot.gpXTerm opt,
  ModPlot.expectedEtaDifferencePerState ModPlot.gpXTerm opt ]

plotRangeResults optParams simParams swp loop =
  do
   let b@(_, Loop.BalanceLoopItem _bForcing _bFStep _bal opt) = vhead "plotRangeResults" $ loop
       term = ModPlot.gpXTerm
       _posLocal = TopoIdx.Position System.LocalRest System.LocalNetwork
       _posRest = TopoIdx.Position System.Rest System.Network
       _posWater = TopoIdx.Position System.Network System.Water
       _posGas = TopoIdx.Position System.LocalNetwork System.Gas
       _posTrafo = TopoIdx.Position System.LocalNetwork System.Network

   concurrentlyMany_ [
--        putStrLn $ showBalanceLoopItem optParams b,
        ModPlot.reqsRec term $ Params.reqsRec simParams,
        ModPlot.sweepStackPerStateCondition term optParams swp,
        ModPlot.stateRange2 term opt,
        --ModPlot.maxState term opt,
        --ModPlot.maxEta term opt,
        --ModPlot.maxObj term opt,
        --ModPlot.maxPos _posLocal term opt,
        --ModPlot.maxPos _posRest term opt,
        --ModPlot.maxPos _posWater term opt,
        --ModPlot.maxPos _posGas term opt,
        --ModPlot.maxPos _posTrafo term opt,
        --ModPlot.givenSignals term opt,
        ModPlot.simulationSignals term opt,
        ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.3, 0.5] 0 swp
        ]


-- plotLoopResults optParams loop =  sequence_ (ModLoop.printEtaLoop optParams loop)
loopResults ::
  Monad m =>
  Params.Optimisation
  node [] Sweep UV.Vector a1
  -> (Params.Optimisation
      node [] Sweep UV.Vector a1
      -> (Loop.Counter,
          Loop.EtaLoopItem
          node Sweep UV.Vector a1 z)
      -> m a)
  -> (Params.Optimisation
      node [] Sweep UV.Vector a1
      -> (Loop.Counter, BalanceLoopItem node a1 z) -> m a)
  -> [Loop.EtaLoopItem
      node Sweep UV.Vector a1 z]
  -> m ()
loopResults optParams printEtaLoopItem printBalanceLoopItem loop
  = sequence_ $ Loop.iterateLoops optParams printEtaLoopItem printBalanceLoopItem (zip [0..] loop)


{-     do
        let _opt = vlast "printEtaLoopItem" _res
            balanceForcing = ilBForcOut $ vlast "printEtaLoopItem" res
-}
 