module Modules.Output.Loop where

import qualified Modules.Input.System as System
import Modules.Input.System (Node)
import qualified Modules.Output.Plot as ModPlot
import qualified EFA.Signal.Vector as SV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Application.Type as Type
import EFA.Application.Optimisation.Sweep (Sweep)
import qualified  EFA.Application.Optimisation.Params as Params
import qualified  EFA.Application.Optimisation.Loop as Loop

import Text.Printf (printf, PrintfArg) --,IsChar)
import qualified EFA.Equation.Arithmetic as Arith

{-
printEtaLoop::
  (SV.Walker efaVec,UV.Unbox b,b~Double,
   SV.Storage efaVec Double,
   SV.FromList efaVec,
   FormatValue.FormatValue b,
   UV.Unbox a,Show (intVec Double),Show (simVec Double),
   Node.C node,SV.Walker simVec,Ord a,Fractional a,
   SV.Storage simVec Double,
   SV.FromList simVec,
   FormatValue.FormatValue a,
   z ~ Type.SignalBasedOptimisation
         node sweep vec Double intVec b simVec c efaVec d,Show (efaVec Double),
   Show a, Show node, PrintfArg a, Arith.Constant a, a ~ Double, d ~ Double, vec ~ UV.Vector,
   sweep ~ Sweep) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  [Loop.EtaLoopItem node Sweep UV.Vector a z] ->
  [IO ()]
printEtaLoop optParams ol =
  iterateLoops optParams printEtaLoopItem printBalanceLoopItem (zip [0..] ol)
-}
{-
printEtaLoopItem ::
  (z ~ Type.SignalBasedOptimisation
       node Sweep UV.Vector a intVec b simVec c vec d,
   Show node, Show (vec a),
   UV.Unbox a,Fractional a,UV.Unbox b,
   SV.Walker vec,
   SV.Storage vec d,FormatValue.FormatValue b,
   SV.FromList vec,
   FormatValue.FormatValue d,
   Arith.ZeroTestable d,
   Arith.Constant d,
   Node.C node,Ord a, Show a,
   FormatValue.FormatValue a) =>
   One.OptimisationParams node [] Sweep UV.Vector a ->
   (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
   IO ()
printEtaLoopItem _params _e@(_step, EtaLoopItem _sfgIn _sweep _res) =
  do
     --putStrLn $ showEtaLoopItem _params _e
     let -- dir = printf "outer-loop-%6.6d" olcnt
      _opt = vlast "printEtaLoopItem" _res
     putStrLn $ showEtaLoopItem _params _e

    --  stoPos = TopoIdx.Position System.Water System.Network
  --    gasPos = TopoIdx.Position System.Gas System.LocalNetwork
      --  _term = ModPlot.gpXTerm
      --  _balanceForcing =ilBForcOut $ vlast "printEtaLoopItem" res

--    ModPlot.sweepStackPerStateEta term params _sweep
--    ModPlot.sweepStackPerStateStoragePower term params System.Water _sweep
--    ModPlot.sweepStackPerStateOpt term params balanceForcing _sweep
--    ModPlot.sweepStackPerStateCondition term params  _sweep


     --concurrentlyMany_ [
--       ModPlot.simulationGraphs ModPlot.dotXTerm _opt,
--       ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 0 _sweep,
--       ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 1 _sweep,
--       ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 2 _sweep,
--       ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 3 _sweep,

      --ModPlot.maxEtaPerState ModPlot.gpXTerm opt,
 --     ModPlot.simulationGraphs ModPlot.dotXTerm opt ]
      --ModPlot.expectedEtaPerState ModPlot.gpXTerm opt,
      --ModPlot.maxObjPerState ModPlot.gpXTerm opt ]

--      ModPlot.drawSweepStateFlowGraph "sfgIn" 0 $ _sfgIn,
--        ModPlot.drawSweepStateFlowGraph "sfgOut" 0 $ _sfgOut,
--        print $ Type.powerSequence $ Type.analysis $ bResult _opt,
--        print $ Type.stateFlowGraph $ Type.analysis $ bResult _opt,
--        ModPlot.drawSweepStateFlowGraph "sfgOut" 0 $ Type.stateFlowGraphSweep $ bResult _opt,
        --ModPlot.simulationGraphs ModPlot.dotXTerm $ bResult _opt]
 {-     ModPlot.simulationSignals term opt,
      ModPlot.maxPos stoPos term opt,
      ModPlot.maxPos gasPos term opt,
      ModPlot.maxState term opt,
      ModPlot.maxObj term opt -}


{-
    ModPlot.maxPos stoPos (ModPlot.gpPNG dir 0) opt
    ModPlot.maxPos gasPos (ModPlot.gpPNG dir 0) opt
    ModPlot.maxState (ModPlot.gpPNG dir 0) opt
    ModPlot.maxEta (ModPlot.gpPNG dir 0) opt
-}


printBalanceLoopItem ::
  (Show node, Show a, PrintfArg a, Arith.Constant a, Show (intVec Double),Show (simVec Double),
   SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   Node.C node,
   z ~ Type.SignalBasedOptimisation
   node sweep vec Double intVec b simVec c efaVec d)=>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (Counter, BalanceLoopItem node a z) -> IO ()
printBalanceLoopItem _optParams _b@(_bStp, BalanceLoopItem _bForcing _bFStep _bal _opt) =
  do


    let  _gTerm = ModPlot.gpPNG _dir _bStp
         _xTerm = ModPlot.gpXTerm
         _term = _xTerm
         _dir = printf "outer-loop-%6.6d" _bStp
         _stoPos = TopoIdx.Position System.Water System.Network

    concurrentlyMany_ [
      putStrLn $ showBalanceLoopItem _optParams _b
--       ModPlot.maxIndexPerState _term _opt,
--       ModPlot.givenSignals _term _opt,
--       print (Map.map (Type.reqsAndDofsSignalsOfState) $
--              Type.interpolationPerState _opt),
--      print $ Type.signals $ Type.simulation $ _opt,
--      ModPlot.simulationSignals _term _opt

--      ModPlot.simulationGraphs (ModPlot.dot _dir _bFStep) _opt
--      ModPlot.givenSignals _term _opt
       ]
--       ModPlot.maxEta _xTerm opt2 -}
     --ModPlot.optimalObjs term _opt
--       ModPlot.stateRange2 term _opt,


--     ModPlot.simulationSignals term opt2

--     print (Type.reqsAndDofsSignals $ Type.interpolation opt2)
--     ModPlot.givenSignals term opt2
--    ModPlot.maxEtaPerState (ModPlot.gpPNG dir bStep) opt
   -- ModPlot.maxPosPerState (ModPlot.gpPNG dir bStep) stoPos opt

--     ModPlot.maxPos _stoPos term opt2

    -- das aktiviert das schreiben der zustandsflussgraphen
    -- pro parzelle (Achtung, ziemlich viel!!!)
    -- ModPlot.optimalObjectivePerState (ModPlot.dotPNG dir bStep) opt
 --    ModPlot.simulationSignals term opt2

--       ModPlot.maxState term opt2]
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir bStep) opt


{-
checkRangeIO ::
  One.SystemParams Node Double ->
  One.OptimisationParams Node [] Sweep UV.Vector Double ->
  One.SimulationParams Node [] Double ->
  EnvResult Node (Sweep UV.Vector Double) ->
  IO ()
checkRangeIO sysParams optParams simParams sfg = do
  let
      indexConversionMap =
        ModUt.indexConversionMap (One.systemTopology sysParams) sfg

      swp = Base.perStateSweep sysParams optParams sfg
      initBalF =  One.initialBattForcing optParams
      initialBalSteps = One.initialBattForceStep optParams
      statForcing = One.StateForcingOn

      fsys balanceForcing =
        NonIO.optimiseAndSimulateSignalBased
          sysParams optParams simParams
          balanceForcing statForcing swp indexConversionMap

      accessf x =
        StateEta.balanceFromRecord
          (One.storagePositions sysParams)
          (Type.signals (Type.simulation x))

      b@(_, BalanceLoopItem _bForcing _bFStep _bal opt) =
        (0, vhead "checkRangeIO"
            $ concat $ balanceIteration fsys accessf initBalF initialBalSteps)

      term = ModPlot.gpXTerm
      _posLocal = TopoIdx.Position System.LocalRest System.LocalNetwork
      _posRest = TopoIdx.Position System.Rest System.Network
      _posWater = TopoIdx.Position System.Network System.Water
      _posGas = TopoIdx.Position System.LocalNetwork System.Gas
      _posTrafo = TopoIdx.Position System.LocalNetwork System.Network

  --print $ Type.reqsAndDofsSignals $ Type.interpolation opt2

  concurrentlyMany_ [
    putStrLn $ showBalanceLoopItem optParams b,
    ModPlot.reqsRec term $ One.reqsRec simParams,
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
-}


{-
getStateTimes ::
  (Arith.Constant a) =>
  One.SystemParams Node a ->
  Map Idx.AbsoluteState a1 ->
  EnvResult Node a ->
  Map Idx.AbsoluteState (Result a)
getStateTimes sysParams stateForceIn sfg = Map.mapWithKey f stateForceIn
  where timeMap = Map.map FlowTopo.label $ StateQty.states sfg
        indexMap =
          Bimap.toMap
          $ ModUt.indexConversionMap (One.systemTopology sysParams) sfg
        f absIdx _ = fromMaybe (Determined Arith.zero) (Map.lookup absIdx timeMapAbs)
        timeMapAbs = Map.mapKeys g timeMap

        g st =
          fromMaybe
            (error $ "getStateTime: key " ++ show st ++ " not found in " ++ show indexMap)
            (Map.lookup st indexMap)


-- Find a solution where all states occur in the simlation signals at minimum state forcing,
-- measurement unit ist state duration
getStateTime ::
  Idx.State ->
  FlowState.Graph
    node edge sectionLabel nodeLabel
    storageLabel edgeLabel carryLabel ->
  sectionLabel
getStateTime stateIdx sfg = FlowTopo.label state
  where state = fromMaybe err $ Map.lookup stateIdx (StateQty.states sfg)
        err = error $ "Error in getStateTime: State " ++ show stateIdx
                      ++ " doesn't exist in StateflowGraph"

-}
