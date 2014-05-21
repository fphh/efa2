module Modules.NonIO where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Application.Type as Type
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.Params as Params
import qualified Data.Vector.Unboxed as UV

import qualified EFA.Application.Optimisation.Loop as Loop
import qualified EFA.Application.Optimisation.Balance as Balance
--import qualified EFA.Application.Type as Type
import EFA.Utility.List (vlast)


{-
checkRange ::
  One.SystemParams Node Double ->
  One.OptimisationParams Node [] Sweep UV.Vector Double ->
  One.SimulationParams Node [] Double ->
  EnvResult Node (Sweep UV.Vector Double) ->
  IO ()
checkRange sysParams optParams simParams sfg = (0, vhead "checkRangeIO"
            $ concat $ balanceIteration fsys accessf initBalF initialBalSteps)
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
            $ concat $ balanceIteration fsys accessf initBalF initialBalSteps)-}

--checkRange = iterationWithAllStates
iterationWithAllStates ::
  (RealFloat a, Show node, Show a, Node.C node, Arith.Constant a, UV.Unbox a, Arith.ZeroTestable a) =>
  Params.System node a ->
 Params.Optimisation node [] Sweep.Sweep UV.Vector a ->
 Params.Simulation node [] a ->
 Type.EnvResult node (Sweep.Sweep UV.Vector a) ->
 [Loop.EtaLoopItem node Sweep.Sweep UV.Vector a (Type.SignalBasedOptimisation node Sweep.Sweep UV.Vector a [] b0 [] c0 [] a)]

iterationWithAllStates sysParams optParams simParams stateFlow = -- Loop.condition optParams
           take 5 $ Loop.iterateEtaWhile
               sysParams optParams simParams stateFlow Balance.StateForcingOn

getLastStateFlow ::
  [Loop.EtaLoopItem node1 sweep1 vec a1 (Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d)] ->
  Type.EnvResult node (sweep sweepVec a)
getLastStateFlow xs = Type.stateFlowGraphSweep $ Loop.bResult $
                 vlast "Main" $ (Loop.balanceLoop $ vlast "Main" xs)

iterationWithBestStates ::
  (RealFloat a, Show node, Show a, Node.C node, Arith.Constant a, UV.Unbox a, Arith.ZeroTestable a) =>
  Params.System node a ->
 Params.Optimisation node [] Sweep.Sweep UV.Vector a ->
 Params.Simulation node [] a ->
 Type.EnvResult node (Sweep.Sweep UV.Vector a) ->
 [Loop.EtaLoopItem node Sweep.Sweep UV.Vector a (Type.SignalBasedOptimisation node Sweep.Sweep UV.Vector a [] b0 [] c0 [] a)]

iterationWithBestStates sysParams optParams simParams initEnv = Loop.iterateEtaWhile sysParams optParams simParams initEnv Balance.StateForcingOff
