module Modules.NonIO where      

import qualified EFA.Application.Optimisation.Loop as Loop
import qualified EFA.Application.Optimisation.Balance as Balance
import qualified EFA.Application.Type as Type
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

iterationWithAllStates sysParams optParams simParams stateFlow = -- Loop.condition optParams
           take 5 $ Loop.iterateEtaWhile
               sysParams optParams simParams stateFlow Balance.StateForcingOn

getLastStateFlow xs = Type.stateFlowGraphSweep $ Loop.bResult $ 
                 vlast "Main" $ (Loop.balanceLoop $ vlast "Main" xs)

iterationWithBestStates sysParams optParams simParams initEnv = Loop.iterateEtaWhile sysParams optParams simParams initEnv Balance.StateForcingOff
