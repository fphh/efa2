{-# LANGUAGE FlexibleContexts #-}


module Main where

import qualified Modules.Input.System as System; import Modules.Input.System (Node)
import qualified EFA.Application.Utility as AppUt
import qualified Modules.Input.Setting as ModSet
--import qualified EFA.Application.Optimisation.Base as Base
--import qualified EFA.Application.Optimisation.NonIO as NonIO
--import qualified Modules.Output.Plot as ModPlot
--import qualified Modules.Types as Types

import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Application.Optimisation.Balance as Balance

import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs
import EFA.Application.Optimisation.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt
import EFA.Application.Optimisation.Params (Name(Name))

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

import EFA.Utility.Filename((+++),FPath(..),DirPath(..),Abs,Rel,Directory(..),FileName(..),fromString,filename)
import EFA.Utility(Caller(..),merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified EFA.Reference.Base as Ref
import qualified EFA.IO.Reference as RefIO

import qualified EFA.Flow.Topology.Index as TopoIdx

import Control.Monad (void)

modul :: ModuleName
modul = ModuleName "Examples.Advanced.Vehicle.Test"

nc :: FunctionName -> Caller
nc = genCaller modul

group :: DirPath Rel
group = DirPath $ map Directory ["Examples","Advanced","Energy"]

testPath :: DirPath Rel
testPath = DirPath [Directory "First"]


test :: Ref.Test
test =  Ref.Test testPath  
        (Map.fromList [(FPath [] (FileName "Var1"), Ref.toData (2 ::Double))])

{-
iterateBalanceIO ::
  Params.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Types.EnvResult Node (Sweep UV.Vector Double) ->
  IO ()
iterateBalanceIO params reqsRec stateFlowGraphOpt = do

  let perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      Types.Optimisation quasi sim =
        NonIO.optimiseAndSimulate params reqsRec perStateSweep

  writeFile (test ++ "/perStateSweep.txt") (show $ Types.perStateSweep quasi)
  writeFile (test ++ "/optimalObjectivePerState.txt") (show $ Types.optimalObjectivePerState quasi)
  writeFile (test ++ "/optimalState.txt") (show $ Types.optimalState quasi)

  Draw.dot (test ++ "/stateFlowGraph.dot")
           $ Draw.stateFlowGraph Draw.optionsDefault
           $ Types.stateFlowGraph sim

  Draw.dot (test ++ "/sequenceFlowGraph.dot")
           $ Draw.seqFlowGraph Draw.optionsDefault
           $ Types.sequenceFlowGraph sim

  writeFile (test ++ "/signals.txt") (show $ Types.signals sim)


initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  Params.OptimalEnvParams Node list sweep vec a->
  Types.EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph


writeOptParams :: Params.OptimalEnvParams Node [] Sweep UV.Vector Double -> IO ()
writeOptParams params = do

  Draw.dot (test ++ "/systemTopology.dot")
           $ Draw.topology (Params.systemTopology params)

  writeFile (test ++ "/etaAssignMap.txt") (show $ Params.etaAssignMap params)
  -- writeFile (test ++ "/points.txt") (show $ Params.points params)
  writeFile (test ++ "/dofsPos.txt") (show $ Params.dofsPos params)
  writeFile (test ++ "/reqsPos.txt") (show $ Params.reqsPos params)
  writeFile (test ++ "/sweepLength.txt") (show $ Params.sweepLength params)
-}



main :: IO()
main = do
  
  tmpFolder <- RefIO.getTmpFolder (nc "main")
  ok <- RefIO.checkFolder tmpFolder
  print tmpFolder
  print ok
  print test
  RefIO.writeTest tmpFolder group test
  ref <- RefIO.readTest (nc "main") tmpFolder group testPath
  print $ Ref.diffTest (Ref.Ref ref) test
  
  
 
  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt.bak"

  let etaMap =
         Map.mapKeys Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys (\(Name str) -> str) ModSet.scaleTableEta)
            tabEta
{-
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


      reqsPos = ReqsAndDofs.unReqs $ ReqsAndDofs.reqsPos ModSet.reqs

      reqsRec :: Record.PowerRecord Node Vector Double
      reqsRec =
        Record.Record (Sig.convert time)
                      (Map.fromList (zip reqsPos [prest, plocal]))


      -- optParams :: Params.OptimalEnvParams Node [] Sweep UV.Vector Double
-}
  let {- sysParams = Params.System {
         Params.systemTopology = System.topology,
         Params.etaAssignMap = System.etaAssignMap,
  --       Params.etaMap = etaMap,
         Params.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
         Params.initStorageState = ModSet.initStorageState,
         Params.initStorageSeq = ModSet.initStorageSeq } -}
      
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

          
  print optParams   

{- -}

{-
  writeOptParams optParams

  iterateBalanceIO optParams reqsRec
        ( AppOpt.storageEdgeXFactors optParams 3 3
          $ AppOpt.initialEnv optParams System.stateFlowGraph)
-}

