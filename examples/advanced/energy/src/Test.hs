{-# LANGUAGE FlexibleContexts #-}


{-

TODO

* Generisches Forcing für Speicherkanten.

* Maybes sollen mit defaultwert in plot geplottet werden.

* powerrecord aus eingelesenen Signalen bauen.

* interpolation in kombination mit nan prüfen

* addZeroCrossings funktioniert mit Listen

* Funktion um Section zu plotten.

* inaktive Kanten schon in extractOptimalPowerMatricesPerState erkennen
  sollen 0 Leistung liefern

* findZeroCrossing verstehen

* fromSequenceFlow Wirkung von allStEdges ???

* wiki-Artikel wg forall verbessern

* to2DMatrix, head entfernen

-}

module Main where

import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO
import qualified Modules.Plot as ModPlot
--import qualified Modules.Types as Types

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs
import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt
import EFA.Application.OneStorage (Name(Name))

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
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
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
  One.OptimalEnvParams Node list sweep vec a->
  Types.EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph


writeOptParams :: One.OptimalEnvParams Node [] Sweep UV.Vector Double -> IO ()
writeOptParams params = do

  Draw.dot (test ++ "/systemTopology.dot")
           $ Draw.topology (One.systemTopology params)

  writeFile (test ++ "/etaAssignMap.txt") (show $ One.etaAssignMap params)
  -- writeFile (test ++ "/points.txt") (show $ One.points params)
  writeFile (test ++ "/dofsPos.txt") (show $ One.dofsPos params)
  writeFile (test ++ "/reqsPos.txt") (show $ One.reqsPos params)
  writeFile (test ++ "/sweepLength.txt") (show $ One.sweepLength params)
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


      -- optParams :: One.OptimalEnvParams Node [] Sweep UV.Vector Double

      sysParams = One.SystemParams {
         One.systemTopology = System.topology,
         One.etaAssignMap = System.etaAssignMap,
         One.etaMap = etaMap,
         One.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
         One.initStorageState = ModSet.initStorageState,
         One.initStorageSeq = ModSet.initStorageSeq }

          
  print sysParams   

{- -}

{-
  writeOptParams optParams

  iterateBalanceIO optParams reqsRec
        ( AppOpt.storageEdgeXFactors optParams 3 3
          $ AppOpt.initialEnv optParams System.stateFlowGraph)
-}

