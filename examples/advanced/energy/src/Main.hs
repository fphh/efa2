{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified EFA.Graph.Topology.Node as Node
import qualified Modules.NonIO as ModNonIO;
import qualified Modules.Input.Setting as ModSet
import qualified Modules.Output as Output
import qualified Modules.Input.System as System;
import qualified EFA.Flow.Topology.Index as TopoIdx
import Modules.Output.Plot as ModPlot
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vlast, vhead)
import Text.Printf (printf)
import qualified  EFA.Application.Optimisation.Loop as Loop
import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Application.Type as Type
import Text.Printf (printf, PrintfArg)
{-

import qualified Modules.Input.System as System;
import Modules.Input.System (Node)
import qualified Modules.Input.Setting as ModSet

import qualified EFA.Application.Utility as AppUt
import qualified EFA.Application.Optimisation.Base as Base
import qualified EFA.Application.Optimisation.Loop as Loop
import qualified EFA.Application.Type as Type
import qualified EFA.Application.Optimisation.Balance as Balance
import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs
import EFA.Application.Optimisation.Sweep (Sweep)
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT


import qualified EFA.IO.TableParser as Table
import EFA.Utility.List (vlast)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
import qualified Data.Vector.Unboxed as UV

import qualified EFA.Flow.Topology.Index as TopoIdx
-}
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Application.Optimisation.Sweep (Sweep)
import qualified Data.Vector.Unboxed as UV
import qualified EFA.IO.TableParser as Table
import qualified EFA.Signal.Vector as SV
import Modules.Input.System (Node)

_term _ = ModPlot.gpXTerm
_dotTerm dir = ModPlot.dotXTerm dir
_stoPos = TopoIdx.Position System.Water System.Network
_gasPos = TopoIdx.Position System.Gas System.LocalNetwork
{-
printBalanceLoopItem ::
  (Show node,UV.Unbox b,
   SV.Walker efaVec,
   SV.Storage efaVec d,
   SV.FromList efaVec,
   Arith.ZeroTestable d,
   Arith.Constant d,
   FormatValue.FormatValue b,
   FormatValue.FormatValue d,
   Show a,
   PrintfArg a,
   Arith.Constant a)=>
  (z ~ Type.SignalBasedOptimisation System.Node
   Sweep
   sweepVec
   Double
   intVec
   b
   simVec
   c
   efaVec
   d) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Loop.Counter,Loop.BalanceLoopItem node a z) -> IO ()

printBalanceLoopItem _optParams _b = concurrentlyMany_ [
  putStrLn $ Loop.showBalanceLoopItem _optParams _b,
  Output.maxPerState _term  _b,
  Output.simulation _dotTerm _b
  ]
 -}
printBalanceLoopItem ::
  (UV.Unbox b,
   SV.Walker efaVec,
   SV.Storage efaVec d,
   SV.FromList efaVec,
   Arith.ZeroTestable d,
   Arith.Constant d,
   FormatValue.FormatValue b,
   FormatValue.FormatValue d,
   Show node, Show a, PrintfArg a, Arith.Constant a,
   Show (intVec Double),Show (simVec Double),
   SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   Node.C node,
   z ~ Type.SignalBasedOptimisation
   node sweep vec Double intVec b simVec c efaVec d)=>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Loop.Counter, Loop.BalanceLoopItem node a z) -> IO ()
printBalanceLoopItem _optParams _b@(_bStp, Loop.BalanceLoopItem _bForcing _bFStep _bal _opt) =
  do


    let  _gTerm = ModPlot.gpPNG _dir _bStp
         _xTerm = ModPlot.gpXTerm
         _term = _xTerm
         _dir = printf "outer-loop-%6.6d" _bStp
         _stoPos = TopoIdx.Position System.Water System.Network

    concurrentlyMany_ [
      putStrLn $ Loop.showBalanceLoopItem _optParams _b,
--      Output.maxPerState _term  _b,
      Output.simulation _dotTerm _b]


printEtaLoopItem _params _loopItem = concurrentlyMany_ [
  putStrLn $ Loop.showEtaLoopItem _params _loopItem
  ]


main :: IO()
main = do

  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt.bak"

  let
    sysParams = ModSet.sysParams tabEta
    optParams = ModSet.optParams
    demandedCycle = ModSet.reqsRec tabPower
    simParams = ModSet.simParams demandedCycle
    initStateFlow = ModSet.initEnv

  let -- r = NonIO.checkRange sysParams optParams simParams
      loop1 = ModNonIO.iterationWithAllStates sysParams optParams simParams initStateFlow
      stateFlow = ModNonIO.getLastStateFlow loop1
      loop2 = ModNonIO.iterationWithBestStates sysParams optParams simParams stateFlow


  Output.iterationLoop optParams loop1
--  Output.loopResults optParams printEtaLoopItem printBalanceLoopItem loop1

  Output.iterationLoop optParams loop2
--  Output.loopResults optParams printEtaLoopItem printBalanceLoopItem loop2
