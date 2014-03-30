module Main where

import qualified Modules.NonIO as ModNonIO;
import qualified Modules.Input.Setting as ModSet
import qualified Modules.Output as Output
import qualified Modules.Input.System as System;
import qualified EFA.Flow.Topology.Index as TopoIdx
import Modules.Output.Plot as ModPlot
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vlast, vhead)

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

import qualified EFA.IO.TableParser as Table

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

  
  let
--    dir = printf "outer-loop-%6.6d" _bStp
--    _gTerm = ModPlot.gpPNG _dir _bStp
    _xTerm = ModPlot.gpXTerm
    _stoPos = TopoIdx.Position System.Water System.Network
    _gasPos = TopoIdx.Position System.Gas System.LocalNetwork
    
    printBalanceLoopItem _optParams _loopItem = 
      concurrentlyMany_ [
      
          ]
          
    printEtaLoopItem _params _loopItem = concurrentlyMany_ [
      
          ]
 {-     do
        let _opt = vlast "printEtaLoopItem" _res
            balanceForcing = ilBForcOut $ vlast "printEtaLoopItem" res
-}            
            
--  Output.plotRangeResults loop1
  
  Output.printIterationLoop optParams loop1
  Output.plotLoopResults optParams printEtaLoopItem printBalanceLoopItem loop1
  
  Output.printIterationLoop optParams loop2
  Output.plotLoopResults optParams printEtaLoopItem printBalanceLoopItem loop2
