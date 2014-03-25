{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}


module Modules.Optimisation.Loop where
--import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm


import qualified Modules.System as System
import Modules.System (Node)
import qualified Modules.Plot as ModPlot
import qualified Modules.Utility as ModUt
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO

-- import EFA.Equation.Result(Result(Determined))
import qualified EFA.Application.Type as Type
import EFA.Application.Type (EnvResult)
import qualified EFA.Application.OneStorage as One
--import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Utility.List as UtList
--import qualified EFA.Graph as Graph
--import EFA.Graph (Graph)
--import qualified EFA.Flow.Draw as Draw

--import qualified EFA.Graph.Topology as Topology
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~/), (~+))
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Terminal as Terminal

--import EFA.Equation.Result (Result(Determined))

--import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.State.SystemEta as StateEta
--import qualified EFA.Flow.State.Quantity as StateQty
--import qualified EFA.Flow.State as FlowState
import qualified EFA.Report.FormatValue as FormatValue

-- import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
--import EFA.Signal.Data (Data(Data)) --, Nil, Apply)

import EFA.Utility.List (vlast, vhead) 
import EFA.Utility.Async (concurrentlyMany_)

--import qualified Data.Monoid as Monoid
import qualified Data.Map as Map; import Data.Map (Map)
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
--import Data.Vector (Vector)
--import Data.Maybe (fromMaybe)

--import qualified Data.Set as Set
--import qualified Data.Bimap as Bimap
--import Data.Bimap (Bimap)
import Data.Tuple.HT (thd3)

import Text.Printf (printf, PrintfArg) --,IsChar)

import EFA.Utility.Trace (mytrace)

import Debug.Trace

type Counter = Int

data BalanceLoopItem node a z =
  BalanceLoopItem {
    bForcing :: One.BalanceForcing node a,
    bFStep :: One.BalanceForcingStep node a,
    balance :: One.Balance node a,
    bResult :: z } deriving (Show)


data EtaLoopItem node sweep vec a z =
  EtaLoopItem {
    stateFlowIn :: EnvResult node ((sweep :: (* -> *) -> * -> *) vec a),
    sweep :: Type.Sweep node sweep vec a,
    balanceLoop :: [BalanceLoopItem node a z] } deriving (Show)


data StateForceDemand =
  MoreForcingNeeded
  | CorrectForcing
  | NoForcingNeeded
  | LessForcingNeeded deriving (Show)


checkBalance ::
  (Ord a, Arith.Sum a) =>
  One.OptimisationParams node list sweep vec a ->
  One.Balance node a ->
  Bool
checkBalance optParams bal =
  Map.foldl' (\acc v -> acc && Arith.abs v <= bt) True bal
  where bt = One.unBalanceThreshold (One.balanceThreshold optParams)

checkBalanceSingle ::
  (Ord a, Arith.Sum a, Ord node, Show node) =>
  One.OptimisationParams node list sweep vec a ->
  One.Balance node a ->
  node ->
  Bool
checkBalanceSingle optParams bal sto =
  Arith.abs x <= One.unBalanceThreshold (One.balanceThreshold optParams)
  where x = One.getStorageBalance "checkBalanceSingle" bal sto
    
-- | Rate Balance Deviation by sum of Standard Deviation and Overall Sum 
balanceDeviation :: 
  (Arith.Product a,
   Ord a,
   Arith.Constant a,
   Arith.Sum a) =>
  One.Balance node a -> a
balanceDeviation m =
  Map.foldl (\acc x -> acc ~+ Arith.square x) Arith.zero m
  ~+ 
  Arith.abs (Map.foldl (~+) Arith.zero m)

-----------------------------------------------------------------

cond :: Int -> (a -> Bool) -> [a] -> [a]
cond n p =  take n . UtList.takeUntil p

-----------------------------------------------------------------

oneIterationOfAllStorages ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a) ->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  [BalanceLoopItem node a z]
oneIterationOfAllStorages fsys accessf force steps =
  Map.foldlWithKey f [] (One.unBalanceForcingMap force)
  where f acc sto _ = acc ++ iterateOneStorage fsys accessf force steps sto


balanceIterationInit ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a) ->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  ( One.BalanceForcing node a,
    One.BalanceForcingStep node a,
    [BalanceLoopItem node a z] )
balanceIterationInit fsys accessf balForceIn balStepsIn =
  (balForceIn, balStepsIn, oioas)
  where oioas = oneIterationOfAllStorages fsys accessf balForceIn balStepsIn

balanceIterationAlgorithm ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a) ->
  ( One.BalanceForcing node a,
    One.BalanceForcingStep node a, 
    [BalanceLoopItem node a z]) ->
  ( One.BalanceForcing node a,
    One.BalanceForcingStep node a,
    [BalanceLoopItem node a z] )
balanceIterationAlgorithm fsys accessf (forcing, stepping, as) =
  (forcing1, stepping1, oioas)
  where oioas = oneIterationOfAllStorages fsys accessf forcing1 stepping1
        forcing1 = bForcing lastElem
        stepping1 = bFStep lastElem
        lastElem = ModUt.ifNull (error "balanceIteration: empty list") last as

balanceIterationCondition ::
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  (u, v, [BalanceLoopItem node a z]) ->
  (u, v, [BalanceLoopItem node a z])
balanceIterationCondition optParams (u, v, xs) = (u, v, cond maxBICnt bip xs)
  where maxBICnt = One.unMaxBalanceIterations $ One.maxBalanceIterations optParams
        bip _ = False

balanceIteration ::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  [BalanceLoopItem node a z]
balanceIteration optParams fsys accessf balForceIn balStepsIn =
  concat $ map thd3 $ iterate go bii
  where bii = balanceIterationCondition optParams
              $ balanceIterationInit fsys accessf balForceIn balStepsIn
        go = balanceIterationCondition optParams
             . balanceIterationAlgorithm fsys accessf


-----------------------------------------------------------------

iterateOneStorageInit ::
  (Ord a2, Ord node, Show a2, Show node, Arith.Constant a2) =>
  (One.BalanceForcing node a2 -> z) ->
  (z -> One.Balance node a2) ->
  One.BalanceForcing node a2 ->
  One.BalanceForcingStep node a2 ->
  node ->
  ((Maybe a, Maybe a1), BalanceLoopItem node a2 z)
iterateOneStorageInit fsys accessf forcingIn steppingIn sto =
  (initBestPair, BalanceLoopItem forcingIn initStep initBal initResult)
  where
    initResult = fsys forcingIn
    initBal = accessf initResult
    initBestPair = (Nothing, Nothing)

    initStep = steppingIn

iterateOneStorageAlgorithm ::
  (Ord node, Ord a, Show node, Show a, Arith.Constant a) =>
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a) ->
  node ->
  ( (Maybe (One.SocDrive a, a),
    Maybe (One.SocDrive a, a)),
    BalanceLoopItem node a t) ->
  ( (Maybe (One.SocDrive a, a),
    Maybe (One.SocDrive a, a)),
    BalanceLoopItem node a z)
iterateOneStorageAlgorithm fsys accessf sto
                           (bestPair, BalanceLoopItem force step _ _) =
  (bestPair1, BalanceLoopItem force1 step1 bal1 res1)
  where force1 = One.addForcingStep force step sto
        res1 = fsys force1
        bal1 = accessf res1
        bestPair1 = One.rememberBestBalanceForcing bestPair (force1, bal1) sto
        step1 = calculateNextBalanceStep bal1 bestPair1 step sto

iterateOneStorage ::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  node ->
  [BalanceLoopItem node a z]
iterateOneStorage fsys accessf forcingIn steppingIn sto = 
  map snd $ iterate go iosi
  where iosi = iterateOneStorageInit fsys accessf forcingIn steppingIn sto
        go = iterateOneStorageAlgorithm fsys accessf sto

-----------------------------------------------------------------


calculateNextBalanceStep :: 
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, Show a,
   Ord node, Show node) =>
  One.Balance node a ->
  (Maybe (One.SocDrive a,a), Maybe (One.SocDrive a,a)) -> 
  (One.BalanceForcingStep node a) ->
  node ->
  (One.BalanceForcingStep node a)
calculateNextBalanceStep balMap bestPair stepMap sto =
  One.updateForcingStep stepMap sto $ One.setSocDrive step1
  where
    bal = One.getStorageBalance "calculateNextBalanceStep" balMap sto
    step = One.getStorageForcingStep "calculateNextBalanceStep" stepMap sto
    fact = Arith.fromRational 2.0
    divi = Arith.fromRational 1.7
    intervall = One.getForcingIntervall bestPair


    step1 =
      case (intervall, Arith.sign bal) of   

           -- Zero Crossing didn't occur so far -- increase step to search faster
           (Nothing, Negative) -> Arith.abs $ (One.getSocDrive step) ~* fact
           (Nothing, Positive) -> Arith.negate $ (Arith.abs $ One.getSocDrive step) ~* fact

           -- The Zero Crossing is contained in the intervall
           -- defined by bestPair - step just a little over the middle
           (Just int, Negative) -> (One.getSocDrive int) ~/ divi
           (Just int, Positive) -> (Arith.negate $ One.getSocDrive  int) ~/ divi
           (_, Zero)  -> Arith.zero

-----------------------------------------------------------------
iterateEtaWhileInit ::
  (RealFloat a, Show a, UV.Unbox a, Arith.ZeroTestable a, Arith.Constant a) =>
  One.SystemParams Node a ->
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  One.SimulationParams Node [] a ->
  EnvResult Node (Sweep UV.Vector a) ->
  One.StateForcing ->
  -- (One.BalanceForcing Node a,
         EtaLoopItem Node Sweep UV.Vector a
           (Type.SignalBasedOptimisation Node Sweep UV.Vector a [] b [] c [] a) -- )
iterateEtaWhileInit sysParams optParams simParams sfgIn statForcing =
  EtaLoopItem sfgIn initSwp initRes

  where initBalF = One.initialBattForcing optParams
        balStepsIn = One.initialBattForceStep optParams
   
        accessf x =
          StateEta.balanceFromRecord
            (One.storagePositions sysParams)
            (Type.signals (Type.simulation x))


        initFsys balanceForcing =
          NonIO.optimiseAndSimulateSignalBased
            sysParams optParams simParams 
            balanceForcing statForcing initSwp initIdxConvMap

        initIdxConvMap =
          ModUt.indexConversionMap (One.systemTopology sysParams) sfgIn


        initSwp = Base.perStateSweep sysParams optParams sfgIn
        initRes = balanceIteration optParams initFsys accessf initBalF balStepsIn

iterateEtaWhileAlgorithm ::
  (RealFloat a, Show a, UV.Unbox a, Arith.ZeroTestable a, Arith.Constant a) =>
  One.SystemParams Node a ->
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  One.SimulationParams Node [] a ->
  One.StateForcing ->
  EtaLoopItem Node Sweep UV.Vector a
    (Type.SignalBasedOptimisation
      Node Sweep UV.Vector a intVec b1 simVec c1 efaVec d) ->
  EtaLoopItem Node Sweep UV.Vector a
    (Type.SignalBasedOptimisation
      Node Sweep UV.Vector a [] b [] c [] a)
iterateEtaWhileAlgorithm sysParams optParams simParams stateForcing
                         (EtaLoopItem sfg _ res) =
  EtaLoopItem sfg1 swp1 res1
  where sfg1 = Type.stateFlowGraphSweep (bResult lastElem)
        swp1 = Base.perStateSweep sysParams optParams sfg

        fsys balanceForcing =
          NonIO.optimiseAndSimulateSignalBased
            sysParams optParams simParams 
            balanceForcing stateForcing swp1 idxConvMap

        idxConvMap = ModUt.indexConversionMap System.topology sfg

        res1 = balanceIteration optParams fsys accessf bfIn balStepsIn

        err str = error $
          "iterateEtaWhileAlgorithm: empty "
          ++ str ++ " balanceIteration\n"
          ++ "probable cause: Maybe your iteration condition is always false?"

        lastElem = ModUt.ifNull (err "outer") last res

        bfIn = bForcing lastElem

        balStepsIn = One.initialBattForceStep optParams
        accessf x =
          StateEta.balanceFromRecord
            (One.storagePositions sysParams)
            (Type.signals (Type.simulation x))


iterateEtaWhileCondition ::
  (Ord a, Arith.Sum a) =>
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  EtaLoopItem Node Sweep UV.Vector a z ->
  EtaLoopItem Node Sweep UV.Vector a z
iterateEtaWhileCondition optParams (EtaLoopItem u v res) =
  EtaLoopItem u v (cond maxBICnt bip res)
  where maxBICnt = One.unMaxBalanceIterations $ One.maxBalanceIterations optParams
        bip = checkBalance optParams . balance

iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   z ~ Type.SignalBasedOptimisation Node Sweep UV.Vector a [] b0 [] c0 [] a,
   Arith.Constant a,RealFloat a, d ~ a) =>
  One.SystemParams Node a ->
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  One.SimulationParams Node [] a ->
  EnvResult Node (Sweep UV.Vector a) ->
  One.StateForcing ->
  [EtaLoopItem Node Sweep UV.Vector a z]
iterateEtaWhile sysParams optParams simParams sfgIn statForcing =
  take maxEICnt $ iterate go iewi
  where maxEICnt = One.unMaxEtaIterations $ One.maxEtaIterations optParams
        iewi = iterateEtaWhileCondition optParams
               $ iterateEtaWhileInit sysParams optParams simParams sfgIn statForcing
        go = iterateEtaWhileCondition optParams
             . iterateEtaWhileAlgorithm sysParams optParams simParams statForcing

-------------------------------- OutPut Below -----------------------------------

concatZipMapsWith :: ((k0, a) -> (k1, b) -> [c]) -> Map k0 a -> Map k1 b -> [c]
concatZipMapsWith f s t = concat $ zipWith f (Map.toList s) (Map.toList t)

printfMap :: (Show k, Show a) => Map k a -> String
printfMap m = Map.foldWithKey f "" m
  where f k v acc = printf "%16s\t%16s\n" (show k) (show v) ++ acc

printfStateMap ::
  (PrintfArg k, PrintfArg v) =>
  Map Idx.AbsoluteState One.StateForcing ->
  Map k v -> [Char]
printfStateMap forceMap timeMap = concatZipMapsWith f forceMap timeMap
  where f (Idx.AbsoluteState k, x) (_, y) = printf "S%2d%16sf%5.3f" k (show x) y

printfBalanceFMap ::
  (Show node, PrintfArg a1, PrintfArg t1, Arith.Constant a1) =>
  One.BalanceForcing node a1 ->
  Map t t1 ->
  [Char]
printfBalanceFMap forceMap balanceMap =
  concatZipMapsWith f (One.unBalanceForcingMap forceMap) balanceMap
  where f (k, x) (_, y) =
          printf "   | Sto: %7s   | F: %10.15f   | B: %10.15f"
                 (show k) (One.getSocDrive x) y

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map a t -> String
printfBalanceMap balanceMap = Map.foldWithKey f "" balanceMap
  where f k v acc = printf " Sto: %7s B: %5.3f" (show k) v ++ acc
          
showEtaLoop ::
  (Show node, Show a, PrintfArg a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] ->
  [String]
showEtaLoop optParams loop =
  iterateLoops optParams showEtaLoopItem showBalanceLoopItem (zip [0..] loop)

iterateLoops ::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.OptimisationParams node [] Sweep UV.Vector a -> 
      (Counter, EtaLoopItem node Sweep UV.Vector a z) -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a -> 
      (Counter, BalanceLoopItem node a z) -> o) ->
  [(Counter, EtaLoopItem node Sweep UV.Vector a z)]->
  [o]
iterateLoops optParams elf blf etaLoop =
  concatMap g etaLoop
  where g x = elf optParams x
              : map (blf optParams) (zip [0..] (balanceLoop (snd x)))

showEtaLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
  String
showEtaLoopItem _optParams (step, EtaLoopItem _sfgIn _sweep _) =
  printf "EL: %8d" step

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (Counter, BalanceLoopItem node a z) ->
  String
showBalanceLoopItem _optParams (bStp, BalanceLoopItem bForc _bFStep bal _) =
  printf " BL: %2d | " bStp ++ printfBalanceFMap bForc bal

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
  One.OptimisationParams node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] ->
  [IO ()]
printEtaLoop optParams ol =
  iterateLoops optParams printEtaLoopItem printBalanceLoopItem (zip [0..] ol)


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