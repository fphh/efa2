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

import EFA.Equation.Result(Result(Determined))
import qualified EFA.Application.Type as Type
import EFA.Application.Type (EnvResult)
import qualified EFA.Application.OneStorage as One
--import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

--import qualified EFA.Graph as Graph
--import EFA.Graph (Graph)
--import qualified EFA.Flow.Draw as Draw

--import qualified EFA.Graph.Topology as Topology
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~/), (~+))
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Terminal as Terminal

--import EFA.Equation.Result (Result(Determined))

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.State.SystemEta as StateEta
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State as FlowState
import qualified EFA.Report.FormatValue as FormatValue

-- import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(Data)) --, Nil, Apply)

import EFA.Utility.List (vlast, vhead) 
import EFA.Utility.Async (concurrentlyMany_)

--import qualified Data.Monoid as Monoid
import qualified Data.Map as Map; import Data.Map (Map)
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
--import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
--import Data.Bimap (Bimap)

import Text.Printf (printf, PrintfArg) --,IsChar)

type Counter = Int


data BalanceLoopItem node a z =
  BalanceLoopItem {
    bForcing :: One.BalanceForcing node a,
    bFStep :: One.BalanceForcingStep node a,
    balance :: One.Balance node a,
    bResult :: z }


data EtaLoopItem node sweep vec a z =
  EtaLoopItem {
    stateFlowIn :: EnvResult node ((sweep :: (* -> *) -> * -> *) vec a),
    sweep :: Type.Sweep node sweep vec a,
    stateFlowOut :: EnvResult node ((sweep :: (* -> *) -> * -> *) vec a),
    balanceLoop :: [BalanceLoopItem node a z] }


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


-- Rate Balance Deviation by sum of Standard Deviation and Overall Sum 
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

balanceIteration::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.BalanceForcing node a -> z) ->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  [BalanceLoopItem node a z]
balanceIteration optParams fsys accessf balForceIn balStepsIn =
  go balForceIn balStepsIn 
  where go forcing stepping = oneIterationOfAllStorages ++ go forcing1 stepping1

            -- hier lieber mit takeWhile arbeiten oder ganz von ausserhalb kontrollieren
            -- if checkBalance optParams bal then [] else go forcing1 stepping1

          where oneIterationOfAllStorages =
                  Map.foldlWithKey f [] (One.unBalanceForcingMap forcing)
                f acc sto _ = acc ++ iterateOneStorage fsys accessf forcing stepping sto

                bal = balance lastElem
                forcing1 = bForcing lastElem
                stepping1 = bFStep lastElem
                lastElem = vlast "balanceIteration" oneIterationOfAllStorages

iterateOneStorage ::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  (One.BalanceForcing node a -> z)->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  node ->
  [BalanceLoopItem node a z]
iterateOneStorage fsys accessf forcingIn steppingIn sto = 
  go forcingIn steppingIn initialResult (Nothing, Nothing)
  where
    initialResult = fsys forcingIn
    go force step res bestPair =
      BalanceLoopItem force step1 bal res : go force1 step1 res1 bestPair1
      where force1 = One.addForcingStep force step sto
            res1 = fsys force1
            bal1 = accessf res1
            bal = accessf res
            bestPair1 = One.rememberBestBalanceForcing bestPair (force1, bal1) sto
            step1 = calculateNextBalanceStep (force1, bal1) bestPair1 step sto

                   
calculateNextBalanceStep :: 
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, Show a,
   Ord node, Show node) =>
  (One.BalanceForcing node a, One.Balance node a) -> 
  (Maybe (One.SocDrive a,a), Maybe (One.SocDrive a,a)) -> 
  (One.BalanceForcingStep node a) ->
  node ->
  (One.BalanceForcingStep node a)
calculateNextBalanceStep (_, balMap) bestPair stepMap sto =
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


getStateTimes ::
  Arith.Constant a =>
  Map Idx.AbsoluteState a1 ->
  StateQty.Graph Node storageLabel (Result a) ->
  Map Idx.AbsoluteState (Result a)
getStateTimes stateForceIn sfg = Map.mapWithKey f stateForceIn
  where timeMap = Map.map FlowTopo.label $ StateQty.states sfg
        indexMap = Bimap.toMap $ ModUt.indexConversionMap System.topology sfg
        f absIdx _ = fromMaybe (Determined Arith.zero) (Map.lookup absIdx timeMapAbs)
        timeMapAbs = Map.mapKeys g timeMap

        g st = fromMaybe 
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
                      ++ "doesn't exist in StateflowGraph"


checkStateTimes ::
  (Num a, Ord a) =>
  One.OptimisationParams node [] Sweep vec a ->
  One.StateDurations a ->
  Map Idx.AbsoluteState (One.StateForcingStep a) ->
  Bool
checkStateTimes optParams stateDurs stateSteps =
  and $ zipWith g (Map.elems stateDurs) (Map.elems stateSteps)
  where g time step = (time > lThr) && ( time <= uThr || step == One.DontForceState )
        uThr = One.unStateTimeThreshold $ One.stateTimeUpperThreshold optParams
        lThr = One.unStateTimeThreshold $ One.stateTimeLowerThreshold optParams

iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   z ~ Type.SignalBasedOptimisation Node Sweep UV.Vector a [] b0 [] c0 [] a,
   Arith.Constant a,RealFloat a, d ~ a) =>
  One.SystemParams Node a ->
  One.OptimisationParams Node [] Sweep UV.Vector a ->
  One.SimulationParams Node [] a ->
  [EtaLoopItem Node Sweep UV.Vector a z]
iterateEtaWhile sysParams optParams simParams =
  go (One.stateFlowGraphOpt optParams) initBalF
  where
        initBalF = One.initialBattForcing optParams

        go sfg bfIn = EtaLoopItem sfg swp sfg1 res : go sfg1 bfOut
          where
            swp = Base.perStateSweep sysParams optParams sfg

            fsys balanceForcing =
              NonIO.optimiseAndSimulateSignalBased
                sysParams optParams simParams 
                balanceForcing statForcing swp indexConversionMap
      
            accessf x =
              StateEta.balanceFromRecord
                (One.storagePositions sysParams)
                (Type.signals (Type.simulation x))

            indexConversionMap = ModUt.indexConversionMap System.topology sfg
            statForcing = One.StateForcingOn
            balStepsIn = One.initialBattForceStep optParams

            res = balanceIteration optParams fsys accessf bfIn balStepsIn 
            sfg1 = Type.stateFlowGraphSweep (bResult lastElem)
            bfOut = bForcing lastElem
            lastElem = vlast "iterateEtaWhile: empty list" res



-------------------------------- OutPut Below -----------------------------------

concatZipMapsWith :: ((k0, a) -> (k1, b) -> [c]) -> Map k0 a -> Map k1 b -> [c]
concatZipMapsWith f s t = concat $ zipWith f (Map.toList s) (Map.toList t)

printfMap :: (Show k, Show a) => Map k a -> String
printfMap m = Map.foldWithKey f "" m
  where f k v acc = printf "%16s\t%16s\n" (show k) (show v) ++ acc

printfStateMap ::
  (PrintfArg t, PrintfArg t2) =>
  Map Idx.AbsoluteState (One.StateForcing t) ->
  Map t1 t2 -> [Char]
printfStateMap forceMap timeMap = concatZipMapsWith f forceMap timeMap
  where f (Idx.AbsoluteState k, One.StateForcing x) (_, y) = printf "S%2d%5.3f%5.3f" k x y

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
  iterateLoops optParams showEtaLoopItem showBalanceLoopItem (take maxCnt $ zip [0..] loop)
  where maxCnt = One.unMaxEtaIterations $ One.maxEtaIterations optParams

iterateLoops ::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.OptimisationParams node [] Sweep UV.Vector a -> 
      (Counter, EtaLoopItem node Sweep UV.Vector a z) -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a -> 
      (Counter, BalanceLoopItem node a z) -> o) ->
  [(Counter, EtaLoopItem node Sweep UV.Vector a z)]->
  [o]
iterateLoops optParams elf blf etaLoop =
  concatMap g $ take mei etaLoop
  where g x = elf optParams x
              : map (blf optParams) (zip [0..] (take mbi $ balanceLoop (snd x)))
        mei = One.unMaxEtaIterations $ One.maxEtaIterations optParams
        mbi = One.unMaxBalanceIterations $ One.maxBalanceIterations optParams


showEtaLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
  String
showEtaLoopItem _optParams (step, EtaLoopItem _sfgIn _sweep _sfgOut _) =
  printf "EL: %8d" step

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (Counter, BalanceLoopItem node a z) ->
  String
showBalanceLoopItem _optParams (bStp, BalanceLoopItem bForc _bFStep bal _) =
  printf " BL: %2d | " bStp ++ printfBalanceFMap bForc bal

printEtaLoop:: 
  (UV.Unbox a,Show (intVec Double),Show (simVec Double),
   Node.C node,SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   FormatValue.FormatValue a,
   z ~ Type.SignalBasedOptimisation
         node sweep vec Double intVec b simVec c efaVec d,
   Show a, Show node, PrintfArg a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] ->
  [IO ()]
printEtaLoop optParams ol =
  iterateLoops optParams printEtaLoopItem printBalanceLoopItem (take maxCnt $ zip [0..] ol)
  where maxCnt = One.unMaxEtaIterations $ One.maxEtaIterations optParams


printEtaLoopItem ::
  (UV.Unbox a,
   Node.C node,
   FormatValue.FormatValue a) =>
   One.OptimisationParams node [] Sweep UV.Vector a ->
   (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
   IO ()
printEtaLoopItem _params _e@(_step, EtaLoopItem _sfgIn _sweep _sfgOut _res) = 
  do
     putStrLn $ showEtaLoopItem _params _e
--    let -- dir = printf "outer-loop-%6.6d" olcnt
    --  stoPos = TopoIdx.Position System.Water System.Network
  --    gasPos = TopoIdx.Position System.Gas System.LocalNetwork
      --  _term = ModPlot.gpXTerm
      --  _balanceForcing =ilBForcOut $ vlast "printEtaLoopItem" res

--    ModPlot.sweepStackPerStateEta term params _sweep
--    ModPlot.sweepStackPerStateStoragePower term params System.Water _sweep
--    ModPlot.sweepStackPerStateOpt term params balanceForcing _sweep
--    ModPlot.sweepStackPerStateCondition term params  _sweep

{-
    concurrentlyMany_ [
      ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 0 _sweep,
      ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 1 _sweep,
      ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 2 _sweep,
      ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.1,0.1] 3 _sweep] -}

--    putStrLn (printf "Loop %6.6d" olcnt)

--    concurrentlyMany_ [
      --ModPlot.maxEtaPerState ModPlot.gpXTerm opt,
 --     ModPlot.simulationGraphs ModPlot.dotXTerm opt ]
      --ModPlot.expectedEtaPerState ModPlot.gpXTerm opt,
      --ModPlot.maxObjPerState ModPlot.gpXTerm opt ]

--    putStrLn $ showEtaLoopItem params e

--    concurrentlyMany_ [
      --putStrLn $ showEtaLoopItem params e]
--    ModPlot.drawSweepStateFlowGraph "sfgIn" 0 $ _sfgIn
--    ModPlot.drawSweepStateFlowGraph "sfgOut" 0 $ _sfgOut
      --Draw.xterm $ Draw.title "sfgIn" $ Draw.stateFlowGraph Draw.optionsDefault _sfgIn,
      --Draw.xterm $ Draw.title "sfgOut" $ Draw.stateFlowGraph Draw.optionsDefault _sfgOut]

      --ModPlot.simulationGraphs ModPlot.dotXTerm opt]
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
    putStrLn $ showBalanceLoopItem _optParams _b 
    concurrentlyMany_ [
--       ModPlot.maxIndexPerState _term _opt, 
--       ModPlot.givenSignals _term _opt, 
--       print (Map.map (Type.reqsAndDofsSignalsOfState) $ 
--              Type.interpolationPerState _opt),
--      print $ Type.signals $ Type.simulation $ _opt,
--      ModPlot.simulationSignals _term _opt
--      ModPlot.givenSignals _term _opt
       ]
--       ModPlot.maxEta _xTerm opt2 -}
     --ModPlot.optimalObjs term _opt
--       ModPlot.stateRange2 term _opt,


--     ModPlot.simulationSignals term opt2
--    ModPlot.simulationGraphs (ModPlot.dot dir bStep) opt2
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



checkRangeIO :: 
  One.SystemParams Node Double -> 
  One.OptimisationParams Node [] Sweep UV.Vector Double ->
  One.SimulationParams Node [] Double ->
  IO ()
checkRangeIO sysParams optParams simParams = do
  let sfg = One.stateFlowGraphOpt optParams
      indexConversionMap = ModUt.indexConversionMap System.topology sfg
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
            $ balanceIteration optParams fsys accessf initBalF initialBalSteps)

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
    ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.3,0.5] 0 swp
    ]


{-
eta ::
  (Node.C node, UV.Unbox a,
   Arith.Product a) =>
  Type.EnergyFlowAnalysis node vec a -> a
eta efa =
  case StateEta.etaSys (Type.stateFlowGraph efa) of
       Determined (Data e) -> e
       _ -> error "Main.iterateBalanceIO"
-}


{-
iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   z ~ Type.SignalBasedOptimisation Node Sweep UV.Vector a [] b0 [] c0 [] a,
   Arith.Constant a,RealFloat a, d ~ a) =>
  One.SystemParams Node a->
  One.OptimisationParams Node [] Sweep UV.Vector a->
  One.SimulationParams Node [] a->
  [EtaLoopItem Node Sweep UV.Vector a z]
iterateEtaWhile sysParams optParams simParams =
   go 0 (One.stateFlowGraphOpt optParams) initBalF
   where
         initBalF =  One.initialBattForcing optParams
         One.MaxEtaIterations maxCnt = One.maxEtaIterations optParams
         go cnt sfg bfIn =
           EtaLoopItem cnt sfg swp sfg1 res :
             if cnt > maxCnt-1 then [] else go (cnt+1) sfg1 bfOut
           where
            swp = trace "sweep" $ Base.perStateSweep sysParams optParams sfg
            fsys balanceForcing = NonIO.optimiseAndSimulateSignalBased sysParams optParams simParams 
                                  balanceForcing statForcing swp indexConversionMap
      
            accessf x = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ x

            indexConversionMap = ModUt.indexConversionMap System.topology sfg
            statForcing = One.StateForcingOn
            balStepsIn = One.initialBattForceStep optParams

            res = balanceIteration optParams fsys accessf bfIn balStepsIn 
            sfg1 = Type.stateFlowGraphSweep (bResult lastElem)
            bfOut = bForcing lastElem
            lastElem = vlast "iterateEtaWhile 2" res
-}


{-
getBalanceResult ::
  (Ord node,Arith.Sum a, Ord a, Arith.Constant a, Show a) =>
  [BalanceLoopItem node a (Type.OptimisationPerState node a) z] ->
  (Map node (One.SocDrive a),Map node (One.SocDrive a),
   Type.OptimalSolutionPerState node a)
getBalanceResult balLoop =
  (bForcing $ bestElem, bFStep bestElem, Type.optimalSolutionPerState $ fst $ bResult bestElem)
  where bestElem = if (length balLoop) == 1 
                   then lastElem
                   else choice
        lastElem =  vlast "interateBalanceUntil" balLoop            
        beforeElem = vlast "interateBalanceUntil" $ init $ balLoop
        choice = trace ("Choice " ++ show (balanceDeviation $ balance lastElem) ++ " " ++ 
                        show (balanceDeviation $ balance lastElem)) $ 
                        if (balanceDeviation $ balance lastElem) <= 
                    (balanceDeviation $ balance beforeElem)
                   then lastElem else beforeElem 
-}

{-
uniqueBalanceLoopX ::
  (Eq a, Eq node) => EtaLoop node a z -> EtaLoop node a z
uniqueBalanceLoopX (EtaLoop ol) = EtaLoop (map f ol)
  where f oli = oli { balanceLoop = BalanceLoop $ map (vlast "uniqueBalanceLoopX")
                                            $ List.groupBy g fzcs }
          where BalanceLoop fzcs = balanceLoop oli
        g a b = snd (result a) == snd (result b)
-}

{-
buildSupportHyperCube :: Ord d => [[[d]]] -> [[d]]
buildSupportHyperCube edgeList = Set.toList $ Set.fromList $ concat $ map g edgeList
 where g xs = foldl f [] xs
       f [] [x] = [[x]]
       f [] [x1, x2] = [[x1], [x2]]
       f acc [x] = map (++ [x]) acc
       f acc [x1, x2] = map (++ [x1]) acc ++ map (++ [x2]) acc
       f _ _ = error "buildSupportHyperCube - invalid amout of edge Points"
-}



{-
-- Check whether balance Step is already the smallest possible 
-- This is the case when only index or stateindex in the optimal Solution differs
-- Only active fields have to be probed 
checkBalanceStep :: 
  (Ord a,Arith.Constant a,Show a,
   SV.Storage vec ([[a]], [Sig.SignalIdx]),
   SV.FromList vec) =>
  One.SimulationParams node vec a -> 
  One.Balance node a -> One.Balance node a ->
  (a2, Type.OptimiseStateAndSimulate node2 sweep1 sweepVec1 a intVec1 b1 simVec1 c1 efaVec1 d1) ->
  (a1, Type.OptimiseStateAndSimulate node1 sweep sweepVec a intVec b simVec c efaVec d) ->
  Bool
checkBalanceStep simParams bal bal1 res res1 = trace ("Diffs: " ++ show numberOfDifferences) numberOfDifferences <= (2::Integer) && 
                                               (map (Arith.sign) $ Map.elems bal) /= (map (Arith.sign) $ Map.elems bal1)
  where optSolution = Type.optimalSolution $ snd res
        optSolution1 = Type.optimalSolution $ snd res1
        differenceList = zipWith cmpFunct indexList indexList1
        cmpFunct x y = case ((ModUt.frth5 x == ModUt.frth5 y) 
                       ,(ModUt.thd5 x == ModUt.thd5 y)) of 
                         (True, True) -> 0
                         (False,False) -> 2
                         (_,_) -> 1
        classList = buildSupportHyperCube $
          map fst $ Sig.toList $ One.activeSupportPoints simParams            
        lookupValues mp xs = map (\ k -> fromMaybe (error $ m2 k) $ fromMaybe (error $ m k) $ 
                                         Map.lookup k mp) xs
        indexList = lookupValues optSolution classList
        indexList1 = lookupValues optSolution1 classList
        m k = ("Error in checkBalanceStep - support Point doesn't exist" ++ show k)
        m2 k = ("Cycle touches Invalid Area " ++ show k)
        numberOfDifferences = (foldl (+) (0) differenceList)
-}


