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
import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~/))
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Terminal as Terminal

--import EFA.Equation.Result (Result(Determined))

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.State.SystemEta as StateEta
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State as FlowState
--import qualified EFA.Report.FormatValue as FormatValue

-- import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(Data)) --, Nil, Apply)

import EFA.Utility.List (vlast,vhead) 
import EFA.Utility.Async (concurrentlyMany_)

--import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
--import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
--import Data.Bimap (Bimap)

import Text.Printf (printf, PrintfArg) --,IsChar)

data BalanceLoopItem node a z =
  BalanceLoopItem { bStep :: Int,
                    bForcing :: Map.Map node (One.SocDrive a),
                    bFStep ::  Map.Map node (One.SocDrive a),
                    balance :: One.Balance node a,
                    bResult :: z}

data EtaLoopItem node sweep vec a z = EtaLoopItem {
  elStep :: Int,
  stateFlowIn :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  sweep :: Type.Sweep node sweep vec a,
  stateFlowOut :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  balanceLoop :: [BalanceLoopItem node a z]}

{-
uniqueBalanceLoopX ::
  (Eq a, Eq node) => EtaLoop node a z -> EtaLoop node a z
uniqueBalanceLoopX (EtaLoop ol) = EtaLoop (map f ol)
  where f oli = oli { balanceLoop = BalanceLoop $ map (vlast "uniqueBalanceLoopX")
                                            $ List.groupBy g fzcs }
          where BalanceLoop fzcs = balanceLoop oli
        g a b = snd (result a) == snd (result b)
-}

eta ::
  (Node.C node, UV.Unbox a,
   Arith.Product a) =>
  Type.EnergyFlowAnalysis node vec a -> a
eta efa =
  case StateEta.etaSys (Type.stateFlowGraph efa) of
       Determined (Data e) -> e
       _ -> error "Main.iterateBalanceIO"

checkBalance :: (Ord a, Arith.Sum a) =>
                One.OptimisationParams node list sweep vec a ->
                One.Balance node a ->  Bool
checkBalance optParams bal = all g $ Map.elems bal
  where g x = (Arith.abs x) <= eps
        One.BalanceThreshold eps =  One.balanceThreshold optParams
        
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
buildSupportHyperCube :: Ord d => [[[d]]] -> [[d]]
buildSupportHyperCube edgeList = Set.toList $ Set.fromList $ concat $ map g edgeList
 where 
   g xs = foldl f [] xs
   f [] [x] = [[x]]
   f [] [x1,x2] = [[x1],[x2]]
   f acc [x] = map (++[x]) acc
   f acc [x1,x2] = map (++[x1]) acc ++ map (++[x2]) acc
   f _ _ = error "buildSupportHyperCube - invalid amout of edge Points"

-- | Rate Balance Deviation by sum of Standard Deviation and Overall Sum 
balanceDeviation :: 
  (Arith.Product a,
   Ord a,
   Arith.Constant a,
   Arith.Sum a) =>
  One.Balance node a -> a
balanceDeviation m = (Map.foldl (\ acc x -> acc Arith.~+ (x Arith.~* x)) (Arith.zero) m) Arith.~+ 
                     (Arith.abs  (Map.foldl (\ acc x -> acc Arith.~+ x) (Arith.zero) m)) 
{-
getBalanceResult ::
  (Ord node,Arith.Sum a, Ord a, Arith.Constant a, Show a) =>
  [BalanceLoopItem node a (Type.OptimisationPerState node a) z] ->
  (Map.Map node (One.SocDrive a),Map.Map node (One.SocDrive a),
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

balanceIteration::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.BalanceForcing node a -> z)->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  [BalanceLoopItem node a z]
balanceIteration optParams fsys accessf balForceIn balStepsIn = 
  go 0 balForceIn balStepsIn 
  where
    go cnt forcing stepping = oneIterationOfAllStorages ++ 
         if checkBalance optParams bal then [] else go cnt1 forcing1 stepping1
      where                              
           oneIterationOfAllStorages = foldl (\acc sto -> acc ++ iterateOneStorage cnt 
                                                          fsys accessf forcing stepping sto) 
                                       [] $ Map.keys forcing
                                       
           bal = balance $ lastElem
           forcing1 = bForcing $ lastElem
           stepping1 = bFStep $ lastElem
--           res1 = bResult $ lastElem
           cnt1 = bStep $ lastElem
           lastElem = vlast "iterateBalance" $ oneIterationOfAllStorages

iterateOneStorage ::  
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  Int -> 
  (One.BalanceForcing node a -> z)->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  node ->
  [BalanceLoopItem node a z]
iterateOneStorage cntIn fsys accessf forcingIn steppingIn sto = 
  go cntIn forcingIn steppingIn initialResult (Nothing,Nothing)
  where
    initialResult = fsys forcingIn
    go cnt force step res bestPair = BalanceLoopItem cnt force step1 bal res : 
                                     if False -- True --checkBalanceSingle 
                                     then [] 
                                       else go (cnt+1) force1 step1 res1 bestPair1
      where
        force1 = One.addForcingStep force step sto
        res1 = fsys force1
        bal1 = accessf res1 
        bal = accessf res 
        bestPair1 = One.rememberBestBalanceForcing bestPair 
                    (force1, bal1) sto
        step1 = calculateNextBalanceStep 
                (force1, bal1) bestPair1 
                step sto

calculateNextBalanceStep :: 
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, Show a,
   Ord node, Show node) =>
  (One.BalanceForcing node a,One.Balance node a) -> 
  (Maybe (One.SocDrive a,a), Maybe (One.SocDrive a,a)) -> 
  (One.BalanceForcingStep node a)->
  node ->
  (One.BalanceForcingStep node a)
calculateNextBalanceStep (_,balMap) bestPair stepMap sto = One.updateForcingStep stepMap sto $ One.setSocDrive step1
 where
   bal = One.getStorageBalance "calculateNextBalanceStep" balMap sto
   step = One.getStorageForcingStep "calculateNextBalanceStep" stepMap sto
   fact = Arith.fromRational 2.0
   divi = Arith.fromRational 1.7
   intervall = One.getForcingIntervall bestPair
   g _ x =  x --trace (str ++": "++ show x) x
   step1 = case (intervall, Arith.sign bal) of   
                    -- Zero Crossing didn't occur so far -- increase step to search faster
                    (Nothing,Negative) -> g "A" $ Arith.abs $ (One.getSocDrive step) ~* fact
                    (Nothing,Positive) ->  g "B" $ Arith.negate $ (Arith.abs $ One.getSocDrive step) ~* fact
                    -- The Zero Crossing is contained in the intervall
                    -- defined by bestPair - step just a little over the middle
                    (Just int, Negative) ->  g "C" $ (One.getSocDrive int) ~/ divi
                    (Just int, Positive) ->   g "D" $ (Arith.negate $ One.getSocDrive  int) ~/ divi
                    (_, Zero)  ->  g "E" $ Arith.zero   

-- | TODO : move to correct Position 
getStateTimes ::
  Arith.Constant a =>
  Map.Map Idx.AbsoluteState a1 ->
  StateQty.Graph Node storageLabel (Result a) ->
  Map.Map Idx.AbsoluteState (Result a)
getStateTimes stateForceIn sfg = Map.mapWithKey f stateForceIn
  where timeMap = Map.map FlowTopo.label $ StateQty.states sfg
        indexMap = Bimap.toMap $ ModUt.indexConversionMap System.topology $ sfg
        f absIdx _ = fromMaybe (Determined Arith.zero) (Map.lookup absIdx timeMapAbs)
        timeMapAbs = Map.mapKeys g timeMap
           where g st = indexMap Map.! st

-- | Find a solution where all states occur in the simlation signals at minimum state forcing,
-- | measurement unit ist state duration
getStateTime ::
  Idx.State ->
  FlowState.Graph node edge sectionLabel nodeLabel
    storageLabel edgeLabel carryLabel ->
  sectionLabel
getStateTime stateIdx sfg = FlowTopo.label $ f state
  where state = Map.lookup stateIdx (StateQty.states sfg)
        f (Just st) = st
        f (Nothing) = error ("Error in getStateTime: State " ++ show stateIdx ++
                             "doesn't exist in StateflowGraph")

data StateForceDemand = MoreForcingNeeded | CorrectForcing |
                        NoForcingNeeded | LessForcingNeeded deriving Show

checkStateTimes :: (Num a, Ord a) => One.OptimisationParams node [] Sweep vec a ->
               One.StateDurations a ->
               Map.Map Idx.AbsoluteState (One.StateForcingStep a) ->
               Bool
checkStateTimes optParams stateDurs stateSteps =
  all g $ zip (Map.elems stateDurs) (Map.elems stateSteps)
  where g (time,step) = (time > lThr)  && ( time <= uThr || step == One.DontForceState )
        (One.StateTimeThreshold uThr) = One.stateTimeUpperThreshold optParams
        (One.StateTimeThreshold lThr) = One.stateTimeLowerThreshold optParams

iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,z
                      ~ Type.SignalBasedOptimisation
                          Node Sweep UV.Vector a [] b0 [] c0 [] a,
    Arith.Constant a,RealFloat a,d ~ a) =>
  One.SystemParams Node a->
  One.OptimisationParams Node [] Sweep UV.Vector a->
  One.SimulationParams Node [] a->
  [EtaLoopItem Node Sweep UV.Vector a z]
iterateEtaWhile sysParams optParams simParams = go 0 (One.stateFlowGraphOpt optParams) initBalF
   where
         initBalF =  One.initialBattForcing optParams
         One.MaxEtaIterations maxCnt = One.maxEtaIterations optParams
         go cnt sfg bfIn = EtaLoopItem cnt sfg swp sfg1 res :
                           if cnt > (maxCnt-1) then []
                                 else go (cnt+1) sfg1 bfOut
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
            sfg1 = Type.stateFlowGraphSweep
                   $ bResult lastElem
            bfOut = bForcing lastElem
            lastElem = vlast "iterateEtaWhile 2" res       



-------------------------------- OutPut Below -----------------------------------

printfMap :: (Show k, Show a) => Map.Map k a -> String
printfMap m = concatMap f $ Map.toList m
  where f (k, x) =
          printf "%16s\t%16s\n" (show k) (show x)

printfStateMap ::
  (PrintfArg t, PrintfArg t2) =>
  Map.Map Idx.AbsoluteState (One.StateForcing t) ->
  Map.Map t1 t2 -> [Char]
printfStateMap forceMap timeMap = concat$ zipWith f (Map.toList forceMap) (Map.toList timeMap)
  where f (Idx.AbsoluteState k, One.StateForcing x) (_, y) =
          "S" ++ (printf "%2d" k) ++ " (" ++ (printf "%5.3f" x) ++ "," ++ (printf "%5.3f" y) ++ ") "

printfBalanceFMap ::
  (Show a, PrintfArg a1, PrintfArg t1, Arith.Constant a1) =>
  Map.Map a (One.SocDrive a1) -> Map.Map t t1 -> [Char]
printfBalanceFMap forceMap balanceMap = concat$ zipWith f (Map.toList forceMap) (Map.toList balanceMap)
  where f (k,x) (_, y) =
          "   | Sto: " ++ (printf "%7s" $ show k) ++ "   | F: " ++ (printf "%10.5f" $ One.getSocDrive x)
          ++ "   | B: " ++ (printf "%10.5f" y)

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map.Map a t -> [Char]
printfBalanceMap balanceMap = concat $ map f (Map.toList balanceMap)
  where f (k,x) =
          " Sto: " ++ (printf "%7s" $ show k) ++ " B: " ++ (printf "%5.3f" x)
          
showEtaLoop ::
  (Show node, Show a, PrintfArg a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] -> [String]
showEtaLoop optParams loop = iterateLoops optParams showEtaLoopItem showBalanceLoopItem loop


iterateLoops ::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z -> o) ->
  [EtaLoopItem node Sweep UV.Vector a z]->
  [o]
iterateLoops optParams elf blf etaLoop =
  concat $ map g $ take n1 etaLoop
  where g x = [elf optParams x] ++ (concat $ take n2 $ map h $ balanceLoop x)
        h x = [blf optParams x]
        (One.MaxEtaIterations n1) = One.maxEtaIterations optParams
        (One.MaxBalanceIterations n2) = One.maxBalanceIterations optParams


showEtaLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z -> String
showEtaLoopItem _optParams (EtaLoopItem step _sfgIn _sweep _sfgOut _) =
      "EL: " ++ printf "%8d" step

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z ->
  String
showBalanceLoopItem _optParams (BalanceLoopItem bStp bForc _bFStep bal _) =
        " BL: " ++ printf "%2d | " bStp ++
         printfBalanceFMap bForc bal

printEtaLoop:: 
  One.OptimisationParams node [] Sweep UV.Vector a
  -> [EtaLoopItem node Sweep UV.Vector a z] -> [IO ()]
printEtaLoop optParams ol =
  iterateLoops (optParams) printEtaLoopItem  printBalanceLoopItem  ol


printEtaLoopItem :: 
   t -> EtaLoopItem t1 t2 t3 t4 t5 -> IO ()
printEtaLoopItem _params _e@(EtaLoopItem _step _sfgIn _sweep _sfgOut _res) = print "EtaLoop"
  --do
  --  let -- dir = printf "outer-loop-%6.6d" olcnt
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

--    concurrentlyMany_ [putStrLn $ showEtaLoopItem params e]
--      ModPlot.drawSweepStateFlowGraph "sfgIn" 0 $ _sfgIn,
--      ModPlot.drawSweepStateFlowGraph "sfgOut" 0 $ _sfgOut]
--      Draw.xterm $ Draw.title "sfgIn" $ Draw.stateFlowGraph Draw.optionsDefault _sfgIn,
--      Draw.xterm $ Draw.title "sfgOut" $ Draw.stateFlowGraph Draw.optionsDefault _sfgOut]

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
  t -> BalanceLoopItem t1 t2 t3 -> IO ()
printBalanceLoopItem _optParams _b@(BalanceLoopItem _bStp _bForcing _bFStep _bal _res) = 
  print "BalanceLoopItem"
  --print "BalanceLoop"
--  do
 --    let _opt = fst res
   --      opt2 = snd res
    --     _gTerm = ModPlot.gpPNG _dir bStp
    --     _xTerm = ModPlot.gpXTerm
{-         term = _xTerm
         _dir = printf "outer-loop-%6.6d" bStp
         _stoPos = TopoIdx.Position System.Water System.Network

     putStrLn $ showBalanceLoopItem optParams b
     concurrentlyMany_ [ -}
{-     ModPlot.maxIndexPerState term _opt
     ModPlot.maxEta _xTerm opt2 -}
     --ModPlot.optimalObjs term _opt
--       ModPlot.stateRange2 term _opt,

--     putStrLn $ show $ Type.signals $ Type.simulation $ opt2
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
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir bStep) opt-}



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
      fsys balanceForcing = NonIO.optimiseAndSimulateSignalBased sysParams optParams simParams 
                                  balanceForcing statForcing swp indexConversionMap
      
      accessf x = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ x
      
      b@(BalanceLoopItem _bStp _bForcing _bFStep _bal opt) = vhead "checkRangeIO" $ 
                                                             balanceIteration optParams fsys accessf initBalF initialBalSteps 
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




        