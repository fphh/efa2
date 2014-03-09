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

import EFA.Equation.Result(Result(Determined,Undetermined))
import qualified EFA.Application.Type as Type
import EFA.Application.Type (EnvResult)
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

--import qualified EFA.Graph as Graph
--import EFA.Graph (Graph)
--import qualified EFA.Flow.Draw as Draw

--import qualified EFA.Graph.Topology as Topology
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~+), (~/))
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
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
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

data BalanceLoopItem node a z0 z =
  BalanceLoopItem { bStep :: Int,
                    bForcing :: Map.Map node (One.SocDrive a),
                    bFStep ::  Map.Map node (One.SocDrive a),
                    balance :: One.Balance node a,
                    bResult :: (z0,z)}

data BalanceLoopItemSig node a z =
  BalanceLoopItemSig { bStepSig :: Int,
                    bForcingSig :: Map.Map node (One.SocDrive a),
                    bFStepSig ::  Map.Map node (One.SocDrive a),
                    balanceSig :: One.Balance node a,
                    bResultSig :: z}

data StateLoopItem node a z = StateLoopItem
                         { sStep ::Int,
                           sForcing  :: Map.Map Idx.AbsoluteState (One.StateForcing a),
                           sFStep :: Map.Map Idx.AbsoluteState (One.StateForcingStep a),
                           stateDurations :: One.StateDurations a,
                           sBalance :: One.Balance node a,
                           sResult :: z }

data InnerLoopItem node a z0 z = InnerLoopItem
                         { ilStep :: Int,
                           ilBForcOut :: Map.Map node (One.SocDrive a),
                           ilBalance :: One.Balance node a,
                           ilSForcOut :: Map.Map Idx.AbsoluteState (One.StateForcing a),
                           ilSDurations :: One.StateDurations a,
                           balanceLoop :: [BalanceLoopItem node a z0 z],
                           stateLoop :: [StateLoopItem node a z]}


data EtaLoopItem node sweep vec a z0 z = EtaLoopItem {
  elStep :: Int,
  stateFlowIn :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  sweep :: Type.Sweep node sweep vec a,
  stateFlowOut :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  innerLoop :: [InnerLoopItem node a z0 z]}


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

balanceIteration::
  (efaVec~[], intVec ~ [], sweep ~ Sweep, a ~ d, simVec ~ [],
   Ord d,
   RealFloat d,
   Show d,
   Show node,
   UV.Unbox d,
   Node.C node,
   Arith.ZeroTestable d,
   Arith.Constant d
   ) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node simVec a ->
  Map.Map Idx.State (Map.Map [a] (Type.SweepPerReq node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  One.IndexConversionMap ->
  [BalanceLoopItem node a (Type.OptimisationPerState node a)
    (Type.OptimiseStateAndSimulate node sweep UV.Vector a intVec b simVec c efaVec d)]
balanceIteration sysParams optParams simParams perStateSweep balForceIn balStepsIn statForceIn indexConversionMap =
  go 0 balForceIn balStepsIn resStart
  where
    One.MaxBalanceIterations maxStepCnt = One.maxBalanceIterations optParams
    seed = One.balanceForcingSeed optParams
    resStart = fsys balForceIn
    fsys bf = NonIO.optimiseAndSimulate sysParams optParams simParams bf statForceIn
              perStateSweep indexConversionMap

    accessf res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ snd res
                      
    -- TODO: Hier fehlt eine Schleife pro Speicher !!
    go cnt force step res  = BalanceLoopItem cnt force step  bal res :
                                 if checkBalance optParams bal || cnt >= maxStepCnt || checkBalanceStep simParams bal bal1 res res1 
                                 then [BalanceLoopItem (cnt+1) force1 step1  bal1 res1]
                                 else go (cnt+1) force1 step1 res1

      where force1 = Map.mapWithKey (\k x -> One.setSocDrive $ (One.getSocDrive x) ~+
                                           (One.getSocDrive $ g $ Map.lookup k step)) force
            g (Just x) = x
            g Nothing = error ("Error in balanceIteration: keys in StorageForcingMap and StepMap differ")

            bal = accessf res
            res1 = fsys $ force1
            bal1 = accessf res1
            
            step1 = Map.mapWithKey (\ k s -> One.setSocDrive $ f  k s) step

            f k stp = let
              y0 = j $ Map.lookup k $ bal
              y1 = j $ Map.lookup k $ bal1
              st = One.getSocDrive stp

              j (Just x) = x
              j Nothing =  error ("Error in balanceIteration: Storage not found")

              _1 = Arith.fromRational 1.1
              _2 = Arith.fromInteger 2

              newStepSize = (Arith.abs st) ~/(Arith.one ~+
                        (Arith.abs $ y0) ~/ (Arith.abs $ y1))

              in if True then case (Arith.sign $ y0, Arith.sign $ y1) of
                              (Negative, Negative) -> ((Arith.abs st) ~* _1)
                              (Positive, Positive) -> (Arith.negate $ (Arith.abs st) ~* _1)
                              (Negative, Positive) -> (Arith.negate $ (Arith.abs st) ~* 0.7)
                              (Positive, Negative) -> ((Arith.abs st) ~* 0.7)
                              (Zero, Positive)  -> Arith.negate $ One.getSocDrive seed
                              (Zero, Negative)  -> One.getSocDrive seed
                              (_, Zero)  -> Arith.zero

                else -- One.setSocDrive -- VARIANT B: Estimating zero crossing position

                  case (Arith.sign $ y0, Arith.sign $ y1) of
                    -- Crossing not found increase step
                    (Negative, Negative) -> ((Arith.abs st) ~* _2)
                    (Positive, Positive) ->  (Arith.negate $ (Arith.abs st) ~* _2)
                    -- Zero crossing occured, step into the middle
                    (Negative, Positive) ->  (Arith.negate newStepSize)
                    (Positive, Negative) ->  newStepSize
                    (Zero, Positive)  ->  Arith.negate $ One.getSocDrive seed
                    (Zero, Negative)  ->  One.getSocDrive seed
                    (_, Zero)  ->  Arith.zero

{-
balanceIteration2 :: 
  (efaVec~[], intVec ~ [], sweep ~ Sweep, a ~ d, simVec ~ [],
   Ord d,RealFloat d,Show d,Show node,
   UV.Unbox d,Node.C node,Arith.ZeroTestable d,Arith.Constant d) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node simVec a ->
  Map.Map Idx.State (Map.Map [a] (Type.SweepPerReq node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map node (One.SocDrive a) ->
  One.StatForcing ->
  One.IndexConversionMap ->
  [BalanceLoopItemSig node a 
   (Type.SignalBasedOptimisation
                          node Sweep UV.Vector a intVec b simVec c efaVec d)] -}
balanceIteration2::  
  (Ord a, Arith.Constant a,Ord node, Show node) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node simVec a ->
  (One.BalanceForcing node a -> z)->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  One.StatForcing ->
--  z -> 
  [BalanceLoopItemSig node a z]
balanceIteration2 sysParams optParams simParams fsys accessf balForceIn balStepsIn statForcing = 
  go 0 balForceIn balStepsIn resStart
  where
    --One.MaxBalanceIterations maxStepCnt = One.maxBalanceIterations optParams
    --seed = One.balanceForcingSeed optParams
    resStart = fsys balForceIn
{-    fsys bf = NonIO.optimiseAndSimulateSignalBased sysParams optParams simParams bf statForcing
              perStateSweep indexConversionMap

    accessf res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulationSig $ res  -}
  
    go cnt forcing stepping res = oneIterationOfAllStorages ++ 
         if checkBalance optParams balance then [] else go cnt forcing1 stepping1 res1
      where                              
           oneIterationOfAllStorages = foldl (\acc sto -> acc ++ iterateOneStorage cnt 
                                                          fsys accessf forcing stepping res sto) 
                                       [] $ Map.keys forcing
                                       
           balance = balanceSig $ lastElem
           forcing1 = bForcingSig $ lastElem
           stepping1 = bFStepSig $ lastElem
           res1 = bResultSig $ lastElem
           cnt1 = bStepSig $ lastElem
           lastElem = vlast "iterateBalance" $ oneIterationOfAllStorages

iterateOneStorage ::  
  (Ord a, Arith.Constant a,Ord node, Show node) =>
  Int -> 
  (One.BalanceForcing node a -> z)->
  (z -> One.Balance node a)->
  One.BalanceForcing node a ->
  One.BalanceForcingStep node a ->
  z -> 
  node ->
  [BalanceLoopItemSig node a z]
iterateOneStorage cntIn fsys accessf forcingIn steppingIn res sto = 
  go cntIn forcingIn steppingIn initialResult (Nothing,Nothing)
  where
    initialResult = fsys forcingIn
    go cnt force step res bestPair = BalanceLoopItemSig cnt force step1 bal res : 
                                     if True --checkBalanceSingle 
                                     then [BalanceLoopItemSig (cnt+1) force1 step  bal1 res1]
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
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, 
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
   step1 = case (intervall, Arith.sign bal) of   
                    -- Zero Crossing didn't occur so far -- increase step to search faster
                    (Nothing,Negative) -> Arith.abs $ (One.getSocDrive step) ~* fact
                    (Nothing,Positive) ->  Arith.negate $ (Arith.abs $ One.getSocDrive step) ~* fact
                    -- The Zero Crossing is contained in the intervall
                    -- defined by bestPair - step just a little over the middle
                    (Just int, Negative) ->  (One.getSocDrive int) ~/ divi
                    (Just int, Positive) ->   (Arith.negate $ One.getSocDrive int) ~/ divi
                    (_, Zero)  ->  Arith.zero   

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

getStateResult ::
  [StateLoopItem node a z] ->
  (Map.Map Idx.AbsoluteState (One.StateForcing a),
      Map.Map Idx.AbsoluteState (One.StateForcingStep a),
      One.StateDurations a,One.Balance node a)
getStateResult statLoop =
  (sForcing $ lastElem , sFStep lastElem, stateDurations $ lastElem, sBalance $ lastElem)
  where lastElem = vlast "interateStateUntil" $ statLoop

stateIteration ::
  (sigVec ~[],d ~ b,efaVec ~ [],simVec ~ [], intVec ~ [], (a ~ b),UV.Unbox b,node~Node,RealFloat b,
   Eq a, Num a,
   Arith.Sum a,
   Ord a,
   Show node,
   Show a,
   Node.C node,
   Arith.ZeroTestable a,
   Arith.Constant a) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node sigVec a ->
  Type.OptimalSolutionPerState node a ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  Maybe (Map.Map Idx.AbsoluteState (One.StateForcingStep a)) ->
  One.IndexConversionMap ->
  [StateLoopItem node a
   (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)]
stateIteration sysParams optParams simParams optimalObjectivePerState stateForceIn staStepsIn indexConversionMap =
  go 0 stateForceIn initialSteps initialResults
  where initialSteps = j staStepsIn

        j (Just st) = st
--        j (Nothing) = Map.map (\x -> if x==0 then seed else One.DontForceState) initialTimes
        j (Nothing) = Map.map (\x -> if x==0 then One.StateForcingStep Arith.zero  else One.DontForceState) initialTimes

        --seed = One.stateForcingSeed optParams
        fsys sf = NonIO.optimiseStateAndSimulate sysParams optParams simParams sf optimalObjectivePerState indexConversionMap
        initialResults = fsys stateForceIn
        initialTimes = accessTimes initialResults
        One.MaxStateIterations maxCnt =  One.maxStateIterations optParams

        accessTimes res =
          Map.map f $ getStateTimes stateForceIn $ Type.stateFlowGraph $ Type.analysis res
           where f (Determined (Data x)) = x
                 f Undetermined  = error "State Time undetermined"

        accessBal res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ res


        go cnt force step res  =
          StateLoopItem cnt force step times bal res :
               if checkStateTimes optParams times step || cnt >= maxCnt
               then [] else  go (cnt+1) force1 step1 res1

          where
            force1 = Map.fromList $ zipWith g (Map.toList force) (Map.toList step)

            g (idx1,One.StateForcing x) (idx2,One.DontForceState) =
              if idx1 /= idx2 then error msg else (idx1, One.StateForcing $ x)
            g (idx1,One.StateForcing x) (idx2,One.StateForcingStep y) =
              if idx1 /= idx2 then error msg else (idx1, One.StateForcing $ x~+y)

            res1 = fsys force1
            times = accessTimes res
            times1 = accessTimes res1
            bal = accessBal res

            step1 = Map.fromList $ zipWith3 (changeStateForce optParams)
                       (zip (Map.elems times)( Map.elems times1))
                       (zip (Map.elems force) (Map.elems force1))
                       (Map.toList step)

            msg = "Error in StateIteration"
                  ++ " -- differing States between Forcing,"
                  ++ " Stateduration and Step"



changeStateForce::
  (Eq a1, Eq a,a~a3,a~a2,
   Num a1, Num a,
   Num a2, Ord a3,
   Ord a2,Arith.Constant a3, a1 ~ a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (a2, a2)->
  (One.StateForcing a, One.StateForcing a1)->
  (t, One.StateForcingStep a3)->
  (t, One.StateForcingStep a3)
changeStateForce _ (_,_) (_,_) (idx,One.DontForceState) = (idx,One.DontForceState)
changeStateForce optParams (y0,y1) (One.StateForcing x0,One.StateForcing x1) (idx,One.StateForcingStep st) = (idx,st1)
  where
     seed = One.stateForcingSeed optParams
     One.StateTimeThreshold uthr = One.stateTimeUpperThreshold optParams
     One.StateTimeThreshold lthr = One.stateTimeLowerThreshold optParams
     g _str = id --trace str
     st1 =
       let
         _3 = Arith.fromInteger 3
         _2 = Arith.fromInteger 2
         eval force timeDur =
           case (force == 0, timeDur <= lthr, timeDur < uthr ) of
                (True, False, _)      -> NoForcingNeeded
                (False, False, True)  -> CorrectForcing
                (_, True, _)      -> MoreForcingNeeded
                (False, False, False) -> LessForcingNeeded

       in case (eval x0 y0, eval x1 y1) of
            (_, NoForcingNeeded) -> g "a1" $  One.StateForcingStep $ Arith.zero
            (_, CorrectForcing) -> g "a2" $ One.StateForcingStep $ Arith.zero
            (NoForcingNeeded, _) -> g "a3" $ seed
            (CorrectForcing, MoreForcingNeeded) -> g "a4" $ seed
            (CorrectForcing, LessForcingNeeded) -> g "a5" $ fmap Arith.negate seed
            (MoreForcingNeeded, MoreForcingNeeded) -> g "a6" $ if st == Arith.zero then seed else One.StateForcingStep $ (Arith.abs st) ~* _2
            (LessForcingNeeded, LessForcingNeeded) -> g "a7" $ One.StateForcingStep $ Arith.negate $ (Arith.abs st) ~* _2
            (MoreForcingNeeded, LessForcingNeeded) -> g "a8" $ One.StateForcingStep $ Arith.negate $ (Arith.abs st) ~/ _3
            (LessForcingNeeded, MoreForcingNeeded) -> g "a9" $ if st == Arith.zero then seed else One.StateForcingStep $ (Arith.abs st) ~/ _3
{-

    fsys bf = NonIO.optimiseAndSimulateSignalBased sysParams optParams simParams bf statForcing
              perStateSweep indexConversionMap

    accessf res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulationSig $ res  



iterateInnerLoop ::
  (intVec~[], efaVec ~ [], simVec ~ [],a ~ d,sigVec ~ [],d ~ b,node~Node,
   Ord d,RealFloat b,
   Show d,
   Show node,
   UV.Unbox d,
   Num b,
   Node.C node,
   Arith.ZeroTestable d,
   Arith.Constant d) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node sigVec a ->
  Map.Map Idx.State (Map.Map [a] (Type.SweepPerReq node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  One.IndexConversionMap ->
  (Map.Map node (One.SocDrive a),
  [InnerLoopItem node a (Type.OptimisationPerState node a)
     (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)])
iterateInnerLoop sysParams optParams simParams perStateSweep balForceIn stateForceIn indexConversionMap = (balForceOut,innerLp)
  where
    innerLp = go 0 balForceIn initialBalSteps stateForceIn initialStateSteps
    balForceOut = ilBForcOut $ vlast "iterateInnerLoop" innerLp
    initialStateSteps = Nothing
    initialBalSteps = One.initialBattForceStep optParams
    One.MaxInnerLoopIterations maxCnt = One.maxInnerLoopIterations optParams

    go cnt balForce balSteps statForce statSteps =
      InnerLoopItem cnt bForceOut bal statForceOut sta balLoop statLoop :
         if (checkBalance optParams bal && checkStateTimes optParams sta staStepsOut) || cnt > (maxCnt-1) then []
         else go (cnt+1) bForceOut balStepsOut statForceOut (Just staStepsOut)
      where
{-
        balLoop = balanceIteration sysParams optParams simParams perStateSweep balForce balSteps statForce indexConversionMap
        (bForceOut, balStepsOut, optimalObjectivePerState) = getBalanceResult balLoop

        statLoop = stateIteration sysParams optParams simParams optimalObjectivePerState statForce statSteps indexConversionMap
        (statForceOut,staStepsOut, sta,bal) = getStateResult statLoop
-}
 
        balLoop = balanceIteration2 sysParams optParams simParams perStateSweep balForce balSteps statForce indexConversionMap
        (bForceOut, balStepsOut, optimalObjectivePerState) = getBalanceResult balLoop
       
iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   Arith.Constant a,RealFloat a) =>
  One.SystemParams Node a->
  One.OptimisationParams Node [] Sweep UV.Vector a->
  One.SimulationParams Node [] a->
  [EtaLoopItem Node Sweep UV.Vector a (Type.OptimisationPerState Node a)
   (Type.OptimiseStateAndSimulate Node Sweep UV.Vector a [] a [] c [] a)]
iterateEtaWhile sysParams optParams simParams = go 0  (One.stateFlowGraphOpt optParams) initBalF
   where
         initBalF =  One.initialBattForcing optParams
         One.MaxEtaIterations maxCnt = One.maxEtaIterations optParams
         go cnt sfg bfIn = EtaLoopItem cnt sfg swp sfg1 res :
                           if cnt > (maxCnt-1) then []
                                 else go (cnt+1) sfg1 bfOut
           where
            swp = trace "sweep" $ Base.perStateSweep sysParams optParams sfg
            indexConversionMap = ModUt.indexConversionMap System.topology sfg
            statf = Map.map (const $ One.StateForcing Arith.zero) (Bimap.toMapR indexConversionMap)
            (bfOut, res) = trace "traceInnerLoop" $ iterateInnerLoop sysParams optParams simParams swp bfIn statf indexConversionMap
            sfg1 = Type.stateFlowGraphSweep
                   $ sResult
                   $ vlast "iterateEtaWhile 2"
                   $ stateLoop
                   $ vlast "iterateEtaWhile 1" res

-}

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
          "   | Sto: " ++ (printf "%7s" $ show k) ++ "   | F: " ++ (printf "%5.3f" $ One.getSocDrive x)
          ++ "   | B: " ++ (printf "%5.3f" y)

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map.Map a t -> [Char]
printfBalanceMap balanceMap = concat $ map f (Map.toList balanceMap)
  where f (k,x) =
          " Sto: " ++ (printf "%7s" $ show k) ++ " B: " ++ (printf "%5.3f" x)
showEtaLoop ::
  (Show node, Show a, PrintfArg a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z0 z] -> [String]
showEtaLoop optParams loop = iterateLoops optParams showEtaLoopItem showInnerLoopItem
              showBalanceLoopItem showStateLoopItem loop


iterateLoops ::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  InnerLoopItem node a z0 z -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z0 z -> o) ->
  (One.OptimisationParams node [] Sweep UV.Vector a ->
  StateLoopItem node a z -> o) ->
  [EtaLoopItem node Sweep UV.Vector a z0 z]->
  [o]
iterateLoops optParams elf ilf blf slf etaLoop =
  concat $ map g $ take n1 etaLoop
  where g x = [elf optParams x] ++ (concat $ take n2 $ map h $ innerLoop x)
        h x = [ilf optParams x] ++ (take n3 $ map k $ balanceLoop x) ++ (take n4 $ map j $ stateLoop x)
        k x = blf optParams x
        j x = slf optParams x
        (One.MaxEtaIterations n1) = One.maxEtaIterations optParams
        (One.MaxInnerLoopIterations n2) = One.maxInnerLoopIterations optParams
        (One.MaxBalanceIterations n3) = One.maxBalanceIterations optParams
        (One.MaxStateIterations n4) = One.maxStateIterations optParams

showEtaLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> String
showEtaLoopItem _optParams (EtaLoopItem step _sfgIn _sweep _sfgOut _) =
      "EL: " ++ printf "%8d" step

showInnerLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  InnerLoopItem node a z0 z ->
  String
showInnerLoopItem _optParams (InnerLoopItem ilStp _ilBForcO _ilBal _ilSForcO
   _ilSDur _balLoop _ ) = "IL: " ++ printf "%8d" ilStp

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z0 z ->
  String
showBalanceLoopItem _optParams (BalanceLoopItem bStp bForc _bFStep bal _) =
        " BL: " ++ printf "%2d | " bStp ++
         printfBalanceFMap bForc bal

showStateLoopItem :: (Show a, Show node,PrintfArg a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  StateLoopItem node a z ->
  String
showStateLoopItem _optParams (StateLoopItem sStp sForc _sFStp stateDur sBal _sRes) =
  printf " SL: %2d" sStp ++ printfStateMap sForc stateDur
  ++ " | Balance: " ++ printfBalanceMap sBal

printEtaLoop ::
  (UV.Unbox a, Arith.Constant a,Show a, PrintfArg a,node~Node,a~Double,
   FormatValue.FormatValue a,Show (simVec Double),
   Show node,
   Show (intVec Double),
   SV.Walker intVec,
   SV.Storage intVec Double,
   SV.FromList intVec,
   SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   Node.C node,
   z~ Type.OptimiseStateAndSimulate
   node sweep sweepVec Double intVec Double simVec c efaVec d,
   z0 ~ Type.OptimisationPerState node00 Double) =>
  One.OptimisationParams node [] Sweep UV.Vector a
  -> [EtaLoopItem node Sweep UV.Vector a z0 z] -> [IO ()]
printEtaLoop optParams ol =
  iterateLoops (optParams) printEtaLoopItem printInnerLoopItem printBalanceLoopItem printStateLoopItem ol

printEtaLoopItem ::
  (UV.Unbox a,sweep~Sweep,vec~UV.Vector,a~Double,node~Node,
   Node.C node,
   FormatValue.FormatValue a,
   Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimisationParams node [] sweep vec a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> IO ()
printEtaLoopItem params e@(EtaLoopItem _step _sfgIn _sweep _sfgOut res) = --print "EtaLoop"
  do
    let -- dir = printf "outer-loop-%6.6d" olcnt
    --  stoPos = TopoIdx.Position System.Water System.Network
  --    gasPos = TopoIdx.Position System.Gas System.LocalNetwork
        _term = ModPlot.gpXTerm
        _balanceForcing =ilBForcOut $ vlast "printEtaLoopItem" res

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

    putStrLn $ showEtaLoopItem params e

    concurrentlyMany_ [
      ModPlot.drawSweepStateFlowGraph "sfgIn" 0 $ _sfgIn,
      ModPlot.drawSweepStateFlowGraph "sfgOut" 0 $ _sfgOut]
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



printInnerLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  InnerLoopItem node a z0 z ->
  IO()
printInnerLoopItem _optParams (InnerLoopItem _ilStp _ilBForcO _ilBal _ilSForcO
   _ilSDur _balLoop _ ) = print "InnerLoop"


printBalanceLoopItem::
  (z ~ Type.OptimiseStateAndSimulate
   node sweep sweepVec Double intVec Double simVec c efaVec d, node ~ Node,
   Show (intVec Double),
   Show node,
   SV.Walker intVec,Show (simVec Double),
   SV.Storage intVec Double,
   SV.FromList intVec,
   SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   Node.C node,
   z0 ~ Type.OptimisationPerState node0 Double,
   Show a, PrintfArg a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z0 z ->
  IO()
printBalanceLoopItem optParams b@(BalanceLoopItem bStp _bForcing _bFStep _bal res) =
  --print "BalanceLoop"
  do
     let _opt = fst res
         opt2 = snd res
         _gTerm = ModPlot.gpPNG _dir bStp
         _xTerm = ModPlot.gpXTerm
         term = _xTerm
         _dir = printf "outer-loop-%6.6d" bStp
         _stoPos = TopoIdx.Position System.Water System.Network

     putStrLn $ showBalanceLoopItem optParams b
     concurrentlyMany_ [
{-     ModPlot.maxIndexPerState term _opt
     ModPlot.maxEta _xTerm opt2 -}
     --ModPlot.optimalObjs term _opt
       ModPlot.stateRange2 term _opt,

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

       ModPlot.maxState term opt2]
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir bStep) opt-}


printStateLoopItem ::
   (Show node, Show a, PrintfArg a) =>
   One.OptimisationParams node [] Sweep UV.Vector a ->
  StateLoopItem node a (Type.OptimiseStateAndSimulate
                           node sweep sweepVec Double intVec Double simVec c efaVec d) ->
  IO()
printStateLoopItem optParams s@(StateLoopItem _sStep _sForcing _sFStep _stateDurations _sBalance _sResult) =
  do
    putStrLn $ showStateLoopItem optParams s
--    ModPlot.maxState (ModPlot.gpXTerm) _sResult

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
      statForce = Map.map (const $ One.StateForcing Arith.zero) (Bimap.toMapR indexConversionMap)
      b@(BalanceLoopItem _bStp _bForcing _bFStep _bal (opt,opt2)) = vhead "checkRangeIO" $
        balanceIteration sysParams optParams simParams swp initBalF initialBalSteps
        statForce indexConversionMap
      term = ModPlot.gpXTerm
      _posLocal = TopoIdx.Position System.LocalRest System.LocalNetwork
      _posRest = TopoIdx.Position System.Rest System.Network
      _posWater = TopoIdx.Position System.Network System.Water
      _posGas = TopoIdx.Position System.LocalNetwork System.Gas
      _posTrafo = TopoIdx.Position System.LocalNetwork System.Network

  print $ Type.reqsAndDofsSignals $ Type.interpolation opt2
  
  concurrentlyMany_ [
    putStrLn $ showBalanceLoopItem optParams b,
    ModPlot.reqsRec term $ One.reqsRec simParams,
    ModPlot.sweepStackPerStateCondition term optParams swp,
    ModPlot.stateRange2 term opt,
    ModPlot.maxState term opt2,
    ModPlot.maxEta term opt2,
    ModPlot.maxObj term opt2,
    ModPlot.maxPos _posLocal term opt2,
    ModPlot.maxPos _posRest term opt2,
    ModPlot.maxPos _posWater term opt2,
    ModPlot.maxPos _posGas term opt2,
    ModPlot.maxPos _posTrafo term opt2,
    ModPlot.givenSignals term opt2,
    ModPlot.simulationSignals term opt2,
    ModPlot.drawSweepStackStateFlowGraph (Idx.State 0) [0.3,0.5] 0 swp
    ]




        