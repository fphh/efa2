{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Modules.Optimisation.Loop where


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

--import qualified EFA.Graph.Topology as Topology
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~+), (~/))
import qualified EFA.Equation.Arithmetic as Arith

--import EFA.Equation.Result (Result(Determined))

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.State.SystemEta as StateEta
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State as FlowState

-- import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

--import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(Data)) --, Nil, Apply)

import EFA.Utility.List (vhead) --, vlast)
--import EFA.Utility.Async (concurrentlyMany_)

--import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
--import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

import qualified Data.Bimap as Bimap
--import Data.Bimap (Bimap)

import Text.Printf (printf, PrintfArg) --,IsChar)

data BalanceLoopItem node a z0 z =
  BalanceLoopItem { bStep :: Int,
                    bForcing :: Map.Map node (One.SocDrive a),
                    bFStep ::  Map.Map node (One.SocDrive a),
                    balance :: One.Balance node a,
                    bResult :: (z0,z)}

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
  where g x = Arith.abs x < eps
        One.BalanceThreshold eps =  One.balanceThreshold optParams

iterateBalanceUntil ::
  (Ord node,Arith.Sum a, Ord a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep vec a ->
  [BalanceLoopItem node a (Type.OptimisationPerState node a) z] ->
  (Map.Map node (One.SocDrive a), Type.OptimalSolutionPerState node a)
iterateBalanceUntil optParams balLoop =
  (bForcing $ lastElem, Type.optimalSolutionPerState $ fst $ bResult $ lastElem)
  where f x = (bStep x < maxStepCnt-1)  && not (checkBalance optParams $ balance x)
        One.MaxBalanceIterations maxStepCnt = One.maxBalanceIterations optParams
        lastElem = vhead "interateBalanceUntil" $ dropWhile f balLoop

balanceIteration::
  (efaVec~[], intVec ~ [], sweep ~ Sweep, a ~ d, simVec ~ [],
   Ord d,
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
  Map.Map Idx.State (Map.Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  One.IndexConversionMap ->
  [BalanceLoopItem node a (Type.OptimisationPerState node a)
    (Type.OptimiseStateAndSimulate node sweep UV.Vector a intVec b simVec c efaVec d)]

balanceIteration sysParams optParams simParams perStateSweep balForceIn statForceIn indexConversionMap =
  go 0 balForceIn initialSteps (accessf resStart) resStart
  where

    seed = One.balanceForcingSeed optParams
    initialSteps = One.initialBattForceStep optParams
    resStart = fsys balForceIn
    fsys bf = NonIO.optimiseAndSimulate sysParams optParams simParams bf statForceIn perStateSweep indexConversionMap

    accessf res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ snd res

    go cnt force step bal res  = BalanceLoopItem cnt force step bal res
                                : go (cnt+1) force1 step1 bal1 res1

      where force1 = Map.mapWithKey (\k x -> One.setSocDrive $ (One.getSocDrive x) ~+
                                           (One.getSocDrive $ g $ Map.lookup k step)) force
            g (Just x) = x
            g Nothing = error ("Error in balanceIteration: keys in StorageForcingMap and StepMap differ")

            res1 = fsys $ force1
            bal1 = accessf res1
            step1 = Map.mapWithKey (\ k s -> One.setSocDrive $ f  k s) step

            f k stp = let
              y0 = j $ Map.lookup k $ bal
              y1 = j $ Map.lookup k $ bal1
              st = One.getSocDrive stp

              j (Just x) = x
              j Nothing =  error ("Error in balanceIteration: Storage not found")

              _3 = Arith.fromInteger 3
              _2 = Arith.fromInteger 2

              newStepSize = (Arith.abs st) ~/(Arith.one ~+
                        (Arith.abs $ y0) ~/ (Arith.abs $ y1))

              in if False then case (Arith.sign $ y0, Arith.sign $ y1) of
                              (Negative, Negative) -> ((Arith.abs st) ~* _2)
                              (Positive, Positive) -> (Arith.negate $ (Arith.abs st) ~* _2)
                              (Negative, Positive) -> (Arith.negate $ (Arith.abs st) ~/ _3)
                              (Positive, Negative) -> ((Arith.abs st) ~/ _3)
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

-- | TODO : move to korrekt Position -- State Labels == Times ??
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

checkStateTimes :: Ord a => One.OptimisationParams a [] Sweep vec a ->
               One.StateDurations a -> Bool
checkStateTimes optParams stateDurs = all g $ Map.elems stateDurs
  where g x = x > eps
        (One.StateTimeThreshold eps) = One.stateTimeThreshold optParams

iterateStateUntil ::
  One.OptimisationParams node list sweep vec a
  -> [StateLoopItem node a z]
  -> (Map.Map Idx.AbsoluteState (One.StateForcing a),
      One.StateDurations a,One.Balance node a)
iterateStateUntil optParams statLoop =
  (sForcing $ lastElem , stateDurations $ lastElem, sBalance $ lastElem)
  where lastElem = vhead "interateStateUntil" $ dropWhile f $ statLoop
        f x =  sStep x > maxStepCnt-1
        (One.MaxStateIterations maxStepCnt) = One.maxStateIterations optParams
        -- thr = One.stateTimeThreshold optParams



stateIteration ::
  (sigVec ~[],d ~ b,efaVec ~ [],simVec ~ [], intVec ~ [], (a ~ b),UV.Unbox b,node~Node,
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
  Map.Map Idx.State (Map.Map [a] (Maybe (a, a, EnvResult node a))) ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  One.IndexConversionMap ->
  [StateLoopItem node a
   (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)]
stateIteration sysParams optParams simParams optimalObjectivePerState stateForceIn indexConversionMap =
  go 0 stateForceIn initialSteps initialResults
  where initialSteps = Map.map (\x -> if x==0 then seed else One.DontForceState) initialTimes
        seed = One.stateForcingSeed optParams
        fsys sf = NonIO.optimiseStateAndSimulate sysParams optParams simParams sf optimalObjectivePerState indexConversionMap
        initialResults = fsys stateForceIn
        initialTimes = accessTimes initialResults
        -- forceIn = Map.map (const $ One.StateForcing Arith.zero) $ Map.filter (==0) initialTimes

        (One.StateTimeThreshold thr) = One.stateTimeThreshold optParams

        accessTimes res =
          Map.map f $ getStateTimes stateForceIn $ Type.stateFlowGraph $ Type.analysis res
           where f (Determined (Data x)) = x
                 f Undetermined  = error "State Time undetermined"

        accessBal res = StateEta.balanceFromRecord (One.storagePositions sysParams) $
                      Type.signals $ Type.simulation $ res


        go cnt force step res  =
          StateLoopItem cnt force step (accessTimes res) bal res
            : go (cnt+1) force1 step1 res1
          where
            force1 = Map.fromList $ zipWith g (Map.toList force) (Map.toList step)

            g (idx1,One.StateForcing x) (idx2,One.DontForceState) =
              if idx1 /= idx2 then error msg else (idx1, One.StateForcing $ x)
            g (idx1,One.StateForcing x) (idx2,One.StateForcingStep y) =
              if idx1 /= idx2 then error msg else (idx1, One.StateForcing $ x~+y)

            res1 = fsys force1
            times = accessTimes res
            times1 = accessTimes res1
            bal = accessBal res1

            step1 = Map.fromList $ zipWith3 (changeStateForce seed thr)
                       (zip (Map.elems times)( Map.elems times1))
                       (zip (Map.elems force) (Map.elems force1))
                       (Map.toList step)

            msg = "Error in StateIteration"
                  ++ " -- differing States between Forcing,"
                  ++ " Stateduration and Step"



changeStateForce::
  (Eq a1, Eq a,
   Num a1, Num a,
   Num a2, Ord a3,
   Ord a2,Arith.Constant a3, a1 ~ a) =>
  One.StateForcingStep a3 ->
  a2-> (a2, a2)->
  (One.StateForcing a, One.StateForcing a1)->
  (t, One.StateForcingStep a3)->
  (t, One.StateForcingStep a3)
changeStateForce _ _ (_,_) (_,_) (idx,One.DontForceState) = (idx,One.DontForceState)
changeStateForce seed thr (y0,y1) (One.StateForcing x0,One.StateForcing x1) (idx,One.StateForcingStep st) = (idx,st1)
  where
     st1 =
       let
         _3 = Arith.fromInteger 3
         _2 = Arith.fromInteger 2
         eval force timeDur =
           case (force == 0, timeDur == 0, timeDur < thr ) of
                (True, False, _)      -> NoForcingNeeded
                (False, False, True)  -> CorrectForcing
                (_, True, _)      -> MoreForcingNeeded
                (False, False, False) -> LessForcingNeeded

       in case (eval x0 y0, eval x1 y1) of
            (_, NoForcingNeeded) -> trace "a1" $  One.StateForcingStep $ Arith.zero
            (_, CorrectForcing) -> trace "a2" $ One.StateForcingStep $ Arith.zero
            (NoForcingNeeded, _) -> trace "a3" $ seed
            (CorrectForcing, MoreForcingNeeded) -> trace "a4" $ seed
            (CorrectForcing, LessForcingNeeded) -> trace "a5" $ fmap Arith.negate seed
            (MoreForcingNeeded, MoreForcingNeeded) -> trace "a6" $ if st == Arith.zero then seed else One.StateForcingStep $ (Arith.abs st) ~* _2
            (LessForcingNeeded, LessForcingNeeded) -> trace "a7" $ One.StateForcingStep $ Arith.negate $ (Arith.abs st) ~* _2
            (MoreForcingNeeded, LessForcingNeeded) -> trace "a8" $ One.StateForcingStep $ Arith.negate $ (Arith.abs st) ~/ _3
            (LessForcingNeeded, MoreForcingNeeded) -> trace "a9" $ if st == Arith.zero then seed else One.StateForcingStep $ (Arith.abs st) ~/ _3
--            (x,y) -> error (show x ++" " ++ show y)





{-
changeStateForce:: (Eq a1, Eq a,
                    Num a1, Num a,
                    Num a2, Ord a3,
                    Ord a2,Arith.Constant a3) =>
                   One.StateForcing a3
                   -> a2
                   -> (a2, a2)
                   -> (One.StateForcing a, One.StateForcing a1)
                   -> (t, One.StateForcing a3)
                   -> (t, One.StateForcing a3)
changeStateForce seed thr (y0,y1) (One.StateForcing x0,One.StateForcing x1) (idx,One.StateForcing st) = (idx,st1)
  where
     st1 =
       let
         _3 = Arith.fromInteger 3
         _2 = Arith.fromInteger 2
         eval force timeDur thr =
           case ( force == 0 && timeDur > 0, timeDur == 0, timeDur < thr ) of
                (True, _, _)          -> NoForcingNeeded
                (False, False, True)  -> CorrectForcing
                (False, True, _)      -> MoreForcingNeeded
                (False, False, False) -> LessForcingNeeded

       in case (eval x0 y0 thr, eval x1 y1 thr) of
            (_, NoForcingNeeded) -> One.StateForcing $ Arith.zero
            (_, CorrectForcing) -> One.StateForcing $ Arith.zero
            (NoForcingNeeded, _) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing seed)
            (CorrectForcing, _) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing seed)
            (MoreForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (LessForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (MoreForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3
            (LessForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3
-}


{-
iterateInnerLoopWhile  optParams stateFlowGraphOpt =
  takeWhile f $ iterateInnerLoop optParams reqsRec stateFlowGraphOpt
   where (One.MaxInnerLoopIterations maxCnt) = One.maxInnerLoopIterations optParams
         stateFlowGraphOpt = One.stateFlowGraphOpt optParams
         reqsRec = One.reqsRec optParams
         f x = ilStep x < maxCnt
-}

iterateInnerLoop ::
  (intVec~[], efaVec ~ [], simVec ~ [],a ~ d,sigVec ~ [],d ~ b,node~Node,
   Ord d,
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
  Map.Map Idx.State (Map.Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.AbsoluteState (One.StateForcing a) ->
  One.IndexConversionMap ->
  [InnerLoopItem node a (Type.OptimisationPerState node a)
     (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)]
iterateInnerLoop sysParams optParams simParams perStateSweep balForceIn stateForceIn indexConversionMap =
  go 0 balForceIn stateForceIn
  where
    go cnt balForce statForce =
      InnerLoopItem cnt balForce1 bal statForce1 sta balLoop statLoop
        : go (cnt+1) balForce1 statForce1
      where
        balLoop = balanceIteration sysParams optParams simParams perStateSweep balForce statForce indexConversionMap
        (balForce1, optimalObjectivePerState) = iterateBalanceUntil optParams balLoop
        statLoop = stateIteration sysParams optParams simParams optimalObjectivePerState statForce indexConversionMap
        (statForce1,sta,bal) = iterateStateUntil optParams statLoop




{-
iterateEtaWhile::(sigVec ~ [], a ~ b,
                  Arith.Constant a,
                  UV.Unbox a,
                  Arith.ZeroTestable a,
                  Node.C node,
                  Num a,
                  Ord a,
                  Show a,
                  Show node)=>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node sigVec a ->
  [EtaLoopItem node Sweep UV.Vector a (Type.OptimisationPerState node a)
   (Type.OptimiseStateAndSimulate node Sweep UV.Vector a [] b0 [] c0 [] a)]  -}
iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   Arith.Constant a) =>
  One.SystemParams Node a->
  One.OptimisationParams Node [] Sweep UV.Vector a->
  One.SimulationParams Node [] a->
  [EtaLoopItem Node Sweep UV.Vector a (Type.OptimisationPerState Node a)
   (Type.OptimiseStateAndSimulate Node Sweep UV.Vector a [] a [] c [] a)]
iterateEtaWhile sysParams optParams simParams = go 0  $ One.stateFlowGraphOpt optParams
   where -- (One.MaxEtaIterations maxCnt) = One.maxEtaIterations optParams
         balf =  One.initialBattForcing optParams
         sfgo = One.stateFlowGraphOpt optParams
         -- graphStates = StateQty.states sfgo
         indexMap = Bimap.toMapR $ ModUt.indexConversionMap System.topology sfgo
         statf = Map.map (const $ One.StateForcing Arith.zero) indexMap
         go cnt sfg = EtaLoopItem cnt sfg sfg1 res : go (cnt+1) sfg1
           where
            sweep = Base.perStateSweep sysParams optParams sfg
            indexConversionMap = ModUt.indexConversionMap System.topology sfg
            res = iterateInnerLoop sysParams optParams simParams sweep balf statf indexConversionMap
            sfg1 = Type.stateFlowGraphSweep
                   $ sResult
                   $ vhead "iterateEtaWhile 2"
                   $ stateLoop
                   $ vhead "iterateEtaWhile 1" res



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
--          "ASt: " ++ (printf "%d" k) ++ ", F: " ++ (printf "%.2f" x) ++ ", D: " ++ (printf "%.2f" y)
          "|  State: " ++ (printf "%2d" k) ++ " (" ++ (printf "%10.5f" x) ++ " ," ++ (printf "%10.5f" y) ++ " )  "

printfBalanceFMap ::
  (Show a, PrintfArg a1, PrintfArg t1, Arith.Constant a1) =>
  Map.Map a (One.SocDrive a1) -> Map.Map t t1 -> [Char]
printfBalanceFMap forceMap balanceMap = concat$ zipWith f (Map.toList forceMap) (Map.toList balanceMap)
  where f (k,x) (_, y) =
          "   | Sto: " ++ (printf "%7s" $ show k) ++ "   | F: " ++ (printf "%5.2f" $ One.getSocDrive x)
          ++ "   | B: " ++ (printf "%5.2f" y)

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map.Map a t -> [Char]
printfBalanceMap balanceMap = concat $ map f (Map.toList balanceMap)
  where f (k,x) =
          " Sto: " ++ (printf "%7s" $ show k) ++ " B: " ++ (printf "%5.2f" x)
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

headLine :: String
headLine = printf "%8s%8s%24s%24s%24s%24s" "Step" "States" "Forcing" "Balance" "Eta" "StepSize"

showEtaLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> String
showEtaLoopItem _optParams (EtaLoopItem step _sfgIn _sfgOut _) =
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
        " BL: " ++ printf "%2d" bStp ++
         printfBalanceFMap bForc bal

--        " BF:  " ++ printfMap bForcing ++
--        " Bal: " ++ printfMap balance

showStateLoopItem :: (Show a, Show node,PrintfArg a) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  StateLoopItem node a z ->
  String
showStateLoopItem _optParams (StateLoopItem sStp sForc _sFStp stateDur sBal _sRes) =
  printf " SL: %2d" sStp ++ printfStateMap sForc stateDur
  ++ " | Balance: " ++ printfBalanceMap sBal
--  ++ "States:\t" ++ show (Map.keys stateDurations)
--  ++ "\nStateForcing And Duration:\n" ++ printfStateMap sForcing stateDurations
--  ++ " StateDuration:\n" ++ printfMap stateDurations
--  ++ " Balance: " ++ printfBalanceMap sBalance


printEtaLoop ::
  (UV.Unbox a, Arith.Constant a,
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
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimisationParams node f sweep vec a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> IO ()
printEtaLoopItem _params (EtaLoopItem _step _sfgIn _sfgOut _) = print "EtaLoop"
--  do
--    let -- dir = printf "outer-loop-%6.6d" olcnt
        --stoPos = TopoIdx.Position System.Water System.Network
        --gasPos = TopoIdx.Position System.Gas System.LocalNetwork

--    putStrLn (printf "Loop %6.6d" olcnt)

--    concurrentlyMany_ [
      --ModPlot.maxEtaPerState ModPlot.gpXTerm opt,
 --     ModPlot.simulationGraphs ModPlot.dotXTerm opt ]
      --ModPlot.expectedEtaPerState ModPlot.gpXTerm opt,
      --ModPlot.maxObjPerState ModPlot.gpXTerm opt ]


{-
    concurrentlyMany_ [
      ModPlot.simulationGraphs ModPlot.dotXTerm opt,
      ModPlot.simulationSignals ModPlot.gpXTerm opt,
      ModPlot.maxPos stoPos ModPlot.gpXTerm opt,
      ModPlot.maxPos gasPos ModPlot.gpXTerm opt,
      ModPlot.maxState ModPlot.gpXTerm opt,
      ModPlot.maxObj ModPlot.gpXTerm opt ]
-}

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
   node sweep sweepVec Double intVec Double simVec c efaVec d,
   Show (intVec Double),
   Show node,
   SV.Walker intVec,
   SV.Storage intVec Double,
   SV.FromList intVec,
   SV.Walker simVec,
   SV.Storage simVec Double,
   SV.FromList simVec,
   Node.C node,
   z0 ~ Type.OptimisationPerState node0 Double) =>
  One.OptimisationParams node [] Sweep UV.Vector a ->
  BalanceLoopItem node a z0 z ->
  IO()
printBalanceLoopItem _optParams (BalanceLoopItem bStp _bForcing _bFStep _bal res) =
  --print "BalanceLoop"
  do
     let _opt = fst res
         opt2 = snd res
         _gTerm = ModPlot.gpPNG _dir bStp
         _xTerm = ModPlot.gpXTerm
         term = _xTerm
         _dir = printf "outer-loop-%6.6d" bStp
         _stoPos = TopoIdx.Position System.Water System.Network
--    ModPlot.maxEta xTerm opt2
--    ModPlot.maxEta gTerm opt2
--    ModPlot.optimalObjs (ModPlot.gpPNG dir bStep) opt
--    ModPlot.simulationGraphs (ModPlot.dot dir bStep) opt2
     print (Type.reqsAndDofsSignals $ Type.interpolation opt2)
     ModPlot.givenSignals term opt2
--    ModPlot.maxEtaPerState (ModPlot.gpPNG dir bStep) opt
   -- ModPlot.maxPosPerState (ModPlot.gpPNG dir bStep) stoPos opt

--    ModPlot.maxPos stoPos (ModPlot.gpPNG dir bStep) opt

    -- das aktiviert das schreiben der zustandsflussgraphen
    -- pro parzelle (Achtung, ziemlich viel!!!)
    -- ModPlot.optimalObjectivePerState (ModPlot.dotPNG dir bStep) opt
     ModPlot.simulationSignals term opt2

--    ModPlot.maxState (ModPlot.gpPNG dir bStep) opt
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir bStep) opt-}
     print "BalanceLoop"

printStateLoopItem ::
  One.OptimisationParams node [] Sweep UV.Vector a ->
  StateLoopItem node a z ->
  IO()
printStateLoopItem _optParams (StateLoopItem _sStep _sForcing _sFStep _stateDurations _sBalance _sResult) = print "stateLoop"



