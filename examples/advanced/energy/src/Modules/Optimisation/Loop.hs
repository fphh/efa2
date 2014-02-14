{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Modules.Optimisation.Loop where


--import qualified Modules.System as System 
import Modules.System (Node)
import qualified Modules.Plot as ModPlot
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO

import EFA.Equation.Result(Result(Determined,Undetermined))
import qualified EFA.Application.Type as Type
import EFA.Application.Type (EnvResult)
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

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
--import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(Data)) --, Apply)

import EFA.Utility.List (vhead) --, vlast)
--import EFA.Utility.Async (concurrentlyMany_)

--import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
--import Data.Vector (Vector)
--import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)

import Text.Printf (printf, PrintfArg,IsChar)

data BalanceLoopItem node a z0 z =
  BalanceLoopItem { bStep :: Int, 
                    bForcing :: Map.Map node (One.SocDrive a),
                    bFStep ::  Map.Map node (One.SocDrive a),
                    balance :: One.Balance node a,
                    bResult :: (z0,z)}

data StateLoopItem node a z = StateLoopItem 
                         { sStep ::Int, 
                           sForcing  :: Map.Map Idx.State (One.StateForcing a),
                           sFStep :: Map.Map Idx.State (One.StateForcing a), 
                           stateDurations :: One.StateDurations a,
                           sBalance :: One.Balance node a,
                           sResult :: z }

data InnerLoopItem node a z0 z = InnerLoopItem
                         { ilStep :: Int,
                           ilBForcOut :: Map.Map node (One.SocDrive a),
                           ilBalance :: One.Balance node a,
                           ilSForcOut :: Map.Map Idx.State (One.StateForcing a), 
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
                One.OptimisationParams a list sweep vec a -> 
                One.Balance node a ->  Bool
checkBalance optParams bal = all g $ Map.elems bal  
  where g x = Arith.abs x < eps
        One.BalanceThreshold eps =  One.balanceThreshold optParams

iterateBalanceUntil ::
  (Ord node,Arith.Sum a, Ord a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep vec a ->
  [BalanceLoopItem node a (Type.OptimisationPerState node a) z] ->
  (Map.Map node (One.SocDrive a), Type.OptimalSolutionPerState node a)
iterateBalanceUntil optParams balanceLoop =
  (bForcing $ lastElem, Type.optimalSolutionPerState $ fst $ bResult $ lastElem)
  where f x = bStep x < maxStepCnt-1 
        One.MaxBalanceIterations maxStepCnt = One.maxBalanceIterations optParams
        lastElem = vhead "interateBalanceUntil" $ dropWhile f balanceLoop
                     
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
  Map.Map Idx.State (One.StateForcing a) ->
  [BalanceLoopItem node a (Type.OptimisationPerState node a)
    (Type.OptimiseStateAndSimulate node sweep UV.Vector a intVec b simVec c efaVec d)]

balanceIteration sysParams optParams simParams perStateSweep balForceIn statForceIn =
  go 0 balForceIn initialSteps (accessf resStart) resStart
  where

    seed = One.balanceForcingSeed optParams
    initialSteps = Map.map (\_ -> seed) balForceIn -- statForceIn
    resStart = fsys balForceIn
    fsys bf = NonIO.optimiseAndSimulate sysParams optParams simParams bf statForceIn perStateSweep
    
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

getStateTimes :: FlowState.Graph node edge sectionLabel nodeLabel storageLabel edgeLabel carryLabel-> One.StateDurations sectionLabel
getStateTimes sfg = Map.map g $ StateQty.states sfg
  where g x = FlowTopo.label x


-- | Find a solution where all states occur in the simlation signals at minimum state forcing, 
-- | measurement unit ist state duration
getStateTime :: Idx.State -> 
                FlowState.Graph node edge sectionLabel nodeLabel storageLabel edgeLabel carryLabel-> 
                sectionLabel
getStateTime stateIdx sfg = FlowTopo.label $ f state
  where state= Map.lookup stateIdx (StateQty.states sfg) 
        f (Just st) = st 
        f (Nothing) = error ("Error in getStateTime: State " ++ show stateIdx ++ "doesn't exist in StateflowGrapgh")

data StateForceDemand = MoreForcingNeeded | CorrectForcing | 
                        NoForcingNeeded | LessForcingNeeded

checkStateTimes :: Ord a => One.OptimisationParams a [] Sweep vec a -> 
               One.StateDurations a -> Bool  
checkStateTimes optParams stateDurations = all g $ Map.elems stateDurations 
  where g x = x > eps
        (One.StateTimeThreshold eps) = One.stateTimeThreshold optParams
  
iterateStateUntil :: 
  One.OptimisationParams node list sweep vec a
  -> [StateLoopItem node a z]
  -> (Map.Map Idx.State (One.StateForcing a),
      One.StateDurations a,One.Balance node a)
iterateStateUntil optParams stateLoop = 
  (sForcing $ lastElem , stateDurations $ lastElem, sBalance $ lastElem)
  where lastElem = vhead "interateStateUntil" $ dropWhile f $ stateLoop
        f x =  sStep x > maxStepCnt-1
        (One.MaxStateIterations maxStepCnt) = One.maxStateIterations optParams 
        thr = One.stateTimeThreshold optParams
        


stateIteration ::
  (sigVec ~[],d ~ b,efaVec ~ [],simVec ~ [], intVec ~ [], (a ~ b),UV.Unbox b,
  
   Eq a, Num a,
   Arith.Sum a,
   Ord a,
   Show node,
   Show a,
   Node.C node,
   Arith.ZeroTestable a,
   Arith.Constant a
  ) =>
  One.SystemParams node a ->
  One.OptimisationParams node [] Sweep UV.Vector a ->
  One.SimulationParams node sigVec a ->
  Map.Map Idx.State (Map.Map [a] (Maybe (a, a, EnvResult node a))) ->
  Map.Map Idx.State (One.StateForcing a) ->
  [StateLoopItem node a 
   (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)]
stateIteration sysParams optParams simParams optimalObjectivePerState stateForceIn  = 
  go 0  stateForceIn initialSteps initialResults 
  where initialSteps = Map.map (\_ -> seed) stateForceIn
        seed = One.stateForcingSeed optParams
        fsys sf = NonIO.optimiseStateAndSimulate sysParams optParams simParams sf optimalObjectivePerState
        initialResults = fsys stateForceIn
        (One.StateTimeThreshold thr) = One.stateTimeThreshold optParams
        
        accessTimes res =
          Map.map f $ getStateTimes $ Type.stateFlowGraph $ Type.analysis res
           where f (Determined (Data x)) = x
                 f Undetermined  = error "State Time undetermined"
                 
        accessBal res = StateEta.balanceFromRecord (One.storagePositions sysParams) $ 
                      Type.signals $ Type.simulation $ res
                
         
        go cnt force step res  =
          StateLoopItem cnt force step (accessTimes res) bal res
            : go (cnt+1) force1 step1 res1 
          where
            force1 = Map.fromList $ zipWith g (Map.toList force) (Map.toList step)      
            g (idx1,x) (idx2,y) =
              if idx1 /= idx2 then error msg else (idx1, x~+y)
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
         eval x y thr = case ( x == 0 && y > 0,y == 0, y < thr ) of
                  (True, _, _) -> NoForcingNeeded 
                  (False,False,True) -> CorrectForcing
                  (False,True,_) -> MoreForcingNeeded
                  (False,False,False) -> LessForcingNeeded
                  
       in  case (eval x0 y0 thr , eval x1 y1 thr )  of
            (_, NoForcingNeeded) -> One.StateForcing $ Arith.zero
            (_, CorrectForcing) -> One.StateForcing $ Arith.zero
            (NoForcingNeeded, _) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing seed)
            (CorrectForcing,_) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing seed)
            (MoreForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (LessForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (MoreForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3
            (LessForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3


{-
iterateInnerLoopWhile ::
  One.OptimisationParams node [] Sweep vec sigVec a ->
  EnvResult node (Sweep vec a) ->
  [InnerLoopItem node a (Type.Optimisation node Sweep vec sigVec a)]
  
iterateInnerLoopWhile  optParams stateFlowGraphOpt = 
  takeWhile f $ iterateInnerLoop optParams reqsRec stateFlowGraphOpt
   where (One.MaxInnerLoopIterations maxCnt) = One.maxInnerLoopIterations optParams  
         stateFlowGraphOpt = One.stateFlowGraphOpt optParams
         reqsRec = One.reqsRec optParams
         f x = ilStep x < maxCnt
-}             

iterateInnerLoop :: 
  (intVec~[], efaVec ~ [], simVec ~ [],a ~ d,sigVec ~ [],d ~ b, 
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
  Map.Map Idx.State (One.StateForcing a) ->
  [InnerLoopItem node a (Type.OptimisationPerState node a)
     (Type.OptimiseStateAndSimulate node Sweep UV.Vector a intVec b simVec c efaVec d)]
iterateInnerLoop sysParams optParams simParams perStateSweep balForceIn stateForceIn = 
  go 0 balForceIn stateForceIn
  where
    go cnt balForce statForce =
      InnerLoopItem cnt balForce1 bal statForce1 sta balLoop statLoop
        : go (cnt+1) balForce1 statForce1
      where 
        balLoop = balanceIteration sysParams optParams simParams perStateSweep balForce statForce
        (balForce1, optimalObjectivePerState) = iterateBalanceUntil optParams balLoop            
        statLoop = stateIteration sysParams optParams simParams optimalObjectivePerState statForce
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
iterateEtaWhile sysParams optParams simParams = go 0  $ One.stateFlowGraphOpt optParams 
   where (One.MaxEtaIterations maxCnt) = One.maxEtaIterations optParams
         balf =  One.initialBattForcing optParams
         graphStates = StateQty.states $ One.stateFlowGraphOpt optParams
         statf = Map.map (const $ One.stateForcingSeed optParams) graphStates
         go cnt sfg = EtaLoopItem cnt sfg sfg1 res : go (cnt+1) sfg1
           where
            sweep = Base.perStateSweep sysParams optParams sfg
            res = iterateInnerLoop sysParams optParams simParams sweep balf statf
            sfg1 = Type.stateFlowGraphSweep 
                   $ sResult 
                   $ vhead "iterateEtaUntil 2"
                   $ stateLoop
                   $ vhead "iterateEtaUntil 1" res



-------------------------------- OutPut Below -----------------------------------

{-showEtaLoop ::
  (PrintfArg a, UV.Unbox a, Arith.Product a, Show a,
   Node.C node, Sweep.SweepClass sweep vec a, Show node) =>
   One.OptimisationParams node [] Sweep UV.Vector a ->
   [EtaLoopItem node Sweep UV.Vector a z] -> [String]-}


printfMap :: (Show k, Show a) => Map.Map k a -> String 
printfMap m = concat $ map f $ Map.toList m
  where f (k, x) = "(" ++ (printf "%5s" (show k)) ++ ","  ++ (printf "%5s" (show x)) ++ ")"
    

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
showEtaLoopItem optParams (EtaLoopItem step sfgIn sfgOut _) =  
      "EL: " ++ printf "%8d" step 

showInnerLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a -> 
  InnerLoopItem node a z0 z -> 
  String
showInnerLoopItem optParams (InnerLoopItem ilStep ilBForcOut ilBalance ilSForcOut 
   ilSDurations balanceLoop _ ) = "IL: " ++ printf "%8d" ilStep 

showBalanceLoopItem::(Show a, Show node)=>
  One.OptimisationParams node [] Sweep UV.Vector a -> 
  BalanceLoopItem node a z0 z -> 
  String
showBalanceLoopItem optParams (BalanceLoopItem bStep bForcing bFStep balance _) =  
        " BL:  " ++ printf "%2d" bStep ++ 
        " BF:  " ++ printfMap bForcing ++ 
        " Bal: " ++ printfMap balance

showStateLoopItem :: (Show a, Show node) =>
  One.OptimisationParams node [] Sweep UV.Vector a -> 
  StateLoopItem node a z -> 
  String
showStateLoopItem optParams (StateLoopItem sStep sForcing sFStep stateDurations sBalance sResult) = 
        " SL:  " ++ printf "%8d" sStep 
        ++ " SF:  " ++ printfMap sForcing
--        " Dur: " ++ printfMap stateDurations ++
--        " Bal: " ++ printfMap sBalance


printEtaLoop :: (UV.Unbox a, Arith.Constant a) =>
  One.OptimisationParams node [] Sweep UV.Vector a
  -> [EtaLoopItem node Sweep UV.Vector a z0 z] -> [IO ()]
printEtaLoop optParams ol =
  iterateLoops (optParams) printEtaLoopItem printInnerLoopItem printBalanceLoopItem printStateLoopItem ol

printEtaLoopItem ::
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimisationParams node f sweep vec a ->
  EtaLoopItem node Sweep UV.Vector a z0 z -> IO ()
printEtaLoopItem _params (EtaLoopItem step sfgIn sfgOut _) = print "EtaLoop"
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
printInnerLoopItem optParams (InnerLoopItem ilStep ilBForcOut ilBalance ilSForcOut 
   ilSDurations balanceLoop _ ) = print "InnerLoop"
      
printBalanceLoopItem::
  One.OptimisationParams node [] Sweep UV.Vector a -> 
  BalanceLoopItem node a z0 z -> 
  IO()
printBalanceLoopItem optParams (BalanceLoopItem bStep bForcing bFStep balance opt) = --print "BalanceLoop" 


  --Just $ do
    --let --dir = printf "outer-loop-%6.6d" bStep
 --       stoPos = TopoIdx.Position System.Water System.Network
--    ModPlot.maxEta ModPlot.gpXTerm opt
{-    ModPlot.maxEta (ModPlot.gpPNG dir ilcnt) opt
    ModPlot.optimalObjs (ModPlot.gpPNG dir ilcnt) opt
    ModPlot.simulationGraphs (ModPlot.dotPNG dir ilcnt) opt

   -- ModPlot.maxEtaPerState (ModPlot.gpPNG dir ilcnt) opt
   -- ModPlot.maxPosPerState (ModPlot.gpPNG dir ilcnt) stoPos opt

    ModPlot.maxPos stoPos (ModPlot.gpPNG dir ilcnt) opt

    -- das aktiviert das schreiben der zustandsflussgraphen 
    -- pro parzelle (Achtung, ziemlich viel!!!)
    -- ModPlot.optimalObjectivePerState (ModPlot.dotPNG dir ilcnt) opt
    -- ModPlot.simulationSignals (ModPlot.gpPNG dir ilcnt) opt
    -- ModPlot.givenSignals (ModPlot.gpPNG dir ilcnt) opt
    ModPlot.maxState (ModPlot.gpPNG dir ilcnt) opt
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir ilcnt) opt-}
    print "BalanceLoop" 

printStateLoopItem :: 
  One.OptimisationParams node [] Sweep UV.Vector a -> 
  StateLoopItem node a z -> 
  IO()
printStateLoopItem optParams (StateLoopItem sStep sForcing sFStep stateDurations sBalance sResult) = print "stateLoop" 



{-

iterateLoops ::
  One.MaxEtaIterations ->
  (Int -> EtaLoopItem node a z -> Maybe b) ->
  (Int ->
    Int -> 
    BalanceLoopItem node a (Type.Optimisation node sweep vec a) ->
    Maybe b) ->
  [EtaLoopItem node a z] ->
  [Maybe b]
iterateLoops optParams etaLoopFunction innerLoopFunction etaLoop =
  concat $ map g $ take n etaLoop
  where g x = map (innerLoopFunction optParams) (etaLoopFunction x) ++ [olif n x]
        f (EtaLoopItem cnt _ _ _ _ innerLoop) = take nos innerloop
        (One.MaxEtaIterations n) = One.maxmaxEtaIterations optParam


showEtaLoopItem ::
  (UV.Unbox a, Arith.Product a,
   PrintfArg a,Show node, Show a,
   Node.C node, Sweep.SweepClass sweep vec a) =>
  Int -> EtaLoopItem node Sweep UV.Vector a z -> Maybe String
showEtaLoopItem _ (EtaLoopItem step sfgIn sfgOut il) =
  let numOfSt = Map.size $ StateQty.states $ sfgOut
  in Just $  printf "%8d%8d" step numOfSt
     
--     "ETA-Loop: " ++ (show numOfSt) ++ " " ++ (show nos) ++ " " ++ (show f)++ " " ++ (show bal)++ " " ++ (show $ eta opt)++ " " ++(show ss) 
  --   printf "%8d%8d%50s%50s%50s%50s\n" numOfSt nos (show f) (show bal) (eta opt)  (show ss)
  -- "%8d%8d%24e%24e%24e%24e\n"




showBalanceLoopItem ::
  (UV.Unbox a, Arith.Product a, Show a, Show node, 
   Node.C node, Sweep.SweepClass sweep vec a, PrintfArg a) =>
  Int -> Int ->
  BalanceLoopItem node a (Type.OptimisationPerState node a,
                           Type.OptimiseStateAndSimulate node sweep UV.Vector a intVec b simVec c efaVec d)
--  BalanceLoopItem node a (Type.Optimisation node sweep vec a, Map.Map node a) ->
  Maybe String
showBalanceLoopItem _olcnt ilcnt (BalanceLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.analysis opt
  in Just $ printf "%8d" numOfSt
     
     --"Balance-Loop: " ++ (show numOfSt)++" "++(show ilcnt) ++" "++(show f) ++" "++(show bal) ++" "++(show $ eta opt) ++" "++(show st)
     --printf "%8d%8d%50s%50s%50s%50s" numOfSt ilcnt (show f) (show bal) (eta opt) (show st)
     -- $ (show numOfSt) ++(show ilcnt) ++(show f) ++(show bal) ++(eta opt) ++(show st)
-}
{-
showStateLoopItem ::
  (UV.Unbox a, Arith.Product a,Show a, 
   Node.C node, Sweep.SweepClass sweep vec a, PrintfArg a, 
   IsChar (One.StateForcing a)) =>
  Int -> Int -> Int ->
  StateLoopItem node a z ->
  Maybe String
showStateLoopItem _olcnt _ slcnt (StateLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "State-Loop: " ++(show numOfSt) ++" "++ (show slcnt) ++" "++ (show f) ++" "++ (show bal) ++" "++ (show (eta opt)) ++" "++ (show st)
  --   printf "%8d%8d%50s%50s%50s%50s" numOfSt slcnt f bal (eta opt) st
  -- $ (show numOfSt) ++ (show slcnt) ++ (show f) ++ (show bal) ++ (show (eta opt)) ++ (show st) 
-}




