{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Modules.Optimisation.Loop where


--import qualified Modules.System as System 
--import Modules.System (Node)
--import qualified Modules.Plot as ModPlot
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
--import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(Data))

import EFA.Utility.List (vhead) --, vlast)
--import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
--import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
--import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)

--import Text.Printf (printf, PrintfArg,IsChar)

data BalanceLoopItem node a z =
  BalanceLoopItem { bStep :: Int, 
                    bForcing :: Map.Map node (One.SocDrive a), 
                    bFStep ::  Map.Map node (One.SocDrive a),
                    balance :: One.Balance node a,
                    bResult :: z}

data StateLoopItem node a z = StateLoopItem 
                         { sStep ::Int, 
                           sForcing  :: Map.Map Idx.State (One.StateForcing a),  
                           sFStep :: Map.Map Idx.State (One.StateForcing a), 
                           stateDurations :: One.StateDurations a,
                           sResult :: z }

data InnerLoopItem node a z = InnerLoopItem
                         { ilStep :: Int,
                           ilBForcOut :: Map.Map node (One.SocDrive a),
                           ilBalance :: One.Balance node a,  
                           ilSForcOut :: Map.Map Idx.State (One.StateForcing a),  
                           ilSDurations :: One.StateDurations a,
                           balanceLoop :: [BalanceLoopItem node a z], 
                           stateLoop :: [StateLoopItem node a z]}
                         

data EtaLoopItem node sweep vec a z = EtaLoopItem {
  elStep :: Int,
  stateFlowIn :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  stateFlowOut :: (EnvResult node ((sweep:: (* -> *) -> * -> *) vec a)),
  innerLoop :: [InnerLoopItem node a z]} 





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
  Type.Optimisation node sweep vec sigVec a -> a
eta opt =
  case StateEta.etaSys (Type.stateFlowGraph $ Type.simulation opt) of
       Determined (Data e) -> e 
       _ -> error "Main.iterateBalanceIO"

checkBalance :: (Ord a, Arith.Sum a) => 
                One.OptimalEnvParams a [] Sweep vec sigVec a -> 
                One.Balance node a ->  Bool
checkBalance  params bal = all g $ Map.elems bal  
  where g x = Arith.abs x < eps
        One.BalanceThreshold eps =  One.balanceThreshold params

iterateBalanceUntil ::
  (Ord node,Arith.Sum a, Ord a, Arith.Constant a) =>
  One.OptimalEnvParams node [] Sweep vec sigVec a ->
  [BalanceLoopItem node a z] ->
  Map.Map node (One.SocDrive a)
iterateBalanceUntil params balanceLoop =
  bForcing $ vhead "interateBalanceUntil" $ dropWhile f balanceLoop
  where f x = bStep x < maxStepCnt-1 
        One.MaxBalanceIterations maxStepCnt = One.maxBalanceIterations params
                     
balanceIteration ::
  (Arith.Sum a, Ord a, Arith.Product a,
   Arith.Constant a,Ord node, Node.C node,
   Eq (sigVec a),
   Fractional a,
   Show a,
   Show node,
   Show (sigVec a),
   UV.Unbox a,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Storage sigVec (Maybe (Result a)),
   SV.Singleton sigVec,
   SV.Lookup sigVec,
   SV.Len (sigVec a),
   SV.FromList sigVec,
   SV.Find sigVec,
   SV.Convert sigVec UV.Vector,
   SV.Convert sigVec [],
   SV.Convert sigVec sigVec,
   Arith.ZeroTestable a) =>
  One.OptimalEnvParams node [] Sweep UV.Vector sigVec a ->
  Map.Map Idx.State (Map.Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.State (One.StateForcing a) ->
  [BalanceLoopItem node a (Type.Optimisation node Sweep UV.Vector sigVec a)]
balanceIteration params perStateSweep balForceIn statForceIn =
  go 0 balForceIn initialSteps (accessf resStart) resStart
  where

    seed = One.balanceForcingSeed params
    initialSteps = Map.map (\_ -> seed) balForceIn -- statForceIn
    resStart = fsys balForceIn
    fsys bf = NonIO.optimiseAndSimulate params bf statForceIn perStateSweep
    accessf res = StateEta.balanceFromRecord (One.storagePositions params) $ 
                      Type.signals $ Type.simulation res
    
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

checkStateTimes :: Ord a => One.OptimalEnvParams a [] Sweep vec sigVec a -> 
               One.StateDurations a -> Bool  
checkStateTimes params stateDurations = all g $ Map.elems stateDurations 
  where g x = x > eps
        (One.StateTimeThreshold eps) = One.stateTimeThreshold params
  
  

-- iterateStateUntil ::[StateLoopItem node a z] -> 

iterateStateUntil params stateLoop = 
  (sForcing $ vhead "interateStateUntil" sl, sl) 
  where sl = dropWhile f $ stateLoop
        f x =  sStep x > maxStepCnt-1
        (One.MaxStateIterations maxStepCnt) = One.maxStateIterations params 
        thr = One.stateTimeThreshold params


stateIteration ::
  (Arith.Constant a,Eq a, Num a,Ord a, Show a,
   Eq (sigVec a),
   Fractional a,
   Show (sigVec a),
   Show node,
   UV.Unbox a,
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage vec a,
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Storage sigVec (Maybe (Result a)),
   SV.Singleton sigVec,
   SV.Lookup sigVec,
   SV.Len (sigVec a),
   SV.FromList sigVec,
   SV.Find sigVec,
   SV.Convert vec UV.Vector,
   SV.Convert sigVec UV.Vector,
   SV.Convert sigVec [],
   SV.Convert sigVec sigVec,
   Arith.ZeroTestable a,
   Arith.ZeroTestable (Sweep vec a),
   Arith.Product (Sweep vec a),
   Node.C node,
   Sweep.SweepVector vec a,
   Sweep.SweepMap Sweep vec a a,
   Sweep.SweepClass Sweep vec a) => 
  One.OptimalEnvParams node [] Sweep vec sigVec a ->
  Map.Map Idx.State (Map.Map [a] (Maybe (a, a, EnvResult node a))) ->
  Map.Map Idx.State (One.StateForcing a) ->
  [StateLoopItem node a (Type.Simulation node Sweep vec sigVec a)]
stateIteration params optimalObjectivePerState stateForceIn  = 
  go 0  stateForceIn initialSteps initialResults 
  where initialSteps = Map.map (\_ -> seed) stateForceIn
        seed = One.stateForcingSeed params
        fsys sf = NonIO.optimiseStatesAndSimulate params sf optimalObjectivePerState
        initialResults = fsys stateForceIn
        (One.StateTimeThreshold thr) = One.stateTimeThreshold params
        
        accessTimes res =
          Map.map f $ getStateTimes $ Type.stateFlowGraph res
           where f (Determined (Data x)) = x
                 f Undetermined  = error "State Time undetermined"
         
        go cnt force step res  =
          StateLoopItem cnt force step (accessTimes res) res
            : go (cnt+1) force1 step1 res1 
          where
            force1 = Map.fromList $ zipWith g (Map.toList force) (Map.toList step)      
            g (idx1,x) (idx2,y) =
              if idx1 /= idx2 then error msg else (idx1, x~+y)
            res1 = fsys force1 
            times = accessTimes res
            times1 = accessTimes res1
            
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
iterateInnerLoopWhile ::(Eq (sigVec a),
                      Fractional a,
                      Ord a,
                      Show (sigVec a),
                      Show node,
                      Show a,
                      UV.Unbox a,
                      SV.Zipper sigVec,
                      SV.Walker sigVec,
                      SV.Storage sigVec (Maybe (Result a)),
                      SV.Storage sigVec a,
                      SV.Storage sigVec Bool , 
                      SV.Singleton sigVec,
                      SV.Lookup sigVec,
                      SV.Len (sigVec a),
                      SV.FromList sigVec,
                      SV.Find sigVec,
                      SV.Convert sigVec sigVec,
                      SV.Convert sigVec [],
                      SV.Convert sigVec vec,
                      Arith.ZeroTestable a,
                      Arith.Product a, 
                      Node.C node, Arith.Constant a
                      )=>
  One.OptimalEnvParams node [] Sweep vec sigVec a ->
  EnvResult node (Sweep vec a) ->
  [InnerLoopItem node a (Type.Optimisation node Sweep vec sigVec a)]
  
iterateInnerLoopWhile  params stateFlowGraphOpt = 
  takeWhile f $ iterateInnerLoop params reqsRec stateFlowGraphOpt
   where (One.MaxInnerLoopIterations maxCnt) = One.maxInnerLoopIterations params  
         stateFlowGraphOpt = One.stateFlowGraphOpt params
         reqsRec = One.reqsRec params
         f x = ilStep x < maxCnt
-}             

iterateInnerLoop ::
  (Ord a, Show a, Show node, UV.Unbox a, Eq a,
   Eq (sigVec a),
   Fractional a,
   Show (sigVec a),
   SV.Zipper sigVec,
   SV.Convert sigVec UV.Vector,
   SV.Walker sigVec,
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Storage sigVec (Maybe (Result a)), 
   SV.Singleton sigVec,
   SV.Lookup sigVec,
   SV.Len (sigVec a),
   SV.FromList sigVec,
   SV.Find sigVec,
   SV.Convert sigVec vec,
   SV.Convert sigVec [],
   SV.Convert sigVec sigVec,
   Arith.ZeroTestable a,
   Arith.Product a, 
   Node.C node, Arith.Constant a) =>
  One.OptimalEnvParams node [] Sweep UV.Vector sigVec a ->
  Map.Map Idx.State (Map.Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Map.Map node (One.SocDrive a) ->
  Map.Map Idx.State (One.StateForcing a) ->
  [InnerLoopItem node a (Type.Optimisation node Sweep UV.Vector sigVec a)]
iterateInnerLoop params perStateSweep balForceIn stateForceIn = 
  go 0 balForceIn stateForceIn
  where
   
    go cnt balForce statForce =
      InnerLoopItem cnt balForce1 bal statForce1 sta balLoop statLoop
        : go (cnt+1) balForce1 statForce1
      where
        balLoop = balanceIteration params perStateSweep balForce statForce
                    
        balForce1 = iterateBalanceUntil params balLoop            
                    
        statLoop = undefined -- stateIteration params balForce1 statForce
                    
        (statForce1,bal,sta) = undefined -- iterateStateUntil checkStates balLoop


{-
iterateEtaWhile :: 
  (Ord a,
   Show a,
   Show node,
   UV.Unbox a,
   Eq (sigVec a),
   Fractional a,
   Show (sigVec a),
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec (Maybe (Result a)),
   SV.Storage sigVec a,
   SV.Storage sigVec Bool,
   SV.Singleton sigVec,
   SV.Lookup sigVec,
   SV.Len (sigVec a),
   SV.FromList sigVec,
   SV.Find sigVec,
   SV.Convert sigVec sigVec,
   SV.Convert sigVec [],
   SV.Convert sigVec UV.Vector,
   Arith.ZeroTestable a,
   Arith.ZeroTestable (Sweep UV.Vector a),
   Arith.Product (Sweep UV.Vector a),
   Arith.Constant a,
   Node.C node,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepMap Sweep UV.Vector a Bool,
   Sweep.SweepMap Sweep UV.Vector a a,
   Sweep.SweepClass Sweep UV.Vector Bool,
   Sweep.SweepClass Sweep UV.Vector a) =>
  One.OptimalEnvParams node [] Sweep UV.Vector sigVec a ->
  [EtaLoopItem node Sweep UV.Vector a (Type.Optimisation node Sweep UV.Vector sigVec a)]
-}
iterateEtaWhile params = go 0  $ One.stateFlowGraphOpt params 
   where (One.MaxEtaIterations maxCnt) = One.maxEtaIterations params
         balf =  One.initialBattForcing params
         graphStates = StateQty.states $ One.stateFlowGraphOpt params
         statf = Map.map (const $ One.stateForcingSeed params) graphStates
         go cnt sfg = EtaLoopItem cnt sfg sfg1 res : go (cnt+1) sfg1
           where
            sweep = Base.perStateSweep params sfg
            res = iterateInnerLoop params sweep balf statf
            sfg1 = Type.stateFlowGraphSweep
                   $ Type.simulation
                   $ sResult
                   $ vhead "iterateEtaUntil 2"
                   $ stateLoop
                   $ vhead "iterateEtaUntil 1" res


{-
etaIteration ::
  (EnvResult node (sweep vec a) -> EtaLoopItem node a z) ->
  EnvResult node (sweep vec a) ->
  [EtaLoopItem node a z]
etaIteration ib = 
  let go inEnv =
        let oli = ib inEnv
            outEnv = Type.stateFlowGraphSweep $ Type.simulation $ optimisation oli
        in oli : go outEnv
  in go
-}

-------------------------------- OutPut Below -----------------------------------
{-

iterateLoops ::
  One.MaxEtaIterations ->
  (Int -> EtaLoopItem node a z -> Maybe b) ->
  (Int ->
    Int -> 
    BalanceLoopItem node a (Type.Optimisation node sweep vec a, Map.Map node a) ->
    Maybe b) ->
  EtaLoop node a z ->
  [Maybe b]
iterateLoops (One.MaxEtaIterations cnt) olif ilf (EtaLoop ol) =
  concat $ zipWith g [0..] ols
  where ols = take cnt ol
        g n o = zipWith (ilf n) [0..] (f o) ++ [olif n o]
        f (EtaLoopItem nos _ _ _ _ (BalanceLoop il)) = take nos il


showEtaLoopItem ::
  (UV.Unbox a, Arith.Product a,
   PrintfArg a,Show node, Show a,
   Node.C node, Sweep.SweepClass sweep vec a) =>
  Int -> EtaLoopItem node a z -> Maybe String
showEtaLoopItem _ (EtaLoopItem nos ss f bal opt _il) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "ETA-Loop: " ++ (show numOfSt) ++ " " ++ (show nos) ++ " " ++ (show f)++ " " ++ (show bal)++ " " ++ (show $ eta opt)++ " " ++(show ss) 
  --   printf "%8d%8d%50s%50s%50s%50s\n" numOfSt nos (show f) (show bal) (eta opt)  (show ss)
  -- "%8d%8d%24e%24e%24e%24e\n"
  -- (show numOfSt) ++ (show nos) ++ (show f)++ (show bal)++ (eta opt)++(show ss)   

showBalanceLoopItem ::
  (UV.Unbox a, Arith.Product a, Show a, Show node, 
   Node.C node, Sweep.SweepClass sweep vec a, PrintfArg a) =>
  Int -> Int ->
  BalanceLoopItem node a (Type.Optimisation node sweep vec a, Map.Map node a) ->
  Maybe String
showBalanceLoopItem _olcnt ilcnt (BalanceLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "Balance-Loop: " ++ (show numOfSt)++" "++(show ilcnt) ++" "++(show f) ++" "++(show bal) ++" "++(show $ eta opt) ++" "++(show st)
     --printf "%8d%8d%50s%50s%50s%50s" numOfSt ilcnt (show f) (show bal) (eta opt) (show st)
     -- $ (show numOfSt) ++(show ilcnt) ++(show f) ++(show bal) ++(eta opt) ++(show st)


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

showEtaLoop ::
  (PrintfArg a, UV.Unbox a, Arith.Product a, Show a,
--   PrintfArg (Map.Map node a), PrintfArg (Map.Map node (One.SocDrive a)),
   Node.C node, Sweep.SweepClass sweep vec a, Show node) =>
   One.MaxEtaIterations ->
   EtaLoop node a z -> [String]

showEtaLoop cnt ol =
  catMaybes $ iterateLoops cnt showEtaLoopItem showBalanceLoopItem ol



printEtaLoopItem ::
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node f sweep vec a ->
  Int -> EtaLoopItem node a z -> Maybe (IO ())
printEtaLoopItem _params olcnt (EtaLoopItem _ _ _ _ opt _il) =
  Just $ do
    let -- dir = printf "outer-loop-%6.6d" olcnt
        --stoPos = TopoIdx.Position System.Water System.Network
        --gasPos = TopoIdx.Position System.Gas System.LocalNetwork

    putStrLn (printf "Loop %6.6d" olcnt)

    concurrentlyMany_ [
      --ModPlot.maxEtaPerState ModPlot.gpXTerm opt,
      ModPlot.simulationGraphs ModPlot.dotXTerm opt ]
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

printStateLoopItem ::
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node f sweep vec a ->
  Int -> Int ->
  StateLoopItem node a z ->
  Maybe (IO ())
printStateLoopItem _params _olcnt _ilcnt (StateLoopItem _ _ (_opt, _)) =
  Nothing

printBalanceLoopItem ::
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node f sweep vec a ->
  Int -> Int ->
  BalanceLoopItem node a (Type.Optimisation node sweep vec a, Map.Map node a) ->
  Maybe (IO ())
printBalanceLoopItem _params _olcnt _ilcnt (BalanceLoopItem _ _ (_opt, _)) =
  Nothing
{-
  Just $ do
    let dir = printf "outer-loop-%6.6d" olcnt
        stoPos = TopoIdx.Position System.Water System.Network

    ModPlot.maxEta (ModPlot.gpPNG dir ilcnt) opt
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
    -- ModPlot.maxStateContour (ModPlot.gpPNG dir ilcnt) opt
-}


printEtaLoop ::
  (Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node f sweep vec a ->
  EtaLoop node a z -> [IO ()]
printEtaLoop params ol =
  catMaybes $ iterateLoops (One.maxEtaIterations params)
                            (printEtaLoopItem params) 
  (printBalanceLoopItem params) ol



-}
