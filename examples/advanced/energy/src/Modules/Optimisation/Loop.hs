{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data(Data))

import EFA.Utility.List (vhead, vlast)
import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)

import Text.Printf (printf, PrintfArg,IsChar)

data BalanceLoopItem node a z =
  BalanceLoopItem { xfzc :: Map.Map node (One.SocDrive a), 
                    stepfzc ::  Map.Map node (One.SocDrive a), 
                    yfzc :: z}

newtype BalanceLoop node a z =
  BalanceLoop { unBalanceLoop :: [BalanceLoopItem node a z] }

data StateLoopItem a z = StateLoopItem 
                         { stF  :: [One.StateForcing a],  
                           stFst :: [One.StateForcing a], 
                           stFY :: z }

newtype StateLoop a z =
  StateLoop { unStateLoop :: [StateLoopItem a z] }

data EtaLoopItem node sweep vec a = EtaLoopItem {
  numberOfSteps :: Int,
  stepSize :: Map.Map node (One.SocDrive a),
  forcing :: Map.Map node (One.SocDrive a),
  balance :: Map.Map node a,
  optimisation :: Type.Optimisation node sweep vec a,
  balanceLoop :: BalanceLoop node a (Type.Optimisation node sweep vec a, Map.Map node a) }

newtype EtaLoop node sweep vec a =
  EtaLoop { unEtaLoop :: [EtaLoopItem node sweep vec a] }

uniqueBalanceLoopX ::
  (Eq a, Eq node) => EtaLoop node sweep vec a -> EtaLoop node sweep vec a
uniqueBalanceLoopX (EtaLoop ol) = EtaLoop (map f ol)
  where f oli = oli { balanceLoop = BalanceLoop $ map (vlast "uniqueBalanceLoopX")
                                            $ List.groupBy g fzcs }
          where BalanceLoop fzcs = balanceLoop oli
        g a b = snd (yfzc a) == snd (yfzc b)

{-
-- TODO:: setChargeDrive Kante rausziehen !!! -- Ein Storage Map erzeugen und Werte übergeben !!
setChargeDrive ::
  (Ord node) =>
  One.OptimalEnvParams node list sweep vec a ->
  (node, a) ->
  One.OptimalEnvParams node list sweep vec a
setChargeDrive params newForcing =
  params { One.forcingPerNode =
             Map.fromList [fmap One.ChargeDrive newForcing] }
-}

eta ::
  (Node.C node, UV.Unbox a,
   Arith.Product a) => 
 --  Sweep.SweepClass sweep UV.Vector a) =>
  Type.Optimisation node sweep UV.Vector a -> a
eta opt =
  case StateEta.etaSys (Type.stateFlowGraph $ Type.simulation opt) of
       Determined (Data e) -> e 
       _ -> error "Main.iterateBalanceIO"


balanceIteration ::
  (Arith.Sum t, Ord t, Arith.Product t,
   Arith.Constant t,Ord node) =>
  (Map.Map node (One.SocDrive t) -> z) ->
  (z -> Map.Map node t) ->
  Map.Map node (One.SocDrive t) ->
  Map.Map node (One.SocDrive t) ->
  One.SocDrive t ->
  [BalanceLoopItem node t z]
balanceIteration fsys accessf xsstart step seed = go xsstart resStart step
  where
    resStart = fsys $ xsstart
    go xs0 res0 st0 = BalanceLoopItem xs0 st0 res0 : go xs1 res1 st1
      where xs1 = Map.mapWithKey (\k x0 -> One.setSocDrive $ (One.getSocDrive x0) ~+ 
                                           (One.getSocDrive $ g $ Map.lookup k st0)) xs0
            g (Just x) = x 
            g Nothing = error ("Error in balanceIteration: keys in StorageForcingMap and StepMap differ")  
          
            res1 = fsys $ xs1
            st1 = Map.mapWithKey (\ k s -> One.setSocDrive $ h  k s) st0
            
            h k sp = let 
              y0 = j $ Map.lookup k $ accessf res0 
              y1 = j $ Map.lookup k $ accessf res1 
              st = One.getSocDrive sp
              
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

stateIteration :: (Arith.Constant f,Eq f, Num f,Ord f,Ord t, Num t, Eq t) => 
                  (One.StateForcings f -> res) -> 
                  (res -> Idx.State -> t) -> 
                  One.StateForcings f -> 
                  One.StateTimeThreshold t -> 
                  One.StateForcing f -> 
                  [StateLoopItem f res]
stateIteration fsys accessf initialStateForcings (One.StateTimeThreshold thr) minStep = 
  go (Map.elems initialStateForcings) initialResult initialSteps
  where initialSteps = map (\_ -> minStep) $ Map.elems initialStateForcings
        stateIndices =  Map.keys initialStateForcings
        initialResult= fsys initialStateForcings
        _3 = Arith.fromInteger 3        
        _2 = Arith.fromInteger 2
                
        go xs0 y0 ss0 = StateLoopItem xs0 ss0 y0:go xs1 y1 ss1
          where
           xs1 = zipWith 
                 (\(One.StateForcing x) (One.StateForcing y)-> 
                   One.StateForcing $ x ~+ y) xs0 ss0
           y1 = fsys (Map.fromList $ zip stateIndices xs1)
           ss1 = zipWith3 (changeStep (y0,y1)) stateIndices (zip xs0 xs1) ss0   

        changeStep (y0,y1) idx (One.StateForcing x0,One.StateForcing x1) (One.StateForcing st) = 
          case (eval x0 (accessf y0 idx), eval x1 (accessf y1 idx))  of
            (_, NoForcingNeeded) -> One.StateForcing $ Arith.zero
            (_, CorrectForcing) -> One.StateForcing $ Arith.zero
            (NoForcingNeeded, _) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing minStep)
            (CorrectForcing,_) -> One.StateForcing $ Arith.zero ~+ (One.unpackStateForcing minStep)
            (MoreForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (LessForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~* _2
            (MoreForcingNeeded, LessForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3
            (LessForcingNeeded, MoreForcingNeeded) -> One.StateForcing $ (Arith.abs st) ~/ _3
        
        eval x y = case ( x == 0 && y > 0,y == 0, y < thr ) of
          (True, _, _) -> NoForcingNeeded 
          (False,False,True) -> CorrectForcing
          (False,True,_) -> MoreForcingNeeded
          (False,False,False) -> LessForcingNeeded

   
iterateBalanceUntil ::
  (Arith.Sum t, Ord t, Arith.Constant t) =>
  (z -> Map.Map node t) ->
  One.MaxBalanceIterations -> 
  One.BalanceThreshold t ->
  [BalanceLoopItem node t z] ->
  (Int, BalanceLoopItem node t z)
iterateBalanceUntil accessf (One.MaxBalanceIterations maxStepCnt) 
  (One.BalanceThreshold eps) ws =
  vhead "interateBalanceUntil" $ dropWhile p (zip [0..] ws)
  where p (n, fzc) = n < maxStepCnt-1 && all (\ x -> Arith.abs x > eps ) 
                     (Map.elems $ accessf $ yfzc fzc)

iterateStateUntil ::
  (Arith.Sum t, Ord t, Arith.Constant t) =>
  Int ->
  [StateLoopItem t z] ->
  (Int, StateLoopItem t z)
iterateStateUntil maxStepCnt ws =
  vhead "interateStateUntil" $ dropWhile p (zip [0..] ws)
  where p (n, _) = n < maxStepCnt-1

etaLoop ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  EnvResult Node (Sweep UV.Vector Double) ->
  EtaLoopItem Node Sweep UV.Vector Double
etaLoop params reqsRec stateFlowGraphOpt =
  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      initStateForcing = One.zeroStateForcing stateFlowGraphOpt
      
      g battF stateF = (opt2, blMap)
        where opt2 = NonIO.optimiseAndSimulate params battF stateF reqsRec perStateSweep
              blMap = StateEta.balanceFromRecord (One.storagePositions params) $ 
                      Type.signals $ Type.simulation opt2

      fzc = balanceIteration (flip g initStateForcing) (snd) 
            (One.initialBattForcing params) (One.initialBattForceStep params) 
            (One.balanceForcingSeed params)
      
      _access res idx = f $ getStateTime idx  $ Type.stateFlowGraph $ Type.simulation $ fst res
        where f (Determined (Data x)) = x
              f Undetermined  = error "State Time undetermined"
              
      _fzz = stateIteration (g $ One.initialBattForcing params) access
            initStateForcing (One.stateTimeThreshold params)  (One.stateForcingSeed params)

      (numOfSteps, BalanceLoopItem force sSize (opt, bal)) = 
        iterateBalanceUntil snd (One.maxBalanceIterations params) (One.balanceThreshold params) fzc
        

  in EtaLoopItem numOfSteps sSize force bal opt (BalanceLoop fzc)

etaIteration ::
  (EnvResult node (sweep vec a) -> EtaLoopItem node sweep vec a) ->
  EnvResult node (sweep vec a) ->
  EtaLoop node sweep vec a
etaIteration ib = 
  let go inEnv =
        let oli = ib inEnv
            outEnv = Type.stateFlowGraphSweep $ Type.simulation $ optimisation oli
        in oli : go outEnv
  in EtaLoop . go


-------------------------------- OutPut Below -----------------------------------


iterateLoops ::
  One.MaxEtaIterations ->
  (Int -> EtaLoopItem node sweep vec a -> Maybe b) ->
  (Int ->
    Int -> 
    BalanceLoopItem node a (Type.Optimisation node sweep vec a, Map.Map node a) ->
    Maybe b) ->
  EtaLoop node sweep vec a ->
  [Maybe b]
iterateLoops (One.MaxEtaIterations cnt) olif ilf (EtaLoop ol) =
  concat $ zipWith g [0..] ols
  where ols = take cnt ol
        g n o = zipWith (ilf n) [0..] (f o) ++ [olif n o]
        f (EtaLoopItem nos _ _ _ _ (BalanceLoop il)) = take nos il


showEtaLoopItem ::
  (UV.Unbox a, Arith.Product a,
   PrintfArg a,Show node, Show a,
   Node.C node, Sweep.SweepClass sweep UV.Vector a) =>
  Int -> EtaLoopItem node sweep UV.Vector a -> Maybe String
showEtaLoopItem _ (EtaLoopItem nos ss f bal opt _il) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "ETA-Loop: " ++ (show numOfSt) ++ " " ++ (show nos) ++ " " ++ (show f)++ " " ++ (show bal)++ " " ++ (show $ eta opt)++ " " ++(show ss) 
  --   printf "%8d%8d%50s%50s%50s%50s\n" numOfSt nos (show f) (show bal) (eta opt)  (show ss)
  -- "%8d%8d%24e%24e%24e%24e\n"
  -- (show numOfSt) ++ (show nos) ++ (show f)++ (show bal)++ (eta opt)++(show ss)   

showBalanceLoopItem ::
  (UV.Unbox a, Arith.Product a, Show a, Show node, 
   Node.C node, Sweep.SweepClass sweep UV.Vector a, PrintfArg a) =>
  Int -> Int ->
  BalanceLoopItem node a (Type.Optimisation node sweep UV.Vector a, Map.Map node a) ->
  Maybe String
showBalanceLoopItem _olcnt ilcnt (BalanceLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "Balance-Loop: " ++ (show numOfSt)++" "++(show ilcnt) ++" "++(show f) ++" "++(show bal) ++" "++(show $ eta opt) ++" "++(show st)
     --printf "%8d%8d%50s%50s%50s%50s" numOfSt ilcnt (show f) (show bal) (eta opt) (show st)
     -- $ (show numOfSt) ++(show ilcnt) ++(show f) ++(show bal) ++(eta opt) ++(show st)


showStateLoopItem ::
  (UV.Unbox a, Arith.Product a,Show a, 
   Node.C node, Sweep.SweepClass sweep UV.Vector a, PrintfArg a, 
   IsChar (One.StateForcing a)) =>
  Int -> Int -> Int ->
  StateLoopItem a (Type.Optimisation node sweep UV.Vector a, a) ->
  Maybe String
showStateLoopItem _olcnt _ slcnt (StateLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ "State-Loop: " ++(show numOfSt) ++" "++ (show slcnt) ++" "++ (show f) ++" "++ (show bal) ++" "++ (show (eta opt)) ++" "++ (show st)
  --   printf "%8d%8d%50s%50s%50s%50s" numOfSt slcnt f bal (eta opt) st
  -- $ (show numOfSt) ++ (show slcnt) ++ (show f) ++ (show bal) ++ (show (eta opt)) ++ (show st) 

showEtaLoop ::
  (PrintfArg a, UV.Unbox a, Arith.Product a, Show a,
--   PrintfArg (Map.Map node a), PrintfArg (Map.Map node (One.SocDrive a)),
   Node.C node, Sweep.SweepClass sweep UV.Vector a, Show node) =>
   One.MaxEtaIterations ->
   EtaLoop node sweep UV.Vector a -> [String]

showEtaLoop cnt ol =
  catMaybes $ iterateLoops cnt showEtaLoopItem showBalanceLoopItem ol



printEtaLoopItem ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep UV.Vector Double ->
  Int -> EtaLoopItem Node sweep UV.Vector Double -> Maybe (IO ())
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
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep vec Double ->
  Int -> Int ->
  StateLoopItem Double (Type.Optimisation Node sweep UV.Vector Double, Double) ->
  Maybe (IO ())
printStateLoopItem _params _olcnt _ilcnt (StateLoopItem _ _ (_opt, _)) =
  Nothing

printBalanceLoopItem ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep vec Double ->
  Int -> Int ->
  BalanceLoopItem node Double (Type.Optimisation Node sweep UV.Vector Double, Map.Map Node Double) ->
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
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep UV.Vector Double ->
  EtaLoop Node sweep UV.Vector Double -> [IO ()]
printEtaLoop params ol =
  catMaybes $ iterateLoops (One.maxEtaIterations params)
                            (printEtaLoopItem params) 
  (printBalanceLoopItem params) ol




