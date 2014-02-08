{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modules.Optimisation.Loop where


import qualified Modules.System as System; import Modules.System (Node)
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

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data(Data))

import EFA.Utility.List (vhead, vlast)
import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Maybe (catMaybes)

import Text.Printf (printf, PrintfArg)

data BalanceLoopItem a z =
  BalanceLoopItem { xfzc :: a, 
                    stepfzc :: a, 
                    yfzc :: z}

newtype BalanceLoop a z =
  BalanceLoop { unBalanceLoop :: [BalanceLoopItem a z] }
{-
data StateLoopItem a z = { xfzc :: a, 
                    stepfzc :: a, 
                    yfzc :: z}

newtype StateLoopItem a z =
  StateLoop { unStateLoop :: [StateLoopItem a z] }
-}
data EtaLoopItem node sweep vec a = EtaLoopItem {
  numberOfSteps :: Int,
  stepSize :: a,
  forcing :: a,
  balance :: a,
  optimisation :: Type.Optimisation node sweep vec a,
  innerLoop :: BalanceLoop a (Type.Optimisation node sweep vec a, a) }

newtype EtaLoop node sweep vec a =
  EtaLoop { unEtaLoop :: [EtaLoopItem node sweep vec a] }

uniqueBalanceLoopX ::
  Eq a => EtaLoop node sweep vec a -> EtaLoop node sweep vec a
uniqueBalanceLoopX (EtaLoop ol) = EtaLoop (map f ol)
  where f oli = oli { innerLoop = BalanceLoop $ map (vlast "uniqueBalanceLoopX")
                                            $ List.groupBy g fzcs }
          where BalanceLoop fzcs = innerLoop oli
        g a b = snd (yfzc a) == snd (yfzc b)

-- TODO:: setChargeDrive Kante rausziehen !!! -- Ein Storage Map erzeugen und Werte übergeben !!
setChargeDrive ::
  (Ord node) =>
  One.OptimalEnvParams node list sweep vec a ->
  (node, a) ->
  One.OptimalEnvParams node list sweep vec a
setChargeDrive params newForcing =
  params { One.forcingPerNode =
             Map.fromList [fmap One.ChargeDrive newForcing] }

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
   Arith.Constant t) =>
  One.OptimalEnvParams Node list sweep vec t ->
  (One.OptimalEnvParams Node list sweep vec t -> z) ->
  (z -> t) ->
  t ->
  t ->
  [BalanceLoopItem t z]
balanceIteration params f accessf xstart step =
  case Arith.sign $ accessf ystart of
       Negative -> go xstart ystart step
       Zero     -> [BalanceLoopItem xstart step ystart]
       Positive -> go xstart ystart (Arith.negate step)
  where
    ystart = f $ setChargeDrive params (System.Water, xstart)
    _3 = Arith.fromInteger 3
    _2 = Arith.fromInteger 2

    go x0 y0 st =
      let x1 = x0 ~+ st
          y1 = f $ setChargeDrive params (System.Water, x1)


          newStepSize = (Arith.abs st) ~/(Arith.one ~+ (Arith.abs $ accessf y0) ~/ (Arith.abs $ accessf y1))
          
      -- VARIANT A: Simple
      in if False then BalanceLoopItem x0 st y0 :
           case (Arith.sign $ accessf y0, Arith.sign $ accessf y1) of
                (Negative, Negative) -> go x1 y1 ((Arith.abs st) ~* _2)
                (Positive, Positive) -> go x1 y1 (Arith.negate $ (Arith.abs st) ~* _2)
                (Negative, Positive) -> go x1 y1 (Arith.negate $ (Arith.abs st) ~/ _3)
                (Positive, Negative) -> go x1 y1 ((Arith.abs st) ~/ _3)
                (Zero, _)  -> []
                (_, Zero)  -> []
                
        else -- VARIANT B: Estimating zero crossing position
           BalanceLoopItem x0 st y0 :
           case (Arith.sign $ accessf y0, Arith.sign $ accessf y1) of
                -- Crossing not found increase step 
                (Negative, Negative) -> go x1 y1 ((Arith.abs st) ~* _2)
                (Positive, Positive) -> go x1 y1 (Arith.negate $ (Arith.abs st) ~* _2)
                -- Zero crossing occured, step into the middle  
                (Negative, Positive) -> go x1 y1 (Arith.negate $ newStepSize)
                (Positive, Negative) -> go x1 y1 (Arith.abs newStepSize)
                (Zero, _)  -> []
                (_, Zero)  -> []
                
-- | Find a solution where all states occur in the simlation signals at minimum state forcing, measurement unit ist state duration
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
                  (One.StateForcings f -> res) -> (res -> Idx.State -> t) -> 
                  One.StateForcings f -> t -> One.StateForcing f -> 
                  [([One.StateForcing f], [One.StateForcing f], res)] 
stateIteration fsys accessf initialStateForcings thr minStep = go (Map.elems initialStateForcings) initialResult initialSteps
  where initialSteps = map (\_ -> minStep) $ Map.elems initialStateForcings
        stateIndices =  Map.keys initialStateForcings
        initialResult= fsys initialStateForcings
        _3 = Arith.fromInteger 3        
        _2 = Arith.fromInteger 2
                
        go xs0 y0 ss0 = (xs0,ss0,y0):go xs1 y1 ss1
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
  (z -> t) ->
  Int -> t ->
  [BalanceLoopItem t z] ->
  (Int, BalanceLoopItem t z)
iterateBalanceUntil accessf maxStepCnt eps ws =
  vhead "interateUntil" $ dropWhile p (zip [0..] ws)
  where p (n, fzc) = n < maxStepCnt-1 && eps < Arith.abs (accessf $ yfzc fzc)

{-
iterateState :: 
iterateState params reqsRec stateFlowGraphOpt initStateForcing = 
  let perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      net2wat = TopoIdx.ppos System.Water System.Network
      initStateForcing = One.zeroStateForcing stateFlowGraphOpt
-}

iterate ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  EnvResult Node (Sweep UV.Vector Double) ->
  EtaLoopItem Node Sweep UV.Vector Double
iterate params reqsRec stateFlowGraphOpt =
  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      -- Kante rausziehen
      net2wat = TopoIdx.ppos System.Water System.Network
      
      initStateForcing = One.zeroStateForcing stateFlowGraphOpt
      initBattForcing = -1
      
      g pars = (opt2, bl)
        where opt2 = NonIO.optimiseAndSimulate pars initStateForcing reqsRec perStateSweep
              rec = Record.partIntegrate (Type.signals $ Type.simulation opt2)
              Sig.TC (Data bl) = Sig.neg $ Sig.sum $ Record.getSig rec net2wat

      fzc = balanceIteration params g snd initBattForcing 1
      
     -- Achtung !!! -> params sind alt !!! 
      h stateForcing  = (opt2, bl)
        where opt2 = NonIO.optimiseAndSimulate params stateForcing reqsRec perStateSweep
              rec = Record.partIntegrate (Type.signals $ Type.simulation opt2)
              Sig.TC (Data bl) = Sig.neg $ Sig.sum $ Record.getSig rec net2wat
              
      access res idx = f $ getStateTime idx  $ Type.stateFlowGraph $Type.simulation $ fst res
        where f (Determined (Data x)) = x
              f Undetermined  = error "State Time undetermined"
              
      fzz = stateIteration h access
            initStateForcing (1::Double) (One.StateForcing 0.001)

      (numOfSteps, BalanceLoopItem force sSize (opt, bal)) =
        iterateBalanceUntil snd 100 (Arith.fromRational 0.1) fzc
        

  in EtaLoopItem numOfSteps sSize force bal opt (BalanceLoop fzc)

outerLoop ::
  (EnvResult node (sweep vec a) -> EtaLoopItem node sweep vec a) ->
  EnvResult node (sweep vec a) ->
  EtaLoop node sweep vec a
outerLoop ib = 
  let go inEnv =
        let oli = ib inEnv
            outEnv = Type.stateFlowGraphSweep $ Type.simulation $ optimisation oli
        in oli : go outEnv
  in EtaLoop . go

iterateLoops ::
  Int ->
  (Int -> EtaLoopItem node sweep vec a -> Maybe b) ->
  (Int ->
    Int -> 
    BalanceLoopItem a (Type.Optimisation node sweep vec a, a) ->
    Maybe b) ->
  EtaLoop node sweep vec a ->
  [Maybe b]
iterateLoops cnt olif ilf (EtaLoop ol) =
  concat $ zipWith g [0..] ols
  where ols = take cnt ol
        f (EtaLoopItem nos _ _ _ _ (BalanceLoop il)) = take nos il
        g n o = zipWith (ilf n) [0..] (f o) ++ [olif n o]


showEtaLoopItem ::
  (UV.Unbox a, Arith.Product a,
   PrintfArg a,
   Node.C node, Sweep.SweepClass sweep UV.Vector a) =>
  Int -> EtaLoopItem node sweep UV.Vector a -> Maybe String
showEtaLoopItem _ (EtaLoopItem nos ss f bal opt _il) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ printf "%8d%8d%24e%24e%24e%24e\n" numOfSt nos f bal (eta opt) ss

showBalanceLoopItem ::
  (UV.Unbox a, Arith.Product a, 
   Node.C node, Sweep.SweepClass sweep UV.Vector a, PrintfArg a) =>
  Int -> Int ->
  BalanceLoopItem a (Type.Optimisation node sweep UV.Vector a, a) ->
  Maybe String
showBalanceLoopItem _olcnt ilcnt (BalanceLoopItem f st (opt, bal)) =
  let numOfSt = Map.size $ StateQty.states $ Type.stateFlowGraph $ Type.simulation opt
  in Just $ printf "%8d%8d%24e%24e%24e%24e" numOfSt ilcnt f bal (eta opt) st


showEtaLoop ::
  (PrintfArg a, UV.Unbox a, Arith.Product a, 
   Node.C node, Sweep.SweepClass sweep UV.Vector a, Show node) =>
   Int ->
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

printBalanceLoopItem ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep vec Double ->
  Int -> Int ->
  BalanceLoopItem Double (Type.Optimisation Node sweep UV.Vector Double, Double) ->
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
  Int ->
  EtaLoop Node sweep UV.Vector Double -> [IO ()]
printEtaLoop params cnt ol =
  catMaybes $ iterateLoops cnt (printEtaLoopItem params) (printBalanceLoopItem params) ol




