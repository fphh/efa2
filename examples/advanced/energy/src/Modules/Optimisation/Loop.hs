{-# LANGUAGE FlexibleContexts #-}


module Modules.Optimisation.Loop where


import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Plot as ModPlot
import qualified Modules.Types as Types
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO

import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)
import qualified EFA.Application.OneStorage as One

import qualified EFA.Graph.Topology.Node as Node

import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative), (~*), (~+), (~-), (~/))
import qualified EFA.Equation.Arithmetic as Arith

import EFA.Equation.Result (Result(Determined))

import qualified EFA.Flow.State.SystemEta as StateEta
-- import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.Part.Index as Idx

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


data FindZeroCrossing a z =
  FindZeroCrossing { xfzc :: a, stepfzc :: a, yfzc :: z }

newtype InnerLoop a z =
  InnerLoop { unInnerLoop :: [FindZeroCrossing a z] }

data OuterLoopItem node sweep vec a = OuterLoopItem {
  numberOfSteps :: Int,
  stepSize :: a,
  forcing :: a,
  balance :: a,
  optimisation :: Types.Optimisation node sweep vec a,
  innerLoop :: InnerLoop a (Types.Optimisation node sweep vec a, a) }

newtype OuterLoop node sweep vec a =
  OuterLoop { unOuterLoop :: [OuterLoopItem node sweep vec a] }

uniqueInnerLoopX ::
  Eq a => OuterLoop node sweep vec a -> OuterLoop node sweep vec a
uniqueInnerLoopX (OuterLoop ol) = OuterLoop (map f ol)
  where f oli = oli { innerLoop = InnerLoop $ map (vlast "uniqueInnerLoopX")
                                            $ List.groupBy g fzcs }
          where InnerLoop fzcs = innerLoop oli
        g a b = snd (yfzc a) == snd (yfzc b)

setChargeDrive ::
  One.OptimalEnvParams Node list sweep vec a ->
  a ->
  One.OptimalEnvParams Node list sweep vec a
setChargeDrive params newForcing =
  params { One.forcingPerNode =
             Map.fromList [(System.Water, One.ChargeDrive newForcing)] }

betterFalsePosition ::
  (Ord a, Arith.Constant a) =>
  One.OptimalEnvParams Node list sweep vec a ->
  (One.OptimalEnvParams Node list sweep vec a -> t) ->
  (t -> a) -> a -> a -> [(a, t)]
betterFalsePosition params f accessf a1 a2 =
  go a1 a2 (f $ setChargeDrive params a1) (f $ setChargeDrive params a2)
  where go x1 x2 f1 f2 =
          let y1 = accessf f1
              y2 = accessf f2
              z = x1 ~- y1 ~* (x2 ~- x1) ~/ (y2 ~- y1)
              fz = f $ setChargeDrive params z
              yz = accessf fz
              epsx = Arith.fromRational $ 1e-4
              fnew = f $ setChargeDrive params (y2 ~* y1 ~/ (y2 ~+ yz))
          in (x1, f1) :
               if Arith.abs (x2 ~- x1) < epsx
                then [(z, fz)]
                else if yz ~* y2 < Arith.zero
                        then go x2 z f2 fz
                        else go x1 z fnew fz



eta ::
  (Node.C node, UV.Unbox a,
   Arith.Product (sweep UV.Vector a),
   Sweep.SweepClass sweep UV.Vector a) =>
  Types.Optimisation node sweep UV.Vector a -> a
eta opt =
  case StateEta.etaSys (Types.stateFlowGraph $ Types.simulation opt) of
       Determined e -> UV.head (Sweep.fromSweep e)
       _ -> error "Main.iterateBalanceIO"

findZeroCrossing ::
  (Arith.Sum t, Ord t, Arith.Product t, Arith.Constant t,
   Ord s, Arith.Constant s) =>
  One.OptimalEnvParams Node list sweep vec t ->
  (One.OptimalEnvParams Node list sweep vec t -> z) ->
  (z -> s) ->
  t ->
  t ->
  [FindZeroCrossing t z]
findZeroCrossing params f accessf xstart step =
  case Arith.sign $ accessf ystart of
       Negative -> go xstart ystart step
       Zero     -> [FindZeroCrossing xstart step ystart]
       Positive -> go xstart ystart (Arith.negate step)
  where
    ystart = f $ setChargeDrive params xstart

    go x0 y0 st =
      let x1 = x0 ~+ st
          y1 = f $ setChargeDrive params x1

          _2 = Arith.fromInteger 4

      in FindZeroCrossing x0 st y0 :
           case (Arith.sign $ accessf y0, Arith.sign $ accessf y1) of
                (Negative, Negative) -> go x1 y1 (st ~* _2)
                (Positive, Positive) -> go x1 y1 (st ~* _2)
                (Negative, Positive) -> go x0 y0 (st ~/ _2)
                (Positive, Negative) -> go x0 y0 (st ~/ _2)
                (Zero, _)  -> []
                (_, Zero)  -> []

iterateUntil ::
  (Arith.Sum t, Ord t) =>
  Int -> t ->
  [FindZeroCrossing t z] ->
  (Int, FindZeroCrossing t z)
iterateUntil maxStepCnt eps ws =
  vhead "interateUntil" $ dropWhile p (zip [0..] ws)
  where p (n, fzc) = n < maxStepCnt-1 && eps < Arith.abs (stepfzc fzc)


iterateBalance ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Types.EnvResult Node (Sweep UV.Vector Double) ->
  OuterLoopItem Node Sweep UV.Vector Double
iterateBalance params reqsRec stateFlowGraphOpt =
  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      net2wat = TopoIdx.ppos System.Water System.Network

      g pars = (opt2, bl)
        where opt2 = NonIO.optimiseAndSimulate pars reqsRec perStateSweep
              rec = Record.partIntegrate (Types.signals $ Types.simulation opt2)
              Sig.TC (Data bl) = Sig.neg $ Sig.sum $ Record.getSig rec net2wat

      fzc = findZeroCrossing params g snd (-1) 1
      (numOfSteps, FindZeroCrossing force sSize (opt, bal)) =
        iterateUntil 100 (1e-6) fzc

  in OuterLoopItem numOfSteps sSize force bal opt (InnerLoop fzc)


outerLoop ::
  (Types.EnvResult node (sweep vec a) -> OuterLoopItem node sweep vec a) ->
  Types.EnvResult node (sweep vec a) ->
  OuterLoop node sweep vec a
outerLoop ib = 
  let go inEnv =
        let oli = ib inEnv
            outEnv = Types.stateFlowGraph $ Types.simulation $ optimisation oli
        in oli : go outEnv
  in OuterLoop . go





iterateLoops ::
  Int ->
  (Int -> OuterLoopItem node sweep vec a -> Maybe b) ->
  (Int ->
    Int -> 
    FindZeroCrossing a (Types.Optimisation node sweep vec a, a) ->
    Maybe b) ->
  OuterLoop node sweep vec a ->
  [Maybe b]
iterateLoops cnt olif ilf (OuterLoop ol) =
  concat $ zipWith g [0..] ols
  where ols = take cnt ol
        f (OuterLoopItem nos _ _ _ _ (InnerLoop il)) = take nos il
        g n o = zipWith (ilf n) [0..] (f o) ++ [olif n o]


showOuterLoopItem ::
  (UV.Unbox a, Arith.Product (sweep UV.Vector a),
   PrintfArg a,
   Node.C node, Sweep.SweepClass sweep UV.Vector a) =>
  Int -> OuterLoopItem node sweep UV.Vector a -> Maybe String
showOuterLoopItem _ (OuterLoopItem nos ss f bal opt _il) =
  --Just $ printf "%6d%24e%24e%24e\n" nos ss f bal
  Just $ printf "%6d%24e%24e%24e%24e\n" nos ss f bal (eta opt)

  -- Just $ printf "%24e%24e\n" f bal



showFindZeroCrossing ::
  (UV.Unbox a, Arith.Product (sweep UV.Vector a),
   Node.C node, Sweep.SweepClass sweep UV.Vector a, PrintfArg a) =>
  Int -> Int ->
  FindZeroCrossing a (Types.Optimisation node sweep UV.Vector a, a) ->
  Maybe String
showFindZeroCrossing _olcnt ilcnt (FindZeroCrossing f st (opt, bal)) =
  Just $ printf "%6d%24e%24e%24e%24e" ilcnt st f bal (eta opt)
  -- Just $ printf "%6d%24e%24e%24e%24e\n" nos ss f bal (eta opt)

  -- Just $ printf "%24e%24e" f bal


showOuterLoop ::
  (PrintfArg a, UV.Unbox a, Arith.Product (sweep UV.Vector a),
   Node.C node, Sweep.SweepClass sweep UV.Vector a, Show node) =>
   Int ->
   OuterLoop node sweep UV.Vector a -> [String]

showOuterLoop cnt ol =
  catMaybes $ iterateLoops cnt showOuterLoopItem showFindZeroCrossing ol



printOuterLoopItem ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep UV.Vector Double ->
  Int -> OuterLoopItem Node sweep UV.Vector Double -> Maybe (IO ())
printOuterLoopItem _params olcnt (OuterLoopItem _ _ _ _ opt _il) =
  Just $ do
    let -- dir = printf "outer-loop-%6.6d" olcnt
        stoPos = TopoIdx.Position System.Water System.Network
        gasPos = TopoIdx.Position System.Gas System.LocalNetwork

    putStrLn (printf "Loop %6.6d" olcnt)

    concurrentlyMany_ [
      ModPlot.simulationGraphs ModPlot.dotXTerm opt,
      ModPlot.simulationSignals ModPlot.gpXTerm opt,
      ModPlot.maxPos stoPos ModPlot.gpXTerm opt,
      ModPlot.maxPos gasPos ModPlot.gpXTerm opt,
      ModPlot.maxState ModPlot.gpXTerm opt,
      ModPlot.maxObj ModPlot.gpXTerm opt ]

{-
    ModPlot.maxPos stoPos (ModPlot.gpPNG dir 0) opt
    ModPlot.maxPos gasPos (ModPlot.gpPNG dir 0) opt
    ModPlot.maxState (ModPlot.gpPNG dir 0) opt
    ModPlot.maxEta (ModPlot.gpPNG dir 0) opt
-}

printFindZeroCrossing ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep vec Double ->
  Int -> Int ->
  FindZeroCrossing Double (Types.Optimisation Node sweep UV.Vector Double, Double) ->
  Maybe (IO ())
printFindZeroCrossing _params _olcnt _ilcnt (FindZeroCrossing _ _ (_opt, _)) =
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


printOuterLoop ::
  (Arith.Product (sweep UV.Vector Double),
   Sweep.SweepClass sweep UV.Vector Double) =>
  One.OptimalEnvParams Node f sweep UV.Vector Double ->
  Int ->
  OuterLoop Node sweep UV.Vector Double -> [IO ()]
printOuterLoop params cnt ol =
  catMaybes $ iterateLoops cnt (printOuterLoopItem params) (printFindZeroCrossing params) ol




{-
forcingSweep  ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Types.EnvResult Node (Sweep UV.Vector Double) ->
  IO ()
forcingSweep params reqsRec stateFlowGraphOpt = do
  let 
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt

      forcings = [-0.085, -0.0849 .. -0.084]
      ps = map (setChargeDrive params) forcings

      f p = do
        let opt = NonIO.optimiseAndSimulate p reqsRec perStateSweep
            forcing = case Map.lookup System.Water (One.forcingPerNode p) of
                           Just (One.ChargeDrive fo) -> fo
                           _ -> error $ "forcingSweep"

            rec = Record.partIntegrate (Types.givenSignals $ Types.simulation opt)

            net2wat = TopoIdx.ppos System.Water System.Network

            Sig.TC (Data balance) =
              Sig.neg $ Sig.sum $ Record.getSig rec net2wat

        putStrLn (show forcing ++ "\t" ++ show balance ++ "\t" ++ show (eta opt))

  mapM_ f ps

-}


{-



iterateBalanceIO ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  Types.EnvResult Node (Sweep UV.Vector Double) ->
  IO (Types.EnvResult Node (Sweep UV.Vector Double))
iterateBalanceIO params reqsRec stateFlowGraphOpt = do

  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt
      net2wat = TopoIdx.ppos System.Water System.Network

      g pars =
        let opt2@(Types.Optimisation _ sim) =
              NonIO.optimiseAndSimulate pars reqsRec perStateSweep

            rec = Record.partIntegrate (Types.signals sim)


            Sig.TC (Data bl) =
              Sig.neg $ Sig.sum $ Record.getSig rec net2wat

        in (opt2, bl)


      fzc = findZeroCrossing params g snd (-1) 1

      lst = iterateUntil (100 :: Int) (1e-6) fzc
      (len, (forcing, _, (opt, bal))) = snd $ lst

  time <- getCurrentTime
  -- print time

  
  -- aeusserer Loop
  --ModPlot.perStateSweep (ModPlot.gpPNG time 0) params opt


  let first = fmap (vhead "iterateBalanceIO" . Sweep.toList)
  Draw.pdf ("tmp/stateFlowAVG/" ++ filename time ++ ".pdf") 
           $ Draw.stateFlowGraph Draw.optionsDefault
           $ StateQty.mapGraph first first stateFlowGraphOpt


  let

      -- innerer Loop
      plot n (fcing, _, (opt2, bl)) = do

        let stoPos = StateIdx.power (Idx.State 0) System.Network System.Water
        -- ModPlot.simulationGraphs (ModPlot.dotXTerm) opt2

        -- ModPlot.simulationGraphs (ModPlot.dotXTerm) opt2
        --ModPlot.simulationGraphs (ModPlot.dotPNG time n) opt2

        --ModPlot.optimalEtas (ModPlot.gpPNG time n) opt2
        --ModPlot.optimalObjs (ModPlot.gpPNG time n) opt2
        --ModPlot.maxEtaPerState (ModPlot.gpPNG time n) opt2
        -- ModPlot.maxPosPerState (ModPlot.gpPNG time n) stoPos opt2
        -- ModPlot.optimalObjectivePerState ModPlot.dotXTerm opt2
        -- ModPlot.simulationSignals (ModPlot.gpXTerm) opt2
        -- ModPlot.givenSignals ModPlot.gpXTerm opt2
        --ModPlot.maxState (ModPlot.gpPNG time n) opt2

        -- putStrLn $ show n ++ "\t" ++ show fcing ++ "\t" ++ show bl ++ "\t" ++ show (eta opt2)

        return ()


  zipWithM_ plot (take len [0 :: Int ..]) fzc
  putStrLn (show len ++ "\t" ++ show forcing ++ "\t" 
                     ++ show bal ++ "\t" ++ show (eta opt))




  return (Types.stateFlowGraph $ Types.simulation opt)


findZeroCrossing ::
  (Arith.Sum t, Ord t, Arith.Product t, Arith.Constant t,
   Ord s, Arith.Constant s) =>
  One.OptimalEnvParams Node list sweep vec t ->
  (One.OptimalEnvParams Node list sweep vec t -> z) ->
  (z -> s) ->
  t ->
  t ->
  [(t, t, z)]
findZeroCrossing params f accessf xstart step =
  case Arith.sign $ accessf ystart of
       Negative -> go xstart ystart step
       Zero     -> [(xstart, step, ystart)]
       Positive -> go xstart ystart (Arith.negate step)
  where
    ystart = f $ setChargeDrive params xstart

    go x0 y0 st =
      let x1 = x0 ~+ st
          y1 = f $ setChargeDrive params x1

          _2 = Arith.fromInteger 4

      in (x0, st, y0) :
           case (Arith.sign $ accessf y0, Arith.sign $ accessf y1) of
                (Negative, Negative) -> go x1 y1 (st ~* _2)
                (Positive, Positive) -> go x1 y1 (st ~* _2)
                (Negative, Positive) -> go x0 y0 (st ~/ _2)
                (Positive, Negative) -> go x0 y0 (st ~/ _2)
                (Zero, _)  -> []
                (_, Zero)  -> []

-}

