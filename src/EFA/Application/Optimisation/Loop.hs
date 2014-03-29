{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}


module EFA.Application.Optimisation.Loop where
--import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified EFA.Application.Optimisation.Base as Base
import qualified EFA.Application.Optimisation.NonIO as NonIO
import qualified EFA.Application.Utility as AppUt

-- import EFA.Equation.Result(Result(Determined))
import qualified EFA.Application.Type as Type
import EFA.Application.Type (EnvResult)
import qualified EFA.Application.Optimisation.Balance as Balance
import qualified EFA.Application.Optimisation.Params as Params
--import qualified EFA.Application.Optimisation.Sweep as Sweep
import EFA.Application.Optimisation.Sweep (Sweep)

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
import qualified EFA.Application.Flow.State.SystemEta as StateEta
--import qualified EFA.Flow.State.Quantity as StateQty
--import qualified EFA.Flow.State as FlowState
--import qualified EFA.Report.FormatValue as FormatValue

-- import qualified EFA.Flow.State.Index as StateIdx
--import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as Idx

--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Vector as SV
--import EFA.Signal.Data (Data(Data)) --, Nil, Apply)

--import EFA.Utility.List (vlast, vhead)
--import EFA.Utility.Async (concurrentlyMany_)

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

--import EFA.Utility.Trace (mytrace)

--import Debug.Trace

type Counter = Int

data BalanceLoopItem node a z =
  BalanceLoopItem {
    bForcing :: Balance.Forcing node a,
    bFStep :: Balance.ForcingStep node a,
    balance :: Balance.Balance node a,
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
  Params.Optimisation node list sweep vec a ->
  Balance.Balance node a ->
  Bool
checkBalance optParams bal =
  Map.foldl' (\acc v -> acc && Arith.abs v <= bt) True bal
  where bt = Params.unBalanceThreshold (Params.balanceThreshold optParams)

checkBalanceSingle ::
  (Ord a, Arith.Sum a, Ord node, Show node) =>
  Params.Optimisation node list sweep vec a ->
  Balance.Balance node a ->
  node ->
  Bool
checkBalanceSingle optParams bal sto =
  Arith.abs x <= Params.unBalanceThreshold (Params.balanceThreshold optParams)
  where x = Balance.getStorageBalance "checkBalanceSingle" bal sto

-- | Rate Balance Deviation by sum of Standard Deviation and Overall Sum
balanceDeviation ::
  (Arith.Product a,
   Ord a,
   Arith.Constant a,
   Arith.Sum a) =>
  Balance.Balance node a -> a
balanceDeviation m =
  Map.foldl (\acc x -> acc ~+ Arith.square x) Arith.zero m
  ~+
  Arith.abs (Map.foldl (~+) Arith.zero m)

-----------------------------------------------------------------

cond :: Int -> (a -> Bool) -> [a] -> [a]
cond n p =  take n . UtList.takeUntil p

condEta :: Int -> (a -> Bool) -> [a] -> [a]
condEta n p = take n . UtList.takeUntil p -- takeWhile (not . p)

-----------------------------------------------------------------

balanceIterationInit ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  Balance.Forcing node a ->
  Balance.ForcingStep node a ->
  ( Balance.Forcing node a,
    Balance.ForcingStep node a,
    [BalanceLoopItem node a z] )
balanceIterationInit optParams fsys accessf balForceIn balStepsIn =
  (balForceIn, balStepsIn, oioas)
  where oioas = oneIterationOfAllStorages optParams fsys accessf balForceIn balStepsIn

balanceIterationAlgorithm ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  ( Balance.Forcing node a,
    Balance.ForcingStep node a,
    [BalanceLoopItem node a z]) ->
  ( Balance.Forcing node a,
    Balance.ForcingStep node a,
    [BalanceLoopItem node a z] )
balanceIterationAlgorithm optParams  fsys accessf (forcing, stepping, as) =
  (forcing1, stepping1, oioas)
  where oioas = oneIterationOfAllStorages optParams fsys accessf forcing stepping
        forcing1 = bForcing lastElem
        stepping1 = bFStep lastElem
        lastElem = AppUt.ifNull (error "balanceIteration: empty list") last as

balanceIterationCondition ::
  Params.Optimisation node [] Sweep UV.Vector a ->
  (u, v, [BalanceLoopItem node a z]) ->
  (u, v, [BalanceLoopItem node a z])
balanceIterationCondition optParams (u, v, xs) = (u, v, cond maxBICnt bip xs)
  where maxBICnt = Params.unMaxBalanceIterations $ Params.maxBalanceIterations optParams
        bip _ = False

balanceIteration ::
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a)->
  Balance.Forcing node a ->
  Balance.ForcingStep node a ->
  [BalanceLoopItem node a z]
balanceIteration optParams fsys accessf balForceIn balStepsIn =
  concat $ map thd3 $ iterate go bii
  where bii = balanceIterationCondition optParams
              $ balanceIterationInit optParams fsys accessf balForceIn balStepsIn
        go = balanceIterationCondition optParams
             . balanceIterationAlgorithm optParams fsys accessf


-----------------------------------------------------------------

iterateOneStorageInit ::
  (Ord a2, Ord node, Show a2, Show node, Arith.Constant a2) =>
  (Balance.Forcing node a2 -> z) ->
  (z -> Balance.Balance node a2) ->
  Balance.Forcing node a2 ->
  Balance.ForcingStep node a2 ->
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
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  node ->
  ( (Maybe (Balance.SocDrive a, a),
    Maybe (Balance.SocDrive a, a)),
    BalanceLoopItem node a t) ->
  ( (Maybe (Balance.SocDrive a, a),
    Maybe (Balance.SocDrive a, a)),
    BalanceLoopItem node a z)
iterateOneStorageAlgorithm fsys accessf sto
                           (bestPair, BalanceLoopItem force step _ _) =
  (bestPair1, BalanceLoopItem force1 step1 bal1 res1)
  where force1 = Balance.addForcingStep force step sto
        res1 = fsys force1
        bal1 = accessf res1
        bestPair1 = Balance.rememberBestBalanceForcing bestPair (force1, bal1) sto
        step1 = calculateNextBalanceStep bal1 bestPair1 step sto

iterateOneStorageCondition ::
  Params.Optimisation node [] Sweep UV.Vector a ->
  [BalanceLoopItem node a z] ->
  [BalanceLoopItem node a z]
iterateOneStorageCondition optParams xs =
  cond cnt p xs
  where cnt = 100
        p _ = False

iterateOneStorage ::
  (Ord a, Arith.Constant a,Ord node, Show node, Show a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a)->
  Balance.Forcing node a ->
  Balance.ForcingStep node a ->
  node ->
  [BalanceLoopItem node a z]
iterateOneStorage optParams fsys accessf forcingIn steppingIn sto =
  iterateOneStorageCondition optParams $ map snd $ iterate go iosi
  where iosi = iterateOneStorageInit fsys accessf forcingIn steppingIn sto
        go = iterateOneStorageAlgorithm fsys accessf sto



oneIterationOfAllStorages ::
  (Ord a, Ord node, Show node, Show a, Arith.Constant a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  Balance.Forcing node a ->
  Balance.ForcingStep node a ->
  [BalanceLoopItem node a z]
oneIterationOfAllStorages optParams fsys accessf force steps =
  Map.foldlWithKey f [] (Balance.unForcingMap force)
  where f acc sto _ = iterateOneStorage optParams fsys accessf force steps sto ++ acc


-----------------------------------------------------------------


calculateNextBalanceStep ::
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, Show a,
   Ord node, Show node) =>
  Balance.Balance node a ->
  (Maybe (Balance.SocDrive a,a), Maybe (Balance.SocDrive a,a)) ->
  (Balance.ForcingStep node a) ->
  node ->
  (Balance.ForcingStep node a)
calculateNextBalanceStep balMap bestPair stepMap sto =
  Balance.updateForcingStep stepMap sto $ Balance.setSocDrive step1
  where
    bal = Balance.getStorageBalance "calculateNextBalanceStep" balMap sto
    step = Balance.getStorageForcingStep "calculateNextBalanceStep" stepMap sto
    fact = Arith.fromRational 2.0
    divi = Arith.fromRational 1.7
    intervall = Balance.getForcingIntervall bestPair


    step1 =
      case (intervall, Arith.sign bal) of

           -- Zero Crossing didn't occur so far -- increase step to search faster
           (Nothing, Negative) -> Arith.abs $ (Balance.getSocDrive step) ~* fact
           (Nothing, Positive) -> Arith.negate $ (Arith.abs $ Balance.getSocDrive step) ~* fact

           -- The Zero Crossing is contained in the intervall
           -- defined by bestPair - step just a little over the middle
           (Just int, Negative) -> (Balance.getSocDrive int) ~/ divi
           (Just int, Positive) -> (Arith.negate $ Balance.getSocDrive  int) ~/ divi
           (_, Zero)  -> Arith.zero

-----------------------------------------------------------------
iterateEtaWhileInit ::
  (RealFloat a, Show a, UV.Unbox a, Arith.ZeroTestable a,
   Arith.Constant a,Ord node,Show node, Node.C node) =>
  Params.System node a ->
  Params.Optimisation node [] Sweep UV.Vector a ->
  Params.Simulation node [] a ->
  EnvResult node (Sweep UV.Vector a) ->
  Balance.StateForcing ->
  -- (Balance.Forcing node a,
         EtaLoopItem node Sweep UV.Vector a
           (Type.SignalBasedOptimisation node Sweep UV.Vector a [] b [] c [] a) -- )
iterateEtaWhileInit sysParams optParams simParams sfgIn statForcing =
  EtaLoopItem sfgIn initSwp initRes

  where initBalF = Params.initialBattForcing optParams
        balStepsIn = Params.initialBattForceStep optParams

        accessf x =
          StateEta.balanceFromRecord
            (Params.storagePositions sysParams)
            (Type.signals (Type.simulation x))


        initFsys balanceForcing =
          NonIO.optimiseAndSimulateSignalBased
            sysParams optParams simParams
            balanceForcing statForcing initSwp initIdxConvMap

        initIdxConvMap =
          AppUt.indexConversionMap (Params.systemTopology sysParams) sfgIn


        initSwp = Base.perStateSweep sysParams optParams sfgIn
        initRes = balanceIteration optParams initFsys accessf initBalF balStepsIn

iterateEtaWhileAlgorithm ::
  (RealFloat a, Show a, UV.Unbox a, Arith.ZeroTestable a, Arith.Constant a, Show node,Node.C node) =>
  Params.System node a ->
  Params.Optimisation node [] Sweep UV.Vector a ->
  Params.Simulation node [] a ->
  Balance.StateForcing ->
  EtaLoopItem node Sweep UV.Vector a
    (Type.SignalBasedOptimisation
      node Sweep UV.Vector a intVec b1 simVec c1 efaVec d) ->
  EtaLoopItem node Sweep UV.Vector a
    (Type.SignalBasedOptimisation
      node Sweep UV.Vector a [] b [] c [] a)
iterateEtaWhileAlgorithm sysParams optParams simParams stateForcing
                         (EtaLoopItem sfg _ res) =
  EtaLoopItem sfg1 swp1 res1
  where sfg1 = Type.stateFlowGraphSweep (bResult lastElem)
        swp1 = Base.perStateSweep sysParams optParams sfg

        fsys balanceForcing =
          NonIO.optimiseAndSimulateSignalBased
            sysParams optParams simParams
            balanceForcing stateForcing swp1 idxConvMap

        idxConvMap = AppUt.indexConversionMap (Params.systemTopology sysParams) sfg

        res1 = balanceIteration optParams fsys accessf bfIn balStepsIn

        err str = error $
          "iterateEtaWhileAlgorithm: empty "
          ++ str ++ " balanceIteration\n"
          ++ "probable cause: Maybe your iteration condition is always false?"

        lastElem = AppUt.ifNull (err "outer") last res

        bfIn = bForcing lastElem

        balStepsIn = Params.initialBattForceStep optParams
        accessf x =
          StateEta.balanceFromRecord
            (Params.storagePositions sysParams)
            (Type.signals (Type.simulation x))


iterateEtaWhileCondition ::
  (Ord a, Arith.Sum a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  EtaLoopItem node Sweep UV.Vector a z ->
  EtaLoopItem node Sweep UV.Vector a z
iterateEtaWhileCondition optParams (EtaLoopItem u v res) =
  EtaLoopItem u v (condEta maxBICnt bip res)
  where maxBICnt = Params.unMaxBalanceIterations $ Params.maxBalanceIterations optParams
        bip = checkBalance optParams . balance

iterateEtaWhile ::
  (Num a, Ord a, Show a, UV.Unbox a, Arith.ZeroTestable a,Ord node,Show node, Node.C node,
   z ~ Type.SignalBasedOptimisation node Sweep UV.Vector a [] b0 [] c0 [] a,
   Arith.Constant a,RealFloat a, d ~ a) =>
  Params.System node a ->
  Params.Optimisation node [] Sweep UV.Vector a ->
  Params.Simulation node [] a ->
  EnvResult node (Sweep UV.Vector a) ->
  Balance.StateForcing ->
  [EtaLoopItem node Sweep UV.Vector a z]
iterateEtaWhile sysParams optParams simParams sfgIn statForcing =
  take maxEICnt $ iterate go iewi
  where maxEICnt = Params.unMaxEtaIterations $ Params.maxEtaIterations optParams
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
  Map Idx.AbsoluteState Balance.StateForcing ->
  Map k v -> [Char]
printfStateMap forceMap timeMap = concatZipMapsWith f forceMap timeMap
  where f (Idx.AbsoluteState k, x) (_, y) = printf "S%2d%16sf%5.3f" k (show x) y

printfBalanceFMap ::
  (Show node, PrintfArg a1, PrintfArg t1, Arith.Constant a1) =>
  Balance.Forcing node a1 ->
  Map t t1 ->
  [Char]
printfBalanceFMap forceMap balanceMap =
  concatZipMapsWith f (Balance.unForcingMap forceMap) balanceMap
  where f (k, x) (_, y) =
          printf "   | Sto: %7s   | F: %10.15f   | B: %10.15f"
                 (show k) (Balance.getSocDrive x) y

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map a t -> String
printfBalanceMap balanceMap = Map.foldWithKey f "" balanceMap
  where f k v acc = printf " Sto: %7s B: %5.3f" (show k) v ++ acc

showEtaLoop ::
  (Show node, Show a, PrintfArg a, Arith.Constant a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] ->
  [String]
showEtaLoop optParams loop =
  iterateLoops optParams showEtaLoopItem showBalanceLoopItem (zip [0..] loop)

iterateLoops ::
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Params.Optimisation node [] Sweep UV.Vector a ->
      (Counter, EtaLoopItem node Sweep UV.Vector a z) -> o) ->
  (Params.Optimisation node [] Sweep UV.Vector a ->
      (Counter, BalanceLoopItem node a z) -> o) ->
  [(Counter, EtaLoopItem node Sweep UV.Vector a z)]->
  [o]
iterateLoops optParams elf blf etaLoop =
  concatMap g etaLoop
  where g x = elf optParams x
              : map (blf optParams) (zip [0..] (balanceLoop (snd x)))

showEtaLoopItem::
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
  String
showEtaLoopItem _optParams (step, EtaLoopItem _sfgIn _sweep _) =
  printf "EL: %8d" step

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Counter, BalanceLoopItem node a z) ->
  String
showBalanceLoopItem _optParams (bStp, BalanceLoopItem bForc _bFStep bal _) =
  printf " BL: %2d | " bStp ++ printfBalanceFMap bForc bal

