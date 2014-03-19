{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}


module Modules.Optimisation.Base where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Utility as ModUt

import qualified EFA.Application.DoubleSweep as DoubleSweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs
import qualified EFA.Application.Type as Type
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Utility as AppUt
import EFA.Application.Type (EnvResult)

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State as State

import qualified EFA.Flow.Sequence as Sequence
import qualified EFA.Flow.Sequence.Quantity as SeqQty

import qualified EFA.Flow.Part.Map as PartMap
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Graph as Graph
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Signal.Signal as Sig

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Vector as Vec
import EFA.Signal.Data (Data(Data), Nil, (:>))
import EFA.Signal.Typ (Typ)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import EFA.Equation.Result (Result(Determined,Undetermined))

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Monoid (Monoid)
import Data.Maybe (fromMaybe)

import Control.Monad (join)
import Control.Applicative (liftA2)

-- import Debug.Trace (trace)

perStateSweep ::
  (Node.C node, Show node,RealFloat a,
   Ord a, Show a, UV.Unbox a, Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a), Arith.Constant a,
   Sweep.SweepVector vec a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a,
   Monoid (sweep vec Bool),
   Sweep.SweepMap sweep vec a Bool,
   Sweep.SweepClass sweep vec Bool) =>
  One.SystemParams node a ->
  One.OptimisationParams node list sweep vec a ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a)) ->
  Map Idx.State (Map (list a) (Type.SweepPerReq node sweep vec a))
perStateSweep sysParams optParams stateFlowGraph  =
  Map.mapWithKey f states
  where states = StateQty.states stateFlowGraph
        reqsAndDofs = map TopoIdx.Power
                      $ ReqsAndDofs.unReqs (One.reqsPos optParams)
                        ++ ReqsAndDofs.unDofs (One.dofsPos optParams)

        f state _ = DoubleSweep.doubleSweep solveFunc (One.points optParams)
          where solveFunc =
                  Optimisation.solve
                    optParams
                    reqsAndDofs
                    (AppOpt.eraseXAndEtaFromState state stateFlowGraph)
                    (One.etaAssignMap sysParams)
                    (One.etaMap sysParams)
                    state



balForcing ::
  (Ord node, Show node,
   Sweep.SweepClass sweep vec a,
   Arith.Sum (sweep vec a),
   Sweep.SweepMap sweep vec a a,
   Arith.Constant a) =>
  One.BalanceForcing node a ->
  One.OptimisationParams node list sweep vec a ->
  Type.StoragePowerMap node sweep vec a ->
  Result (sweep vec a)
balForcing balanceForcing params powerMap =
  Map.foldWithKey f zero (One.unBalanceForcingMap balanceForcing)
      where
        zero = Determined $ Sweep.fromRational (One.sweepLength params) Arith.zero
        f stoNode forcingFactor acc = g acc force
          where
            g (Determined ac) (Determined fo) = Determined $ ac ~+ fo
            g _ _ = Undetermined

            force = fmap (Sweep.map (One.getSocDrive forcingFactor ~*)) stoPower

            stoPower =
              fromMaybe (error $ "forcing failed, because node not found: " ++ show stoNode)
                        (join $ Map.lookup stoNode powerMap)

optStackPerState ::
  (UV.Unbox a,
   Arith.Sum a,
   Sweep.SweepClass sweep UV.Vector a,
   Ord node,
   Show node,
   Arith.Sum (sweep UV.Vector a),
   Arith.Constant a,
   Sweep.SweepClass sweep UV.Vector (a, a)) =>
  One.OptimisationParams node list sweep UV.Vector a ->
  One.BalanceForcing node a ->
  Map Idx.State (Map [a] (Type.SweepPerReq node sweep UV.Vector a)) ->
  Type.OptStackPerState sweep UV.Vector a
optStackPerState params balanceForcing =
  Map.map
  $ Map.map
  $ DoubleSweep.objectiveValue (balForcing balanceForcing params)


optimalObjectivePerState ::
  (Ord a, Arith.Constant a, Arith.Sum a, UV.Unbox a,RealFloat a,
   Show node, Node.C node, Monoid (sweep UV.Vector  Bool),
   Ord (sweep UV.Vector  a),
   Arith.Product (sweep UV.Vector  a),
   Sweep.SweepVector UV.Vector  Bool,
   Sweep.SweepClass sweep UV.Vector  Bool,
   Sweep.SweepMap sweep UV.Vector  a Bool,
   Sweep.SweepVector UV.Vector  a,
   Sweep.SweepClass sweep UV.Vector  a,
   Sweep.SweepMap sweep UV.Vector  a a) =>
  One.OptimisationParams node list sweep UV.Vector a ->
  One.BalanceForcing node a ->
  Map Idx.State (Map [a] (Type.SweepPerReq node sweep UV.Vector a)) ->
  Type.OptimalSolutionPerState node a
optimalObjectivePerState params balanceForcing =
  Map.map
  $ Map.map
  $ DoubleSweep.optimalSolutionState2 (balForcing balanceForcing params)


expectedValuePerState ::
  (UV.Unbox a,
   Arith.Constant a,
   Sweep.SweepClass sweep UV.Vector a,
   Sweep.SweepClass sweep UV.Vector Bool) =>
  Map Idx.State (Map (list a) (Type.SweepPerReq node sweep UV.Vector a)) ->
  Map Idx.State (Map (list a) (Maybe a))
expectedValuePerState =
  Map.map (Map.map DoubleSweep.expectedValue)


{-
-- TODO: is this code is still neeed for Display purposes ? -- needs to work with new StateForcing -- does it make sense ?
selectOptimalState ::
  (Ord a,Arith.Sum a,Show (One.StateForcing a), Show a,RealFloat a) =>
  One.OptimisationParams node list sweep vec a ->
  Map Idx.AbsoluteState (One.StateForcing a) ->
  Type.OptimalSolutionPerState node a ->
  One.IndexConversionMap ->
  Type.OptimalSolution node a 
selectOptimalState _params stateForcing stateMap indexConversionMap =
  let
      g _ Nothing y = y
      g _ x Nothing = x
      g f (Just x) (Just y) = Just (f x y)

  in List.foldl1' (Map.unionWith (g $ ModUt.maxByWithNaN ModUt.fst5))
     $ map (\(st, m) ->
             Map.map (fmap
                      (\(objVal, eta, idx ,env) ->
                        (objVal Arith.~+
                         maybe (error "Base.selectOptimalState")
                         One.unStateForcing
                         (ModUt.state2absolute st indexConversionMap >>= flip Map.lookup stateForcing),
                         eta, st, idx, env))) m)
     $ Map.toList stateMap
-}

supportPoints ::
  (Ord a,Show (vec a),Vec.Len (vec a),Node.C node,
   Vec.Unique vec [a],
   Vec.Storage vec ([[a]], [Sig.SignalIdx]),
   Vec.Storage vec Sig.SignalIdx,
   Vec.Storage vec Int,
   Vec.Storage vec ([a]),
   Vec.FromList vec,
   Vec.Find vec,
   Vec.Filter vec, 
   Vec.Zipper vec,
   Vec.Walker vec,
   Vec.Storage vec a,
   Vec.Singleton vec,
   Arith.Constant a, 
   Show a,
   Vec.Storage vec Bool,
   Vec.Lookup vec) =>
  [ TopoIdx.Position node] ->
  Record.PowerRecord node vec a ->
  [(a -> [a])] ->
  Sig.UTDistr vec ([[a]], [Sig.SignalIdx])
supportPoints idList rec functList = 
  Sig.getActiveSupportPointsND
  $ zip functList
  $ map (Sig.untype . Record.getSig rec) idList

envToPowerRecord ::
  (Ord node) =>
  TopoQty.Section node (Result (Data (v :> Nil) a)) ->
  Record.PowerRecord node v a
envToPowerRecord =
  TopoRecord.sectionToPowerRecord
  . TopoQty.mapSection (AppUt.checkDetermined "envToPowerRecord")


convertRecord ::
  (Vec.Storage v d2, Vec.Storage t d2, Vec.Storage v d1,
   Vec.Storage t d1, Vec.Convert t v) =>
  Record.Record s1 s2 t1 t2 id t d1 d2 ->
  Record.Record s1 s2 t1 t2 id v d1 d2
convertRecord (Record.Record time sigMap) =
  Record.Record (Sig.convert time) (Map.map Sig.convert sigMap)


consistentRecord ::
  (Ord t5, Show t5, Arith.Constant t5) =>
  Record.Record t t3 t1 t4 k [] t2 t5 -> Bool
consistentRecord (Record.Record _ m) =
  case Map.elems m of
       [xs, ys] -> consistentIndices xs ys
       zs -> error $ "consistentRecord: more or less than exactly two signals: "
                     ++ show zs
  where consistentIndices (Sig.TC (Data xs)) (Sig.TC (Data ys)) =
          let zs = xs ++ ys
          in all (<= Arith.zero) zs || all (Arith.zero <=) zs


consistentSection ::
  (Ord t5, Show t5, Node.C node, Arith.Constant t5) =>
  One.SystemParams node a ->
  Sequ.Section (Record.Record t t3 t1 t4 (TopoIdx.Position node) [] t2 t5) ->
  Bool
consistentSection sysParams (Sequ.Section _ _ rec) =
  let recs = map f $ Graph.edges $ One.systemTopology sysParams
      f (Graph.DirEdge fr to) =
        Record.extract [TopoIdx.ppos fr to, TopoIdx.ppos to fr] rec
  in all consistentRecord recs


filterPowerRecordList ::
  (Ord a, Show a, Arith.Constant a, Node.C node) =>
  One.SystemParams node a ->
  Sequ.List (Record.PowerRecord node [] a) ->
  ( Sequ.List (Record.PowerRecord node [] a),
    Sequ.List (Record.PowerRecord node [] a) )
filterPowerRecordList sysParams (Sequ.List recs) =
  let (ok, bad) = List.partition (consistentSection sysParams) recs
  in (Sequ.List ok, Sequ.List bad)



-- HH: hier sollen tatsächlich params und ppos getrennt hineingefuehrt werden,
-- damit man die Funktion auch für andere Positionen verwenden kann.

signCorrectedOptimalPowerMatrices ::
  (Ord a, Arith.Sum a, Arith.Constant a, Show node, Ord node,
   Vec.Storage varVec (Maybe (Result a)),
   Vec.FromList varVec) =>
  One.SystemParams node a ->
  ReqsAndDofs.Dofs (TopoIdx.Position node) ->
  Map [a] (Maybe (a, a, Idx.State, Int, EnvResult node a)) ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector varVec (Maybe (Result a)))
signCorrectedOptimalPowerMatrices systemParams (ReqsAndDofs.Dofs ppos) m =
  Map.fromList $ map g ppos
  where g pos = (pos, ModUt.to2DMatrix $ Map.map f m)
          where f Nothing = Nothing
                f (Just (_, _, st, _, graph)) =
                  case StateQty.lookup (StateIdx.powerFromPosition st pos) graph of
                       Just sig -> Just $
                         if isFlowDirectionPositive systemParams st pos graph
                            then sig
                            else fmap Arith.negate sig
                       _ -> fmap (const (Determined Arith.zero))
                                 (getEdgeFromPosition st pos graph)

isFlowDirectionPositive ::
  (Ord node, Show node) =>
  One.SystemParams node a ->
  Idx.State ->
  TopoIdx.Position node ->
  EnvResult node a ->
  Bool
isFlowDirectionPositive sysParams state (TopoIdx.Position f t) graph =
  case Set.toList es of
       [Graph.DirEdge fe te] ->
         case flowTopoEs of
              Just set ->
                case ( Set.member (Graph.EDirEdge $ Graph.DirEdge fe te) set,
                       Set.member (Graph.EDirEdge $ Graph.DirEdge te fe) set ) of
                     (True, False)  -> True
                     (False, True)  -> False
                     tf -> error $ "isFlowDirectionPositive: "
                                   ++ "inconsisten flow graph " ++ show tf
              _ -> error $ "State (" ++ show state ++ ") not found"
       _ -> error $ "More or less than exactly one edge between nodes "
                    ++ show f ++ " and " ++ show t ++ " in " ++ show es
  where flowTopoEs = fmap Graph.edgeSet $ ModUt.getFlowTopology state graph
        topo = One.systemTopology sysParams
        es = Graph.adjacentEdges topo f
               `Set.intersection` Graph.adjacentEdges topo t


getEdgeFromPosition ::
  (Ord (e a), Ord a, Show (e a), Show a, Graph.Edge e) =>
  Idx.State ->
  TopoIdx.Position a ->
  State.Graph a e sectionLabel nl storageLabel el carryLabel ->
  Maybe (e a)
getEdgeFromPosition state (TopoIdx.Position f t) =
  let g flowTopo =
        case Set.toList es of
             [e] -> e
             _ -> error $ "More or less than exactly one edge between nodes "
                          ++ show f ++ " and " ++ show t ++ " in " ++ show es
        where es = Graph.adjacentEdges flowTopo f
                     `Set.intersection` Graph.adjacentEdges flowTopo t
  in fmap g . ModUt.getFlowTopology state



extractOptimalPowerMatricesPerState ::
  (Ord b, Ord node,
  Vec.Storage vec (vec (Maybe (Result a))),
  Vec.Storage vec (Maybe (Result a)),
  Vec.FromList vec) =>
  Map Idx.State (Map [b] (Maybe (a1, EnvResult node a))) ->
  [TopoIdx.Position node] ->
  Map (TopoIdx.Position node)
      (Map Idx.State (Sig.PSignal2 Vector vec (Maybe (Result a))))
extractOptimalPowerMatricesPerState m ppos =
  Map.map (Map.map ModUt.to2DMatrix)
  $ Map.fromList $ map (\p -> (p, Map.mapWithKey (f p) m)) ppos
  where f p st matrixMap = Map.map g matrixMap
          where pos = StateIdx.powerFromPosition st p
                g = join . fmap (StateQty.lookup pos . snd)


seqFlowBalance ::
  (Arith.Sum a, UV.Unbox a, Arith.Sum (sweep vec a)) =>
  Sequence.Graph node structEdge sectionLabel nodeLabel
                 (Result (sweep vec a)) boundaryLabel structLabel edgeLabel ->
  Map node (Result (sweep vec a))
seqFlowBalance = fmap (f . Storage.nodes . fst) . SeqQty.storages
  where f pm = liftA2 (~-) (PartMap.exit pm) (PartMap.init pm)


stateFlowBalance ::
  (Arith.Sum a, UV.Unbox a, Arith.Sum (sweep vec a)) =>
  EnvResult node (sweep vec a) ->
  Map node (Result (sweep vec a))
stateFlowBalance = fmap (f . Storage.nodes) . StateQty.storages
  where f pm = liftA2 (~-) (PartMap.exit pm) (PartMap.init pm)



getOptimalControlMatricesOfOneState ::
  (Vec.Walker varVec, Vec.Storage varVec a, 
   Arith.Constant a, Ord node,
   Ord a,
   Show node,
   Vec.Storage varVec (Maybe (Result a)),
   Vec.FromList varVec,
   Arith.Sum a) =>
  One.SystemParams node a -> 
  One.OptimisationParams node list sweep vec a -> 
  Idx.State ->
  Type.OptimalSolutionOfOneState node a ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector varVec a)
getOptimalControlMatricesOfOneState sysParams optParams state =
  Map.map (Sig.map ModUt.nothing2Nan)
  . signCorrectedOptimalPowerMatrices sysParams (One.dofsPos optParams)
  . Map.map (fmap (\(o, e, i, v) -> (o, e, state, i, v)))


{-
optimalMatrixOfOneState::
  (Vec.Walker varVec, Vec.Storage varVec a, Arith.Constant a, 
   Ord a, Vec.Storage varVec (Maybe (Result a)), Vec.FromList varVec) =>
  Type.OptimalSolutionOfOneState node a ->
  Sig.PSignal2 Vector varVec a
-}



optimalMatrixOfOneState ::
  (Ord b, Arith.Constant d2, Vec.Walker v1, Vec.Walker v2,
   Vec.Storage v1 d2, Vec.Storage v2 (v1 d2),
   Vec.Storage v1 (Maybe (Result d2)),
   Vec.Storage v2 (v1 (Maybe (Result d2))), Vec.FromList v1,
   Vec.FromList v2) =>
  (a -> d2) ->
  Map [b] (Maybe a) ->
  Sig.TC s (Typ x y z) (Data (v2 :> (v1 :> Nil)) d2)
optimalMatrixOfOneState f =
  Sig.map ModUt.nothing2Nan
  . ModUt.to2DMatrix
  . Map.map (fmap (Determined . f))

optimalObjectiveMatrixOfOneState, optimalEtaMatrixOfOneState ::
  (Vec.Walker varVec, Vec.Storage varVec a, Arith.Constant a, 
   Ord a, Vec.Storage varVec (Maybe (Result a)), Vec.FromList varVec) =>
  Type.OptimalSolutionOfOneState node a ->
  Sig.PSignal2 Vector varVec a
optimalObjectiveMatrixOfOneState = optimalMatrixOfOneState ModUt.fst4
optimalEtaMatrixOfOneState = optimalMatrixOfOneState ModUt.snd4

optimalIndexMatrixOfOneState::
  (Vec.Storage varVec Int, Arith.Constant Int, 
   Vec.Storage varVec (Maybe (Result Int))) =>
  (Vec.Walker varVec, Vec.Storage varVec a, Arith.Constant a, 
   Ord a, Vec.Storage varVec (Maybe (Result a)), Vec.FromList varVec) =>
  Type.OptimalSolutionOfOneState node a ->
  Sig.PSignal2 Vector varVec Int
optimalIndexMatrixOfOneState = optimalMatrixOfOneState ModUt.thd4


genOptimalObjectiveSignal :: 
  (Vec.Zipper vec,Ord a,Show (vec Bool),Show (vec a),RealFloat a,
   Vec.Walker vec, Vec.Storage vec a) =>
  Type.InterpolationOfAllStates node vec a -> Sig.UTSignal vec a
genOptimalObjectiveSignal interpolation =
  Map.foldl' (Sig.zipWith (ModUt.maxByWithNaN id)) h t
  where objectiveSigPerState =
          Map.map Type.optObjectiveSignalOfState interpolation

        (h, t) = case Map.minView objectiveSigPerState of
                      Just x -> x
                      Nothing -> error "genOptimalObjectiveSignal: empty interpolation map"



myTrace :: Show a => String -> a -> a
myTrace _str x = x -- trace (str ++ ": " ++ show x) x


findOptimalObjectiveStates :: 
  (Vec.Zipper vec,Ord a,Vec.Storage vec Bool,Show (vec Bool),
   Vec.Singleton vec,Show (vec a),RealFloat a,Show a,
   Arith.Sum a,
   Vec.Walker vec,
   Vec.Storage vec a) =>
  One.StateForcing ->
  Type.InterpolationOfAllStates node vec a -> Map Idx.State (Sig.UTSignal vec Bool)
findOptimalObjectiveStates statForcing interpolation =
  Map.map (g . f . Type.optObjectiveSignalOfState) interpolation
  where opt = genOptimalObjectiveSignal interpolation
        f = forceOptimalStateSignal statForcing opt
        g = Sig.zipWith (==) opt

forceOptimalStateSignal :: 
  (Vec.Walker vec, Arith.Sum a,Vec.Zipper vec, Show a,RealFloat a,
   Ord a, Vec.Storage vec a, Vec.Singleton vec) =>
  One.StateForcing ->
  Sig.UTSignal vec a ->
  Sig.UTSignal vec a ->
  Sig.UTSignal vec a 
forceOptimalStateSignal stateForcing overallOptimalSignal optimalSignalOfState =
  case stateForcing of
       One.StateForcingOn -> Sig.offset minimalDifference optimalSignalOfState
       One.StateForcingOff -> optimalSignalOfState
  where differenceSignal = overallOptimalSignal Sig..- optimalSignalOfState
        minimalDifference = Sig.fromScalar $ Sig.minimumWithNaN differenceSignal


genOptimalStatesSignal ::
  (Ord a,Vec.Storage vec [Idx.State],Show (vec [Idx.State]),Show (vec Bool),
   Vec.Singleton vec,Show (vec a),Show a,
   Arith.Sum a,
   Vec.Zipper vec,
   Vec.Walker vec,
   Vec.Storage vec a,RealFloat a,
   Vec.Storage vec Bool) =>
  One.StateForcing ->
  Type.InterpolationOfAllStates node vec a ->
   Sig.UTSignal vec [Idx.State]
genOptimalStatesSignal statForcing interpolation =
  Map.foldlWithKey' (flip (Sig.zipWith . f)) emptyIndexSignal optStates
  where optStates = findOptimalObjectiveStates statForcing interpolation

        f st a b = a ++ (if b then [st] else [])

        time = Record.getTime
               $ Type.reqsAndDofsSignalsOfState
               $ ModUt.findMinElem interpolation

        emptyIndexSignal = Sig.untype $ Sig.map (const []) time  


-- TODO test bauen::
-- TC (Data [0.0,0.3333333333333333,0.6666666666666666,1.0,1.25,1.5,1.75,2.0])
-- genOptimalTime (Sig.fromList [[Idx.State 0],[Idx.State 1, Idx.State 1, Idx.State 1, Idx.State 0],[Idx.State 1]]) (Sig.fromList [0,1,2]) :: Sig.TSignal [] Double

genOptimalSteppedTime  :: 
  (Vec.Zipper vec,Eq a,Show a,Show (vec a),
   Vec.Walker vec,
   Vec.Storage vec (a, a),
   Arith.Constant a,
   Vec.FromList vec,
   Vec.Storage vec a,
   Vec.Singleton vec,
   Vec.Storage vec [Idx.State]) =>
  Sig.UTSignal vec [Idx.State] -> 
  Sig.TSignal vec a -> Sig.TSignal vec a
genOptimalSteppedTime indexSignal time =
  Sig.fromList $ concat $ zipWith f is ts
  where f states (t1, t2) =
          if t1 == t2
             then []
             else concat $ zipWith (\x y -> [x, y]) leftTimes rightTimes

          where 
                leftTimes = map (g . convert) [0 .. len]
                rightTimes =
                  case leftTimes of
                       (_:xs) -> xs
                       _ -> error "genOptimalSteppedTime: empty time list"

                len = length states
                convert = Arith.fromRational . fromIntegral
                g cnt = t1 ~+ (cnt ~* (t2 ~- t1) ~/ convert len)

        is = Sig.toList indexSignal
        ts = Sig.toList $ Sig.deltaMap (,) time

genOptimalSteppedSignal  :: 
  (Vec.Storage vec [Idx.State], Eq a,Vec.Storage vec (a, a), Show (vec a),
   Vec.Singleton vec,Show a,
   Vec.Zipper vec, Vec.Walker vec,
   Vec.Storage vec a, Vec.FromList vec, 
   Vec.Storage vec (Map Idx.State a)) =>
  Sig.UTSignal vec [Idx.State] -> 
  Sig.TSignal vec a ->
  Map Idx.State (Sig.PSignal vec a) -> 
  Sig.PSignal vec a 
genOptimalSteppedSignal indexSignal time signalMap =
  Sig.fromList $ concat $ zipWith3 g is ts signalOfMaps
  where
    g states (t1, t2) m
      = if t1 == t2 then [] else concatMap (h m) states

    h m st = let x = fromMaybe (error $ err st m) (Map.lookup st m) in [x, x]

    err x m = "genOptimalSteppedSignal: Element "
              ++ show x ++ " not found in " ++ show m

{-
    wofür ist das denn da?

    xlast =
      case (signalOfMaps, is) of
           (m:_, xs@(x:_):_) ->
             let x = last xs in fromMaybe (error $ err x m) (Map.lookup x m)
           _ -> error "genOptimalSteppedSignal: empty list"
-}

    signalOfMaps = Sig.toList $
      Map.foldrWithKey' (Sig.zipWith . Map.insert) emptySig signalMap

    is = Sig.toList indexSignal
    ts = Sig.toList $ Sig.deltaMap (,) time

    emptySig = Sig.map (const Map.empty) $ snd $ Map.findMin signalMap


 