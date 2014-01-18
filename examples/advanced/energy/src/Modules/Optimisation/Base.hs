{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}


module Modules.Optimisation.Base where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Utility as ModUt
import Modules.Types (EnvResult)

import qualified EFA.Application.DoubleSweep as DoubleSweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Utility as AppUt

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

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined), toMaybe)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Monoid (Monoid)

import Control.Monad (join)
import Control.Applicative (liftA2)



perStateSweep ::
  (Node.C node,
   Ord a, Show a, UV.Unbox a, Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a), Arith.Constant a,
   Sweep.SweepVector vec a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node list sweep vec a ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a)) ->
  Map Idx.State (Map (list a) (EnvResult node (sweep vec a)))
perStateSweep params stateFlowGraph =
  Map.mapWithKey f states
  where states = StateQty.states stateFlowGraph
        reqsAndDofs = map TopoIdx.Power
                      $ ReqsAndDofs.unReqs (One.reqsPos params)
                        ++ ReqsAndDofs.unDofs (One.dofsPos params)

        f state _ = DoubleSweep.doubleSweep solveFunc (One.points params)
          where solveFunc =
                  Optimisation.solve
                    reqsAndDofs
                    (AppOpt.eraseXAndEtaFromState state stateFlowGraph)
                    (One.etaAssignMap params)
                    (One.etaMap params)
                    state



forcing ::
  (Arith.Constant a, Arith.Sum a, Ord a,
   UV.Unbox a, Show a, Ord (sweep vec a), Arith.Sum (sweep vec a),
   Ord node, Show node,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepMap sweep vec a a) =>
  One.OptimalEnvParams node list sweep vec a ->
  Idx.State ->
  StateQty.Graph node b (Result (sweep vec a)) ->
  Result (sweep vec a)
forcing params state graph = Determined $
  case ModUt.getFlowTopology state graph of
    Nothing ->
      error $ "forcing failed, because state not found: " ++ show state
    Just flowTopo ->
      Set.fold (Arith.~+) zero
        $ Map.foldWithKey f Set.empty (One.forcingPerNode params)
      where
        zero = Sweep.fromRational (One.sweepLength params) Arith.zero

        f sto ff =
          Set.union (Set.map (g ff sto) $ Graph.adjacentEdges flowTopo sto)

        g ff sto (Graph.EDirEdge (Graph.DirEdge from to)) =
          (if sto == from then h ff else Arith.negate . h ff . ModUt.flipPower) $
             StateIdx.power state from to
        g _ _ (Graph.EUnDirEdge _) = zero

        h soc p =
          maybe (error $ "forcing failed, because position not found: " ++ show p)
                (Sweep.map (One.getSocDrive soc Arith.~*))
                (join $ fmap toMaybe $ StateQty.lookup p graph)


optimalObjectivePerState ::
  (Ord a, Show a, UV.Unbox a, Arith.Constant a, Arith.Sum a,
   Node.C node, Show node,
   Monoid (sweep vec Bool),
   Ord (sweep vec a),
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec Bool,
   Sweep.SweepClass sweep vec Bool,
   Sweep.SweepMap sweep vec a Bool,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepMap sweep vec a a) =>
  One.OptimalEnvParams node list sweep vec a ->
  Map Idx.State (Map (list a) (EnvResult node (sweep vec a))) ->
  Map Idx.State (Map (list a) (Maybe (a, a, EnvResult node a)))
optimalObjectivePerState params =
  Map.mapWithKey $
    Map.map
    . DoubleSweep.optimalSolutionState2
    . forcing params



selectOptimalState ::
  (Ord a) =>
  Map Idx.State (Map [a] (Maybe (a, a, EnvResult node a))) ->
  Map [a] (Maybe (a, a, Idx.State, EnvResult node a))
selectOptimalState =
  List.foldl1' (Map.unionWith (liftA2 $ ModUt.maxBy ModUt.fst4))
  . map (\(st, m) -> Map.map (fmap (\(objVal, eta, env) -> (objVal, eta, st, env))) m)
  . Map.toList


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
  One.OptimalEnvParams node list sweep vec v ->
  Sequ.Section (Record.Record t t3 t1 t4 (TopoIdx.Position node) [] t2 t5) ->
  Bool
consistentSection params (Sequ.Section _ _ rec) =
  let recs = map f $ Graph.edges $ One.systemTopology params
      f (Graph.DirEdge fr to) =
        Record.extract [TopoIdx.ppos fr to, TopoIdx.ppos to fr] rec
  in all consistentRecord recs


filterPowerRecordList ::
  (Ord v, Show v, Arith.Constant v, Node.C node) =>
  One.OptimalEnvParams node list sweep vec v ->
  Sequ.List (Record.PowerRecord node [] v) ->
  ( Sequ.List (Record.PowerRecord node [] v),
    Sequ.List (Record.PowerRecord node [] v) )
filterPowerRecordList params (Sequ.List recs) =
  let (ok, bad) = List.partition (consistentSection params) recs
  in (Sequ.List ok, Sequ.List bad)



-- HH: hier sollen tatsächlich params und ppos getrennt hineingefuehrt werden,
-- damit man die Funktion auch für andere Positionen verwenden kann.

signCorrectedOptimalPowerMatrices ::
  (Ord v, Arith.Sum v, Arith.Constant v, Show node, Ord node) =>
  One.OptimalEnvParams node [] sweep vec v ->
  Map [v] (Maybe (Double, Double, Idx.State, EnvResult node v)) ->
  ReqsAndDofs.Dofs (TopoIdx.Position node) ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector Vector (Maybe (Result v)))
signCorrectedOptimalPowerMatrices params m (ReqsAndDofs.Dofs ppos) =
  Map.fromList $ map g ppos
  where g pos = (pos, ModUt.to2DMatrix $ Map.map f m)
          where f Nothing = Nothing
                f (Just (_, _, st, graph)) =
                  case StateQty.lookup (StateIdx.powerFromPosition st pos) graph of
                       Just sig -> Just $
                         if isFlowDirectionPositive params st pos graph
                            then sig
                            else fmap Arith.negate sig
                       _ -> fmap (const (Determined Arith.zero))
                                 (getEdgeFromPosition st pos graph)






isFlowDirectionPositive ::
  (Ord node, Show node) =>
  One.OptimalEnvParams node list sweep vec v ->
  Idx.State ->
  TopoIdx.Position node ->
  EnvResult node v ->
  Bool
isFlowDirectionPositive params state (TopoIdx.Position f t) graph =
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
        topo = One.systemTopology params
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
  (Ord b, Ord node) =>
  Map Idx.State (Map [b] (Maybe (a1, EnvResult node Double))) ->
  [TopoIdx.Position node] ->
  Map (TopoIdx.Position node)
      (Map Idx.State (Sig.PSignal2 Vector Vector (Maybe (Result Double))))
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
  where f pm = liftA2 (Arith.~-) (PartMap.exit pm) (PartMap.init pm)


stateFlowBalance ::
  (Arith.Sum a, UV.Unbox a, Arith.Sum (sweep vec a)) =>
  EnvResult node (sweep vec a) ->
  Map node (Result (sweep vec a))
stateFlowBalance = fmap (f . Storage.nodes) . StateQty.storages
  where f pm = liftA2 (Arith.~-) (PartMap.exit pm) (PartMap.init pm)

