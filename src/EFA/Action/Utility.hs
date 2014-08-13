{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Action.Utility where

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Application.Optimisation.Balance as Balance
--import qualified EFA.Application.Optimisation.Params as Params

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import qualified EFA.Flow.State as State


import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Application.Type as Type
import qualified EFA.Flow.Topology.Quantity as TopoQty

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.Part.Index as PartIdx
--import qualified EFA.Signal.Vector as Vec
--import qualified EFA.Signal.Signal as Sig
--import EFA.Signal.Data (Data, Nil, (:>))
--import EFA.Signal.Typ (Typ)
import Control.Monad ((>=>))
import EFA.Graph (Graph)
--import qualified Data.Bimap as Bimap

import qualified EFA.Report.Format as Format


import EFA.Equation.Unknown (Unknown)
import EFA.Equation.Result (Result(Determined, Undetermined))


import qualified EFA.Utility.Map as MapU

import Data.Tuple.HT (fst3, snd3, thd3)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Foldable (foldMap)

import EFA.Utility(Caller,
                 merror,
--                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Action.Utility"

nc :: FunctionName -> Caller
nc = genCaller modul


topologyFromEdges ::
   Node.C node => [(node, node)] -> Topo.Topology node
topologyFromEdges es =
   Graph.fromMap
      (MapU.fromSet (const ()) $
       foldMap (\(x,y) -> Set.fromList [x,y]) es)
      (Map.fromList $
       map (\(x, y) -> (Graph.DirEdge x y, ())) es)


-- @HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PositionLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PositionLabel, PositionLabel)]
type PositionLabelMap node = Map (TopoIdx.Position node) String


topologyFromLabeledEdges ::
   (Node.C node) =>
   LabeledEdgeList node -> Topo.LabeledTopology node
topologyFromLabeledEdges es =
   Graph.fromMap
      (MapU.fromSet (const "") $
       foldMap (\(x,y,_,_,_) -> Set.fromList [x,y]) es)
      (Map.fromList $
       map (\(x, y, lab, _, _) -> (Graph.DirEdge x y, lab)) es)


{- |
Construct solvable topology from topology with default directions.
-}
quantityTopology ::
   (Node.C node, Unknown v) =>
   Topo.Topology node ->
   FlowTopo.Section node v
quantityTopology topo =
   FlowTopo.sectionFromPlain $
   let flowTopo = Topo.flowFromPlain topo
   in  if StateAnalysis.admissibleTopology flowTopo
         then flowTopo
         else error "quantityTopology: topology has forbidden default edges"


dirEdge :: node -> node -> Graph.EitherEdge node
dirEdge x y = Graph.EDirEdge $ Graph.DirEdge x y

undirEdge :: (Node.C node) => node -> node -> Graph.EitherEdge node
undirEdge x y = Graph.EUnDirEdge $ Graph.UnDirEdge x y

identifyFlowState ::
   (Node.C node) =>
   Topo.Topology node -> [Graph.EitherEdge node] -> Topo.FlowTopology node
identifyFlowState topo givenEdges =
   case StateAnalysis.identify topo givenEdges of
      [] -> error "identifyFlowState: impossible given edges"
      [flowTopo] -> flowTopo
      _ -> error "identifyFlowState: ambiguous given edges"

seqFlowGraphFromStates ::
   (Node.C node, Unknown a, Unknown v) =>
   Topo.Topology node ->
   [[Graph.EitherEdge node]] ->
   SeqFlow.Graph node a v
seqFlowGraphFromStates topo =
   seqFlowGraphFromFlowTopos . map (identifyFlowState topo)

seqFlowGraphFromFlowTopos ::
   (Node.C node, Unknown a, Unknown v) =>
   [Topo.FlowTopology node] ->
   SeqFlow.Graph node a v
seqFlowGraphFromFlowTopos =
   SeqFlow.sequenceGraph . Sequ.fromList

checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Undetermined -> error $ "undetermined " ++ name
      Determined x -> x



unzipMap :: Map k (a, b) -> (Map k a, Map k b)
unzipMap m = (Map.map fst m, Map.map snd m)

unzip3Map :: Map k (a, b, c) -> (Map k a, Map k b, Map k c)
unzip3Map m = (Map.map fst3 m, Map.map snd3 m, Map.map thd3 m)

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

thd4 :: (a, b, c, d) -> c
thd4 (_, _, a, _) = a

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, a) = a

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _,_) = a

snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _,_) = b

thd5 :: (a, b, c, d, e) -> c
thd5 (_, _, c, _,_) = c

frth5 :: (a, b, c, d, e) -> d
frth5 (_, _, _, d,_) = d

findMinElem :: Map k a -> a
findMinElem = snd . Map.findMin

unzip4Map :: Map k (a, b, c, d) -> (Map k a, Map k b, Map k c, Map k d)
unzip4Map m = (Map.map fst4 m, Map.map snd4 m, Map.map thd4 m, Map.map frth4 m)

{-# INLINE mapQuadruple #-}
mapQuadruple :: (a -> u, b -> v, c -> w, d -> x) -> (a,b,c,d) -> (u,v,w,x)
mapQuadruple ~(f, g, h, i) ~(x, y, z, a) = (f x, g y, h z, i a)



getFlowTopology ::
  Idx.State ->
  State.Graph node edge sectionLabel nodeLabel
              storageLabel edgeLabel carryLabel ->
  Maybe (Graph.Graph node edge nodeLabel edgeLabel)
getFlowTopology state =
  fmap FlowTopo.topology . Map.lookup state . State.states


flipPower ::
  Idx.InPart part TopoIdx.Power node -> Idx.InPart part TopoIdx.Power node
flipPower = Idx.liftInPart $
  \(TopoIdx.Power (TopoIdx.Position f t)) -> TopoIdx.Power (TopoIdx.Position t f)


nestM :: (Monad m) => Int -> (a -> m a) -> a -> m a
nestM n act = foldr (>=>) return (replicate n act)



data Orientation = Dir | UnDir deriving Show

absoluteStateIndex ::
  (Node.C node) =>
  Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph node Graph.EitherEdge nodeLabel a ->
  Idx.State
absoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber: edge not found "
                          ++ Format.showRaw (Node.display f :: Format.ASCII)
                          ++ "->"
                          ++ Format.showRaw (Node.display t :: Format.ASCII)

      toTernary xs = Idx.State $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels


-- TODO :: Introduce absolute State as Type Variable
absoluteStateFlowGraph ::
  Node.C node =>
  Graph node Graph.DirEdge nodeLabel1 a1->
  State.Graph node Graph.EitherEdge stateLabel nodeLabel storageLabel flowLabel carryLabel ->
  State.Graph node Graph.EitherEdge stateLabel nodeLabel storageLabel flowLabel carryLabel
absoluteStateFlowGraph topo sfg = sfg {StateQty.states = states,  
                                       StateQty.storages = storages}
  where                                          
    stateMap = Map.fromList $ map (\(oldState,x) -> (oldState,absoluteStateIndex topo $ FlowTopo.topology x)) $
           Map.toList $ StateQty.states sfg
    states = Map.fromList $ map (\(oldState,x) -> (stateMap Map.! oldState,x)) $
           Map.toList $ StateQty.states sfg
           
    storages = Map.map f $ StateQty.storages sfg
    
    f stoGraph = stoGraph{Storage.edges= Map.mapKeys g $ Storage.edges stoGraph}
    
    g (StorageIdx.Edge initSec exitSec) = StorageIdx.Edge (h1 initSec) (h2 exitSec)  
      
    h1 PartIdx.Init = PartIdx.Init  
    h1 (PartIdx.NoInit oldState) = (PartIdx.NoInit $ stateMap Map.! oldState)

    h2 PartIdx.Exit = PartIdx.Exit  
    h2 (PartIdx.NoExit oldState) = (PartIdx.NoExit $ stateMap Map.! oldState)

ifNull :: b -> ([a] -> b) -> [a] -> b
ifNull x _ [] = x
ifNull _ f xs = f xs


