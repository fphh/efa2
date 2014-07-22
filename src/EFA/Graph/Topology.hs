module EFA.Graph.Topology (
   Topology,
   FlowTopology,
   LabeledTopology,
   flowFromPlain,
   plainFromFlow,
   plainFromLabeled,
   outPosFromDirEdge, inPosFromDirEdge,
   dirEdgeFromOutPos, dirEdgeFromInPos,
   isActive,
   anyActive,
   getAbsoluteStateIndex,
   StoreDir(..),
   InOut,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Graph as Graph
import qualified EFA.Graph.Topology.Node as Node
import EFA.Graph (Graph)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold

import EFA.Flow.Part.Index (AbsoluteState(AbsoluteState))

isActive :: Graph.EitherEdge node -> Bool
isActive (Graph.EUnDirEdge _) = False
isActive (Graph.EDirEdge _) = True

anyActive :: Map (Graph.EitherEdge node) () -> Bool
anyActive = Fold.any isActive . Map.keysSet


{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology node = Graph node Graph.DirEdge () ()

type FlowTopology node = Graph node Graph.EitherEdge () ()

type LabeledTopology node = Graph node Graph.DirEdge String String


flowFromPlain :: (Ord node) => Topology node -> FlowTopology node
flowFromPlain = Graph.mapEdgeKeys Graph.EDirEdge

plainFromFlow :: (Ord node) => FlowTopology node -> Topology node
plainFromFlow =
   Graph.mapEdgeKeys
      (\e ->
         case e of
            Graph.EDirEdge de -> de
            Graph.EUnDirEdge ue -> Graph.DirEdge (Graph.from ue) (Graph.to ue))

plainFromLabeled :: Graph node edge nl el -> Graph node edge () ()
plainFromLabeled = Graph.mapNode (const ()) . Graph.mapEdge (const ())


outPosFromDirEdge, inPosFromDirEdge ::
   Graph.DirEdge node -> TopoIdx.Position node
outPosFromDirEdge (Graph.DirEdge x y) = TopoIdx.Position x y
inPosFromDirEdge (Graph.DirEdge x y) = TopoIdx.Position y x

dirEdgeFromOutPos, dirEdgeFromInPos ::
   TopoIdx.Position node -> Graph.DirEdge node
dirEdgeFromOutPos (TopoIdx.Position x y) = Graph.DirEdge x y
dirEdgeFromInPos  (TopoIdx.Position x y) = Graph.DirEdge y x


data StoreDir = In | Out deriving (Eq, Ord, Show)

type InOut node nodeLabel =
        (Map (Graph.EitherEdge node) (),
         nodeLabel,
         Map (Graph.EitherEdge node) ())


-- PG: Code from HH calculate flow state from flow direction against topology 
-- Is that the right place for this code ??

data Orientation = Dir | UnDir deriving Show

getAbsoluteStateIndex ::
  (Node.C node, Show node) =>
  Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph node Graph.EitherEdge nodeLabel a ->
  AbsoluteState
getAbsoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      -- lookup a topology as (node1,node2) and reverse (node2,node1) and check flow
      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber - edge not found: " 
                  ++ show f  ++ "->" ++ show t
      
      -- nodes are Ord, topology Edges are always sorted same, single edge states are
      -- converted generating a ternary number       
      toTernary xs = AbsoluteState $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels




