{- |
Temporary topology data structure with incomplete edge set.
We need it for combinatorics on flow states.
-}
module EFA.Graph.Topology.Count (
   CountTopology,
   checkNode,
   checkNodeType,
   edgeOrients,
   admissibleEdges,
   splitNodesEdges,
   admissibleTopology,
   nodeDegrees,
   removeCounts,
   insEdge,
   insEdgeSet,
   expand,
   ) where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (FlowTopology, Topology)

import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold
import Control.Monad (guard)
import Data.Set (Set)


type NodeType = Node.Type ()
type NumberOfAdj = Int
type CountTopology node =
        Graph node Graph.EitherEdge NumberOfAdj ()


infix 1 `implies`

implies :: Bool -> Bool -> Bool
implies x y = not x || y

{- |
We export this function only for testing.
Do not use it outside of the module.
-}
checkNodeType :: NodeType -> Bool -> Bool -> Bool -> Bool
checkNodeType typ complete sucActive preActive =
   case typ of
      Node.Crossing -> complete `implies` sucActive == preActive
      Node.Source -> not preActive
      Node.AlwaysSource -> not preActive && (complete `implies` sucActive)
      Node.Sink -> not sucActive
      Node.AlwaysSink -> not sucActive && (complete `implies` preActive)
      Node.Storage _ -> True
      Node.NoRestriction -> True
      Node.DeadNode -> not sucActive && not preActive

checkInOut ::
   (Node.C node) =>
   NodeType -> Topo.InOut node NumberOfAdj -> Bool
checkInOut nodeType (pre, nadj, suc) =
   checkNodeType nodeType
      (Map.size pre + Map.size suc == nadj)
      (Topo.anyActive suc)
      (Topo.anyActive pre)

checkNode :: (Node.C node) => CountTopology node -> node -> Bool
checkNode topo x =
   case Map.lookup x $ Graph.graphMap topo of
      Nothing -> error "checkNode: node not in graph"
      Just inOut -> checkInOut (Node.typ x) inOut

admissibleTopology :: (Node.C node) => CountTopology node -> Bool
admissibleTopology =
   Fold.and . Map.mapWithKey (checkInOut . Node.typ) . Graph.graphMap


insEdge ::
   Ord node =>
   Graph.EitherEdge node -> CountTopology node -> CountTopology node
insEdge e = Graph.insEdge (e, ())

insEdgeSet ::
   Ord node =>
   Set (Graph.EitherEdge node) -> CountTopology node -> CountTopology node
insEdgeSet e = Graph.insEdgeSet (MapU.fromSet (const ()) e)


edgeOrients ::
   (Ord node, Graph.Edge edge) =>
   edge node -> [Graph.EitherEdge node]
edgeOrients e =
   let x = Graph.from e
       y = Graph.to e
   in  (Graph.EDirEdge $ Graph.DirEdge x y) :
       (Graph.EDirEdge $ Graph.DirEdge y x) : -- x and y swapped!
       (Graph.EUnDirEdge $ Graph.unDirEdge x y) :
       []


admissibleEdges ::
   (Node.C node, Graph.Edge edge) =>
   edge node -> CountTopology node ->
   [(Graph.EitherEdge node, CountTopology node)]
admissibleEdges e0 g0 = do
   e1 <- edgeOrients e0
   let g1 = insEdge e1 g0
   guard $ Fold.all (checkNode g1) e0
   return (e1, g1)

expand ::
   (Node.C node, Graph.Edge edge) =>
   edge node -> CountTopology node -> [CountTopology node]
expand e g = map snd $ admissibleEdges e g

nodeDegrees ::
   (Ord node, Ord (edge node), Graph.Edge edge) =>
   Graph node edge () () ->
   Map node NumberOfAdj
nodeDegrees =
   Map.map (\(pre,(),suc) -> Map.size pre + Map.size suc) .
   Graph.graphMap

splitNodesEdges ::
   (Ord node) =>
   Topology node ->
   (CountTopology node, [Graph.DirEdge node])
splitNodesEdges topo =
   (Graph.fromMap (nodeDegrees topo) Map.empty,
    Graph.edges topo)

removeCounts :: CountTopology node -> FlowTopology node
removeCounts = Graph.mapNode (const ())
