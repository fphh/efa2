module EFA.Application.Tree where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import Data.Monoid ((<>))


data Tree node nodeLabel =
   Tree node (Graph node Graph.DirEdge nodeLabel ())


toGraph :: Tree node () -> Topo.Topology node
toGraph (Tree _ graph) = graph

cons :: Node.C node => node -> Tree node ()
cons node = Tree node $ Graph.insertNode (node, ()) Graph.empty



infixl 5 <+, +>

(<+), (+>) ::
   (Ord node) =>
   Tree node nodeLabel ->
   Tree node nodeLabel ->
   Tree node nodeLabel
(<+) = attach Graph.DirEdge
(+>) = attach (flip Graph.DirEdge)

attach ::
   (Ord node) =>
   (node -> node -> Graph.DirEdge node) ->
   Tree node nodeLabel ->
   Tree node nodeLabel ->
   Tree node nodeLabel
attach edge (Tree top graph) (Tree subTop subGraph) =
   Tree top $
      Graph.insertEdge (edge subTop top, ()) $
      graph <> subGraph
