module EFA.Application.Tree where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph (Graph)

import Data.Monoid ((<>))


data Tree node nodeLabel =
   Tree node (Graph node Gr.DirEdge nodeLabel ())


toGraph :: Tree node (Node.Type ()) -> Topo.Topology node
toGraph (Tree _ graph) = graph

cons :: Node.C node => node -> Tree node (Node.Type ())
cons node = Tree node $ Gr.insNode (node, Node.typ node) Gr.empty



infixl 5 <+, +>

(<+), (+>) ::
   (Ord node) =>
   Tree node nodeLabel ->
   Tree node nodeLabel ->
   Tree node nodeLabel
(<+) = attach Gr.DirEdge
(+>) = attach (flip Gr.DirEdge)

attach ::
   (Ord node) =>
   (node -> node -> Gr.DirEdge node) ->
   Tree node nodeLabel ->
   Tree node nodeLabel ->
   Tree node nodeLabel
attach edge (Tree top graph) (Tree subTop subGraph) =
   Tree top $
      Gr.insEdge (edge subTop top, ()) $
      graph <> subGraph
