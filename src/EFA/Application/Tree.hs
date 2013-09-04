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

cons :: Ord node => node -> nodeLabel -> Tree node nodeLabel
cons node label = Tree node $ Gr.insNode (node, label) Gr.empty


storage, sink, alwaysSink, source, alwaysSource,
   crossing, deadNode, noRestriction ::
      Ord node => node -> Tree node (Node.Type ())
storage       = flip cons Node.storage
sink          = flip cons Node.Sink
alwaysSink    = flip cons Node.AlwaysSink
source        = flip cons Node.Source
alwaysSource  = flip cons Node.AlwaysSource
crossing      = flip cons Node.Crossing
deadNode      = flip cons Node.DeadNode
noRestriction = flip cons Node.NoRestriction



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
