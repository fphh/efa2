module EFA.Example.Tree where

import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Graph (Graph)

import Data.Monoid ((<>))


data Tree node nodeLabel =
   Tree node (Graph node Gr.DirEdge nodeLabel ())


toGraph :: Tree node (TD.NodeType ()) -> TD.Topology node
toGraph (Tree _ graph) = graph

cons :: Ord node => node -> nodeLabel -> Tree node nodeLabel
cons node label = Tree node $ Gr.insNode (node, label) Gr.empty


storage, sink, alwaysSink, source, alwaysSource,
   crossing, deadNode, noRestriction ::
      Ord node => node -> Tree node (TD.NodeType ())
storage       = flip cons TD.storage
sink          = flip cons TD.Sink
alwaysSink    = flip cons TD.AlwaysSink
source        = flip cons TD.Source
alwaysSource  = flip cons TD.AlwaysSource
crossing      = flip cons TD.Crossing
deadNode      = flip cons TD.DeadNode
noRestriction = flip cons TD.NoRestriction



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
