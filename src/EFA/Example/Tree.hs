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
