module EFA.Application.Topology.LinearOne where

import EFA.Application.Utility ( makeEdges, )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr


data Node = Sink | Source deriving (Ord, Eq, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topology :: Topo.Topology Node
topology = Gr.fromList nodes (makeEdges edges)
  where nodes = [(Sink, Node.AlwaysSink), (Source, Node.AlwaysSource)]
        edges = [(Sink, Source)]
