module EFA.Application.Topology.LinearOne where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo


data Node = Sink | Source deriving (Ord, Eq, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

   typ Source = Node.AlwaysSource
   typ Sink   = Node.AlwaysSink


topology :: Topo.Topology Node
topology = topologyFromEdges [(Sink, Source)]
