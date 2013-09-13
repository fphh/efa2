module EFA.Application.Topology.LinearOne where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Sink | Source deriving (Ord, Eq, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript Source = Format.integer 0
   subscript Sink = Format.integer 1
   dotId = Node.dotIdDefault

   typ Source = Node.AlwaysSource
   typ Sink   = Node.AlwaysSink


topology :: Topo.Topology Node
topology = topologyFromEdges [(Sink, Source)]
