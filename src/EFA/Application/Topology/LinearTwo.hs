module EFA.Application.Topology.LinearTwo where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Sink | Crossing | Source deriving (Eq, Ord, Enum, Show)

node0, node1, node2 :: Node
node0 = Sink
node1 = Crossing
node2 = Source


instance Node.C Node where
   display = Format.integer . fromIntegral . fromEnum
   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source   = Node.AlwaysSource
   typ Crossing = Node.Crossing
   typ Sink     = Node.AlwaysSink


topology :: Topo.Topology Node
topology = topologyFromEdges [(node0, node1), (node1, node2)]
