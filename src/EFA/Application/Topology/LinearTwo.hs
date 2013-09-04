module EFA.Application.Topology.LinearTwo where

import EFA.Application.Utility ( makeEdges, )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

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


topology :: Topo.Topology Node
topology = Gr.fromList ns (makeEdges es)
  where ns = [(node0, Node.Source),
              (node1, Node.Crossing),
              (node2, Node.Sink)]
        es = [(node0, node1), (node1, node2)]
