module EFA.Application.Topology.TripodA where

import EFA.Application.Utility ( makeEdges, )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format


data Node = Node0 | Node1 | Node2 | Node3 deriving (Show, Eq, Ord, Enum)

node0, node1, node2, node3 :: Node
node0 = Node0
node1 = Node1
node2 = Node2
node3 = Node3


instance Node.C Node where
   display Node0 = Format.literal "null"
   display Node1 = Format.literal "eins"
   display Node2 = Format.literal "zwei"
   display Node3 = Format.literal "drei"

   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault


topology :: Topo.Topology Node
topology = Gr.fromList ns (makeEdges es)
  where ns = [(node0, Node.Source),
              (node1, Node.Sink),
              (node2, Node.Crossing),
              (node3, Node.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]
