module EFA.Application.Topology.TripodB where

import EFA.Application.Utility ( makeEdges, )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr


data Node = Node0 | Node1 | Node2 | Node3 deriving (Show, Eq, Ord, Enum)

node0, node1, node2, node3 :: Node
node0 = Node0
node1 = Node1
node2 = Node2
node3 = Node3

source, crossing, sink, storage :: Node
source   = Node0
crossing = Node1
sink     = Node2
storage  = Node3


instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topology :: Topo.Topology Node
topology = Gr.fromList ns (makeEdges es)
  where ns = [ (Node0, Node.Source),
               (Node1, Node.Crossing),
               (Node2, Node.Sink),
               (Node3, Node.storage) ]
        es = [(Node0, Node1), (Node1, Node3), (Node1, Node2)]
