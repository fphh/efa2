module EFA.Example.Topology.TripodA where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Node0 | Node1 | Node2 | Node3 deriving (Show, Eq, Ord, Enum)

node0, node1, node2, node3 :: Node
node0 = Node0
node1 = Node1
node2 = Node2
node3 = Node3

source, sink, crossing, storage :: Node
source   = Node0
sink     = Node1
crossing = Node2
storage  = Node3

instance Node.C Node where
   display Node0 = Format.literal "Quelle"
   display Node1 = Format.literal "Senke"
   display Node2 = Format.literal "Kreuzung"
   display Node3 = Format.literal "Speicher"

   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Node0 = Node.Source
   typ Node1 = Node.Sink
   typ Node2 = Node.Crossing
   typ Node3 = Node.storage


topology :: Topo.Topology Node
topology =
   topologyFromEdges
      [(node0, node2), (node2, node1), (node2, node3)]
