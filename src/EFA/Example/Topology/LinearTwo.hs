module EFA.Example.Topology.LinearTwo where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Source | Crossing | Sink deriving (Eq, Ord, Enum, Show)

node0, node1, node2 :: Node
node0 = Source
node1 = Crossing
node2 = Sink


instance Node.C Node where
   display Source   = Format.literal "Quelle"
   display Crossing = Format.literal "Kreuzung"
   display Sink     = Format.literal "Senke"
   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source   = Node.AlwaysSource
   typ Crossing = Node.Crossing
   typ Sink     = Node.AlwaysSink


topology :: Topo.Topology Node
topology = topologyFromEdges [(Source, Crossing), (Crossing, Sink)]
