module EFA.Example.Topology.LinearOne where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Source | Sink deriving (Ord, Eq, Enum, Show)

instance Node.C Node where
   display Source   = Format.literal "Quelle"
   display Sink     = Format.literal "Senke"
   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source = Node.AlwaysSource
   typ Sink   = Node.AlwaysSink


topology :: Topo.Topology Node
topology = topologyFromEdges [(Source, Sink)]
