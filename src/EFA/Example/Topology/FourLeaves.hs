module EFA.Example.Topology.FourLeaves where

import EFA.Application.Utility (topologyFromEdges)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Report.Format as Format


data Node = Source0 | Source1 | Crossing | Storage | Sink
   deriving (Show, Eq, Ord, Enum)

source0, source1, crossing, storage, sink :: Node
source0  = Source0
source1  = Source1
storage  = Storage
sink     = Sink
crossing = Crossing

instance Node.C Node where
   display Source0  = Format.literal "Quelle0"
   display Source1  = Format.literal "Quelle1"
   display Storage  = Format.literal "Speicher"
   display Sink     = Format.literal "Senke"
   display Crossing = Format.literal "Kreuzung"

   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source0  = Node.Source
   typ Source1  = Node.Source
   typ Storage  = Node.Storage
   typ Sink     = Node.Sink
   typ Crossing = Node.Crossing


topology :: Topo.Topology Node
topology =
   topologyFromEdges
      [(source0, crossing), (source1, crossing),
       (crossing, storage), (crossing, sink)]
