
module EFA.Example.Topology.Tripod.State where

import qualified EFA.Example.Topology.TripodA as Tripod
import EFA.Example.Topology.TripodA (Node, node0, node1, node2, node3)

import EFA.Application.Utility (identifyFlowState, dirEdge, undirEdge)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence as SeqFlowPlain

import qualified EFA.Graph.Topology.Index as Idx
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.Sequence as Sequ

import EFA.Equation.Result (Result)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))


assembleFlowTopos ::
   [FlowTopology Node] -> SeqFlow.Graph Node (Result a) (Result v)
assembleFlowTopos =
   SeqFlow.graphFromPlain .
   SeqFlowPlain.sequenceGraph .
   Sequ.fromList


state0, state3 :: FlowTopology Node

state0 =
   identifyFlowState Tripod.topology
      [dirEdge node2 node1, dirEdge node2 node3]

state3 =
   identifyFlowState Tripod.topology
      [undirEdge node0 node2, dirEdge node2 node1]


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = assembleFlowTopos [state0, state3]
