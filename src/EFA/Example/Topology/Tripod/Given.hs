module EFA.Example.Topology.Tripod.Given where

import qualified EFA.Example.Topology.Tripod as Tripod
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)
import EFA.Application.Utility (seqFlowGraphFromStates, dirEdge)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.Sequence.Absolute ( (.=) )

import EFA.Equation.Result (Result)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (Monoid, mconcat)


sec0, sec1, sec2 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ _ = Stream.enumFrom $ Idx.section0


equationSystem :: EqSys.EquationSystemIgnore Node s Double Double
equationSystem =
   mconcat $

   (XIdx.dTime sec0 .= 0.5) :
   (XIdx.dTime sec1 .= 2) :
   (XIdx.dTime sec2 .= 1) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= 10.0) :


   (XIdx.power sec0 node2 node3 .= 4.0) :

   (XIdx.x sec0 node2 node3 .= 0.32) :

   (XIdx.power sec1 node3 node2 .= 5) :
   (XIdx.power sec2 node3 node2 .= 6) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.eta sec1 node2 node1 .= 0.5) :
   (XIdx.eta sec1 node0 node2 .= 0.75) :
   (XIdx.power sec1 node1 node2 .= 4.0) :


   (XIdx.eta sec2 node3 node2 .= 0.75) :
   (XIdx.eta sec2 node2 node1 .= 0.5) :
   (XIdx.eta sec2 node0 node2 .= 0.75) :
   (XIdx.power sec2 node1 node2 .= 4.0) :

   (XIdx.eta sec1 node2 node3 .= 0.25) :

   []

seqFlowGraph :: SeqFlow.Graph Node (Result a) (Result v)
seqFlowGraph =
   seqFlowGraphFromStates Tripod.topology
      [[dirEdge node0 node2, dirEdge node3 node2],
       [dirEdge node2 node1, dirEdge node2 node3],
       [dirEdge node0 node2, dirEdge node3 node2]]
