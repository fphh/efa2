
module Main where

import qualified EFA.Example.Topology.TripodA as Tripod
import EFA.Example.Topology.TripodA (Node, node0, node1, node2, node3)

import EFA.Application.Utility (seqFlowGraphFromStates, dirEdge, undirEdge)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (mconcat)


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


given :: EqSys.EquationSystemIgnore Node s Double Double
given =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :

   (XIdx.storage Idx.initial node3 .= 10.0) :

   (XIdx.power sec0 node0 node2 .= 4.0) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.power sec1 node3 node2 .= 4.0) :
   (XIdx.eta sec1 node2 node3 .= 0.25) :
   (XIdx.eta sec1 node0 node2 .= 0.75) :



   []

main :: IO ()
main =
   Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault $
      EqSys.solve
         (seqFlowGraphFromStates Tripod.topology
            [[dirEdge node0 node2, undirEdge node2 node3],
             [dirEdge node0 node2, undirEdge node1 node2]])
         given
