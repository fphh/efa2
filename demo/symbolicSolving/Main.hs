module Main where

import qualified EFA.Example.Topology.Tripod as Tripod
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)
import EFA.Application.Utility (seqFlowGraphFromStates, dirEdge)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((=<>))

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Graph.Topology.Index as Idx

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0


given ::
   EqSys.SymbolicEquationSystemIgnore Node s SumProduct.Term
given =
   XIdx.dTime sec0 =<>

   XIdx.storage (Idx.afterSection sec0) node3 =<>

   XIdx.power sec0 node3 node2 =<>
   XIdx.x sec0 node2 node3 =<>

   XIdx.eta sec0 node3 node2 =<>
   XIdx.eta sec0 node0 node2 =<>
   XIdx.eta sec0 node2 node1 =<>

   mempty


main :: IO ()
main =
   Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault $
      EqSys.solve
         (seqFlowGraphFromStates Tripod.topology
            [[dirEdge node0 node2, dirEdge node3 node2]])
         given
