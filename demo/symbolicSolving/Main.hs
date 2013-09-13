module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Utility (seqFlowGraphFromStates)

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
   Draw.xterm $ Draw.sequFlowGraph Draw.optionsDefault $
      EqSys.solve (seqFlowGraphFromStates Tripod.topology [1]) given
