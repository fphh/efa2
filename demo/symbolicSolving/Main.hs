module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Utility (constructSeqTopo)
import EFA.Application.Absolute ((=<>))

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Draw as Draw

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0


given ::
   EqGen.SymbolicEquationSystem Node s SumProduct.Term
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
main = do

  let seqTopo = constructSeqTopo Tripod.topology [1]
      env = EqGen.solve seqTopo given

  Draw.xterm $ Draw.sequFlowGraphAbsWithEnv seqTopo env
