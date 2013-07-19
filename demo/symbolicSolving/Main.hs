module Main where

import qualified EFA.Application.Index as XIdx
import EFA.Application.Utility (makeEdges, constructSeqTopo)
import EFA.Application.Absolute ((=<>))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Application.Absolute as EqGen

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


given ::
   EqGen.SymbolicEquationSystem Node.Int s SumProduct.Term
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

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve seqTopo given

  Draw.xterm $ Draw.sequFlowGraphAbsWithEnv seqTopo env
