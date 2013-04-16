module Main where

import qualified EFA.Example.Index as XIdx
import EFA.Example.Utility
          (ScalarTerm, SignalTerm,
           makeEdges, constructSeqTopo, (=<>), (.=))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Record as Record
import EFA.Equation.Arithmetic (zero)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import Data.Monoid (mempty, (<>))


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


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   EqGen.EquationSystem Record.Delta Node.Int s
      (ScalarTerm Record.Delta SumProduct.Term Node.Int)
      (SignalTerm Record.Delta SumProduct.Term Node.Int)
given =
   (Idx.delta (XIdx.dTime sec0) .= zero) <>

   Idx.before (XIdx.dTime sec0) =<>

   Idx.before (XIdx.storage Idx.initial node3) =<>
   Idx.delta (XIdx.storage (Idx.afterSection sec0) node3) =<>

   Idx.after (XIdx.power sec0 node3 node2) =<>
   Idx.after (XIdx.x sec0 node2 node3) =<>

   Idx.before (XIdx.power sec0 node3 node2) =<>
   Idx.before (XIdx.x sec0 node2 node3) =<>

   Idx.before (XIdx.eta sec0 node3 node2) =<>
   Idx.before (XIdx.eta sec0 node0 node2) =<>
   Idx.before (XIdx.eta sec0 node2 node1) =<>

   Idx.after (XIdx.eta sec0 node3 node2) =<>
   Idx.delta (XIdx.eta sec0 node0 node2) =<>
   Idx.delta (XIdx.eta sec0 node2 node1) =<>

   mempty


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve seqTopo given

  Draw.xterm $ Draw.sequFlowGraphDeltaWithEnv seqTopo env
