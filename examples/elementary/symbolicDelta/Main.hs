module Main where

import EFA.Example.Utility
          (Term, edgeVar, makeEdges, constructSeqTopo, (=<>), (.=))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Env as Env

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
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Int 0


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   EqGen.EquationSystem Env.Delta Node.Int s
      (Term SumProduct.Term Idx.Delta Node.Int)
given =
   (Idx.delta (Idx.DTime Idx.initSection) .= 0) <>
   (Idx.delta (Idx.DTime sec0) .= 0) <>

   Idx.before (Idx.DTime Idx.initSection) =<>
   Idx.before (Idx.DTime sec0) =<>

   Idx.before (Idx.Storage (Idx.SecNode sec0 node3)) =<>

   Idx.after (edgeVar Idx.Power sec0 node3 node2) =<>
   Idx.after (edgeVar Idx.X sec0 node2 node3) =<>

   Idx.before (edgeVar Idx.Power sec0 node3 node2) =<>
   Idx.before (edgeVar Idx.X sec0 node2 node3) =<>

   Idx.before (edgeVar Idx.Eta sec0 node3 node2) =<>
   Idx.before (edgeVar Idx.Eta sec0 node0 node2) =<>
   Idx.before (edgeVar Idx.Eta sec0 node2 node1) =<>

   Idx.after (edgeVar Idx.Eta sec0 node3 node2) =<>
   Idx.delta (edgeVar Idx.Eta sec0 node0 node2) =<>
   Idx.delta (edgeVar Idx.Eta sec0 node2 node1) =<>

   mempty


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve given seqTopo

  Draw.sequFlowGraphDeltaWithEnv seqTopo env
