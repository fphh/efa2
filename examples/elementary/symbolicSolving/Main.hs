module Main where

import EFA.Example.Utility (recAbs, edgeVar, makeEdges, (=<>), constructSeqTopo)

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Equation.System as EqGen
import qualified EFA.Graph as Gr
import EFA.Equation.Variable (Index)
import EFA.Symbolic.SumProduct (Term)
import qualified EFA.Graph.Topology.Node as Node

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Node.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Node 0


topoDreibein :: TD.Topology Node.Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


{-
Use new Term type here since it simplifies automatically.
-}
given :: EqGen.EquationSystem Node.Node s (Term (Index Node.Node))
given =
   Idx.DTime recAbs Idx.initSection =<>
   Idx.DTime recAbs sec0 =<>

   Idx.Storage recAbs (Idx.SecNode sec0 node3) =<>

   edgeVar (Idx.Power recAbs) sec0 node3 node2 =<>
   edgeVar (Idx.X recAbs) sec0 node2 node3 =<>

   edgeVar (Idx.Eta recAbs) sec0 node3 node2 =<>
   edgeVar (Idx.Eta recAbs) sec0 node0 node2 =<>
   edgeVar (Idx.Eta recAbs) sec0 node2 node1 =<>

   mempty


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve given seqTopo

  Draw.sequFlowGraphAbsWithEnv seqTopo env
