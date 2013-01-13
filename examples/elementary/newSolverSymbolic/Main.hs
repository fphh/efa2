module Main where

import EFA2.Example.Utility (rec, edgeVar, makeEdges, (=<>), constructSeqTopo)

import EFA2.Topology.Draw (drawTopology)

import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Topology.EquationGenerator as EqGen
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Interpreter.Env as Env
import EFA2.Solver.Term (Term)

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Idx.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Idx.Node 0


topoDreibein :: TD.Topology
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


{-
Use new Term type here since it simplifies automatically.
-}
given :: EqGen.EquationSystem s (Term Env.Index)
given =
   Idx.DTime rec Idx.initSection =<>
   Idx.DTime rec sec0 =<>

   Idx.Storage rec (Idx.SecNode sec0 node3) =<>

   edgeVar (Idx.Power rec) sec0 node3 node2 =<>
   edgeVar (Idx.X rec) sec0 node2 node3 =<>

   edgeVar (Idx.Eta rec) sec0 node3 node2 =<>
   edgeVar (Idx.Eta rec) sec0 node0 node2 =<>
   edgeVar (Idx.Eta rec) sec0 node2 node1 =<>

   mempty


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solveSystem given seqTopo

  drawTopology seqTopo env
