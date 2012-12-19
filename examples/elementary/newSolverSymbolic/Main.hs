module Main where

import EFA2.Example.ExampleHelper (rec, edgeVar, makeEdges, (=<>))

import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw (drawAll, drawTopology)

import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Topology.EquationGenerator as EqGen
import qualified EFA2.Topology.Flow as Flow
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Interpreter.Env as Env
import EFA2.Signal.SequenceData (SequData(SequData))
import EFA2.Solver.Equation (EqTerm, simplify)

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


seqTopo :: TD.SequFlowGraph
seqTopo = mkSeqTopo (select sol states)
  where sol = StateAnalysis.bruteForce topoDreibein
        states = [1]
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData


given :: EqGen.EquationSystem s EqTerm
given =
   Idx.DTime rec Idx.initSection =<>
   Idx.DTime rec sec0 =<>

   Idx.Storage rec (Idx.SecNode sec0 node3) =<>

   edgeVar (Idx.Power rec) sec0 node3 node2 =<>
   edgeVar (Idx.X rec) sec0 node2 node3 =<>

   edgeVar (Idx.FEta rec) sec0 node3 node2 =<>
   edgeVar (Idx.FEta rec) sec0 node0 node2 =<>
   edgeVar (Idx.FEta rec) sec0 node2 node1 =<>

   mempty


main :: IO ()
main = do

  let env :: Env.Envs Env.SingleRecord [EqTerm]
      env = EqGen.solveSystem given seqTopo

  drawAll [
    drawTopology seqTopo env,
    drawTopology seqTopo (fmap (map simplify) env) ]

