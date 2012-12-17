module Main where

import EFA2.Example.ExampleHelper (makeEdges)


import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))
import EFA2.StateAnalysis.StateAnalysis (bruteForce)
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData as TD
import EFA2.Topology.EquationGenerator
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr
import EFA2.Signal.SequenceData
import qualified EFA2.Interpreter.Env as Env
import EFA2.Solver.Equation


rec :: Idx.Record
rec = Idx.Record Idx.Absolute

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


seqTopo :: SequFlowGraph
seqTopo = mkSeqTopo (select sol states)
  where sol = bruteForce topoDreibein  -- ehemals stateAnalysis
        states = [1]
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData

given :: [(Env.Index, EqTerm)]
given  = map (\x -> (x, Atom x)) given'


given' :: [Env.Index]
given' = [ mkVar (Idx.DTime rec Idx.initSection),
           mkVar (Idx.DTime rec sec0),
           makeVar Idx.Power (Idx.SecNode sec0 node3)
                             (Idx.SecNode sec0 node2),

           mkVar (Idx.Storage rec
                              (Idx.SecNode sec0 node3)),


           makeVar Idx.X (Idx.SecNode sec0 node2)
                         (Idx.SecNode sec0 node3),



           makeVar Idx.FEta (Idx.SecNode sec0 node3)
                            (Idx.SecNode sec0 node2),

           makeVar Idx.FEta (Idx.SecNode sec0 node0)
                            (Idx.SecNode sec0 node2),

           makeVar Idx.FEta (Idx.SecNode sec0 node2)
                            (Idx.SecNode sec0 node1) ]

main :: IO ()
main = do 

  let env :: Env.Envs Env.SingleRecord [EqTerm]
      env = solveSystem given seqTopo

  drawAll [
    drawTopology seqTopo env,
    drawTopology seqTopo (fmap (map simplify) env) ]

