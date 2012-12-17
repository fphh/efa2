module Main where

import EFA2.Example.ExampleHelper (makeEdges)

import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw (drawTopology)

import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Topology.EquationGenerator as EqGen
import qualified EFA2.Topology.Flow as Flow
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Interpreter.Env as Env
import EFA2.Signal.SequenceData (SequData(SequData))
import EFA2.Topology.EquationGenerator (makeVar)
import EFA2.Solver.Equation (mkVar)


rec :: Idx.Record
rec = Idx.Record Idx.Absolute

sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

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
        states = [1, 0, 1]
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData

given :: [(Env.Index, Double)]
given = [ (mkVar (Idx.DTime rec Idx.initSection), 1),
          (mkVar (Idx.DTime rec sec0), 1),
          (mkVar (Idx.DTime rec sec1), 1),
          (mkVar (Idx.DTime rec sec2), 1),


          (mkVar (Idx.Storage rec
                              (Idx.SecNode sec2 node3)), 10.0),

{-
          (makeVar Idx.Power (Idx.SecNode Idx.initSection node3)
                             (Idx.SecNode Idx.initSection Idx.rootNode), 3.0),
-}

          (makeVar Idx.Power (Idx.SecNode sec0 node2)
                             (Idx.SecNode sec0 node3), 4.0),


          (makeVar Idx.X (Idx.SecNode sec0 node2)
                         (Idx.SecNode sec0 node3), 0.32),

{-
          (makeVar Idx.X (Idx.SecNode sec1 node3)
                         (Idx.SecNode sec2 node3), 1),
-}

          (makeVar Idx.Power (Idx.SecNode sec1 node3)
                             (Idx.SecNode sec1 node2), 5),

          (makeVar Idx.Power (Idx.SecNode sec2 node3)
                             (Idx.SecNode sec2 node2), 6),

          (makeVar Idx.Power (Idx.SecNode sec3 node3)
                             (Idx.SecNode sec3 node2), 7),

          (makeVar Idx.Power (Idx.SecNode sec4 node3)
                             (Idx.SecNode sec4 node2), 8),



          (makeVar Idx.FEta (Idx.SecNode sec0 node3)
                            (Idx.SecNode sec0 node2), 0.25),
          (makeVar Idx.FEta (Idx.SecNode sec0 node2)
                            (Idx.SecNode sec0 node3), 0.25),


          (makeVar Idx.FEta (Idx.SecNode sec0 node2)
                            (Idx.SecNode sec0 node1), 0.5),

          (makeVar Idx.FEta (Idx.SecNode sec0 node0)
                            (Idx.SecNode sec0 node2), 0.75) ]


main :: IO ()
main = do

  let env = EqGen.solveSystem given seqTopo

  drawTopology seqTopo env

