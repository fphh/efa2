module Main where


import EFA2.Example.ExampleHelper (makeNodes, makeSimpleEdges)

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


topoDreibein :: Topology
topoDreibein = mkGraph (makeNodes ns) (makeSimpleEdges es)
  where ns = [(0, Source), (1, Sink), (2, Crossing), (3, TD.Storage)]
        es = [(0, 2), (1, 2), (2, 3)]


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
given' = [ mkVar (Idx.DTime (Idx.Record Absolute) initSection),
           mkVar (Idx.DTime (Idx.Record Absolute) (Section 0)),
           makeVar Idx.Power (Idx.SecNode (Section 0) (Idx.Node 3))
                             (Idx.SecNode (Section 0) (Idx.Node 2)),
 
           mkVar (Idx.Storage (Idx.Record Absolute) 
                              (Idx.SecNode (Section 0) (Idx.Node 3))),


           makeVar Idx.X (Idx.SecNode (Section 0) (Idx.Node 2))
                         (Idx.SecNode (Section 0) (Idx.Node 3)),



           makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 3))
                            (Idx.SecNode (Section 0) (Idx.Node 2)),

           makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 0))
                            (Idx.SecNode (Section 0) (Idx.Node 2)),

           makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 2))
                            (Idx.SecNode (Section 0) (Idx.Node 1)) ]

main :: IO ()
main = do 

  let env :: Env.Envs Env.SingleRecord [EqTerm]
      env = solveSystem given seqTopo

  drawAll [
    drawTopology seqTopo env,
    drawTopology seqTopo (fmap (map simplify) env) ]

