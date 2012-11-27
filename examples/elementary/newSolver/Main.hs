module Main where

import qualified Data.List as L
import qualified Data.Map as M

-- import EFA2.Example.Examples
import EFA2.StateAnalysis.StateAnalysis
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData as TD
--import EFA2.Topology.Topology2
import EFA2.Topology.EquationGenerator
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr
import EFA2.Signal.SequenceData
import EFA2.Interpreter.Env as Env
import EFA2.Solver.Equation (mkVar)


import UniqueLogic.ST.Expression
import UniqueLogic.ST.Rule
import UniqueLogic.ST.System

import Control.Monad.ST
import Control.Monad

sec :: Idx.Section
sec = Idx.Section 0

makeNode :: Int -> Idx.Node
makeNode = Idx.Node

makeNodes :: [(Int, NodeType)] -> [Gr.LNode Idx.Node NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)

makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Edge (Idx.Node a) (Idx.Node b), ())


topoDreibein :: Topology
topoDreibein = mkGraph (makeNodes nodes) (makeSimpleEdges edges)
  where nodes = [(0, Source), (1, Sink), (2, Crossing), (3, TD.Storage)]
        edges = [(0, 2), (1, 2), (2, 3)]


seqTopo :: SequFlowGraph
seqTopo = mkSeqTopo (select sol states)
  where sol = bruteForce topoDreibein  -- ehemals stateAnalysis
        states = [1]
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData

given :: [(Env.Index, Double)]
given = [ (mkVar (Idx.DTime (Idx.Record Absolute) initSection), 1.0),
          (mkVar (Idx.DTime (Idx.Record Absolute) (Section 0)), 1.0),
          --(mkVar (Idx.DTime (Idx.Record Absolute) (Section 1)), 1.0),
          --(mkVar (Idx.DTime (Idx.Record Absolute) (Section 2)), 1.0),
          --(mkVar (Idx.DTime (Idx.Record Absolute) (Section 3)), 1.0),
          --(mkVar (Idx.DTime (Idx.Record Absolute) (Section 4)), 1.0),


          --(makeVar Idx.Power (Idx.SecNode initSection (Idx.Node 3))
           --                  (Idx.SecNode initSection (Idx.Node (-1))), 4.0),

          (makeVar Idx.Power (Idx.SecNode (Section 0) (Idx.Node 3))
                             (Idx.SecNode (Section 0) (Idx.Node 2)), 4.0),

          (makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 3))
                            (Idx.SecNode (Section 0) (Idx.Node 2)), 0.4),

          (makeVar Idx.X (Idx.SecNode (Section 0) (Idx.Node 2))
                         (Idx.SecNode (Section 0) (Idx.Node 0)), 0.32),

          (makeVar Idx.Power (Idx.SecNode (Section 0) (Idx.Node 1))
                             (Idx.SecNode (Section 0) (Idx.Node 2)), 2.0),


          (makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 1))
                            (Idx.SecNode (Section 0) (Idx.Node 2)), 0.4) ]


main :: IO ()
main = do 

  --let sys = makeAllEquations seqTopo
  let env = solveSystem given seqTopo
  -- drawTopologySimple seqTopo

  drawTopology seqTopo env

