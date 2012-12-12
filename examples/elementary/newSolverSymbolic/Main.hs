module Main where

import qualified Data.Map as M

import EFA2.StateAnalysis.StateAnalysis
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData as TD
import EFA2.Topology.EquationGenerator
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr
import EFA2.Signal.SequenceData
import EFA2.Interpreter.Env as Env
import EFA2.Solver.Equation

import EFA2.Interpreter.Env (Index)

import Data.Ratio (numerator, denominator, (%), approxRational)

import Debug.Trace


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

{-
given :: [(Env.Index, Double)]
given = [ (mkVar (Idx.DTime (Idx.Record Absolute) initSection), 1.0),
          (mkVar (Idx.DTime (Idx.Record Absolute) (Section 0)), 1.0),
          (makeVar Idx.Power (Idx.SecNode (Section 0) (Idx.Node 3))
                             (Idx.SecNode (Section 0) (Idx.Node 2)), 8.0),

          (mkVar (Idx.Storage (Idx.Record Absolute) 
                              (Idx.SecNode (Section 0) (Idx.Node 3))), 5.0),


          (makeVar Idx.X (Idx.SecNode (Section 0) (Idx.Node 2))
                         (Idx.SecNode (Section 0) (Idx.Node 3)), 0.6),



          (makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 3))
                            (Idx.SecNode (Section 0) (Idx.Node 2)), 0.25),

          (makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 0))
                            (Idx.SecNode (Section 0) (Idx.Node 2)), 0.75),

          (makeVar Idx.FEta (Idx.SecNode (Section 0) (Idx.Node 2))
                            (Idx.SecNode (Section 0) (Idx.Node 1)), 0.5) ]
-}


t :: EqTerm
t = Atom $ mkVar (Idx.DTime (Idx.Record Absolute) initSection)

given :: [(Env.Index, EqTerm)]
given  = map (\x -> (x, Atom x)) given'

xidx :: Idx.X
xidx =
   Idx.X (Idx.Record Absolute)
      (Idx.SecNode (Section 0) (Idx.Node 2))
      (Idx.SecNode (Section 0) (Idx.Node 0))

--given' :: [(Env.Index, Term Env.Index)]
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



emptyGiven :: [(Env.Index, Double)]
emptyGiven = []

main :: IO ()
main = do 

  --let sys = makeAllEquations seqTopo
  let env = solveSystem given seqTopo
      m = M.map (map showEqTerm) $ xMap env
  -- drawTopologySimple seqTopo

  --print (storageMap env)
  --drawTopology seqTopo env

  print (m M.! xidx)
