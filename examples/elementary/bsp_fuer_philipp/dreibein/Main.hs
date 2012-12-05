
module Main where

import qualified Data.Map as M
import qualified Data.List as L

import qualified EFA2.Example.Examples as Example
import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData
import EFA2.Topology.Topology
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.SequenceData


import qualified EFA2.Signal.Index as Idx
import EFA2.Signal.Signal (Sc, toScalar, fromScalar)
import EFA2.Interpreter.Env as Env
import EFA2.Solver.EquationOrder (order)
import EFA2.Solver.Equation
          (MkTermC, mkTerm, showAbsAssign, showEquations,
           EqTerm, Term(Const), toAbsEquations, mapEqTermEnv)
import EFA2.Topology.Draw (drawDeltaTopology, drawTopology, drawAll)

import EFA2.Interpreter.Interpreter
          (eqToInTerm, interpretFromScratch, interpretTerm)
import qualified EFA2.Signal.Signal as S

import Debug.Trace

--showAbsAssigns :: [Assign] -> String
showAbsAssigns ts = L.intercalate "\n" $ map showAbsAssign ts


{-
edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Record -> Int -> Int -> idx
edgeIdx mkIdx rec x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))

makeNode :: Int -> Idx.SecNode
makeNode = Idx.SecNode sec . Idx.Node

makeNodes :: [(Int, NodeType)] -> [Gr.LNode Idx.SecNode NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)

makeEdges :: [(Int, Int)] -> [Gr.LEdge Idx.SecNode ELabel]
makeEdges es = map f es
  where f (a, b) = (Gr.Edge (makeNode a) (makeNode b), defaultELabel)


rec0 :: Idx.Record
rec0 = Idx.Record 0

sec :: Idx.Section
sec = Idx.Section 0

linearOne :: SequFlowGraph
linearOne = Gr.mkGraph ns es
  where ns = makeNodes  [(0, Source), (1, Crossing), (2, Sink)]
        es = makeEdges [(0, 1), (1, 2)]

dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (Idx.DTime rec0 sec, toScalar 1.0) ]

power0num :: PowerMap Sc
power0num = M.fromList [ (edgeIdx Idx.Power rec0 0 1, toScalar 3.0) ]


eta0num :: FEtaMap Sc
eta0num = M.fromList $
   (edgeIdx Idx.FEta rec0 0 1, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 1 2, S.map $ const 0.7) : []


envs0num :: Envs SingleRecord Sc
envs0num = emptyEnv { recordNumber = SingleRecord rec0,
                      powerMap = power0num,
                      dtimeMap = dtimes0num,
                      fetaMap = eta0num }

-- res ist vom Typ (Envs MixedRecord Sc)
-- separateEnvs gibt eine List von (Envs SingleRecord Sc) zurück
numeric :: SequFlowGraph -> [Envs SingleRecord Sc]
numeric g = separateEnvs res
  where (envs0, ts0) = makeAllEquations g [envs0num]

        ts0o = order ts0

        ts = toAbsEquations ts0o
        res = interpretFromScratch (recordNumber envs0) 0 (map (eqToInTerm envs0) ts)
-}

rec0 :: Idx.Record
rec0 = Idx.Record 0

sec :: Int -> Idx.Section
sec = Idx.Section

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Idx.Record -> Int -> Int -> idx
edgeIdx mkIdx sec rec x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))

dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList $
  (Idx.DTime rec0 (sec (-1)), toScalar 1.0) :
  (Idx.DTime rec0 (sec 0), toScalar 0.7) :
  (Idx.DTime rec0 (sec 1), toScalar 1.4) :
  (Idx.DTime rec0 (sec 2), toScalar 0.9) :
  (Idx.DTime rec0 (sec 3), toScalar 1.1) : []


power0num :: PowerMap Sc
power0num = M.fromList $
  (edgeIdx Idx.Power (sec (-1)) rec0 3 (-1), toScalar 3.0) :
  (edgeIdx Idx.Power (sec 0) rec0 2 3, toScalar 2.1) :
  (edgeIdx Idx.Power (sec 1) rec0 3 2, toScalar 2.6) :
  (edgeIdx Idx.Power (sec 2) rec0 2 3, toScalar 0.9) :
  (edgeIdx Idx.Power (sec 3) rec0 3 2, toScalar 3.1) :
 -- (edgeIdx Idx.Power (sec 0) rec0 2 1, toScalar 1.1) :
 -- (edgeIdx Idx.Power (sec 1) rec0 1 2, toScalar 1.6) :
 -- (edgeIdx Idx.Power (sec 2) rec0 1 2, toScalar 0.7) :
 -- (edgeIdx Idx.Power (sec 3) rec0 1 2, toScalar 2.1) :
  (edgeIdx Idx.Power (sec 0) rec0 0 2, toScalar 3.1) :
  (edgeIdx Idx.Power (sec 1) rec0 0 2, toScalar 3.6) :
  (edgeIdx Idx.Power (sec 2) rec0 0 2, toScalar 2.7) :
  (edgeIdx Idx.Power (sec 3) rec0 0 2, toScalar 4.1) : []


eta0num :: FEtaMap Sc
eta0num = M.fromList $
  (edgeIdx Idx.FEta (sec 0) rec0 3 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 3 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 3 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 3 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 0) rec0 2 1, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 2 1, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 2 1, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 2 1, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 0) rec0 2 0, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 2 0, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 2 0, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 2 0, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 0) rec0 2 3, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 2 3, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 2 3, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 2 3, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 0) rec0 1 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 1 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 1 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 1 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 0) rec0 0 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 1) rec0 0 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 2) rec0 0 2, S.map $ const 0.8) :
  (edgeIdx Idx.FEta (sec 3) rec0 0 2, S.map $ const 0.8) : []


x0num :: XMap Sc
x0num = M.fromList $
  (edgeIdx Idx.X (sec 0) rec0 2 1, toScalar 0.8) :
  (edgeIdx Idx.X (sec 1) rec0 2 1, toScalar 0.3) :
  (edgeIdx Idx.X (sec 2) rec0 2 1, toScalar 0.6) : []


envs0num :: Envs SingleRecord Sc
envs0num = emptyEnv { Env.recordNumber = SingleRecord rec0,
                      powerMap = power0num,
                      dtimeMap = dtimes0num,
                      fetaMap = eta0num,
                      xMap = x0num }

numeric :: SequFlowGraph -> [Envs SingleRecord Sc]
numeric g = trace (showEquations ts0) $ separateEnvs res
  where (envs0, ts0) = makeAllEquations g [envs0num]

        ts0o = order ts0

        ts = toAbsEquations ts0o
        res = interpretFromScratch (Env.recordNumber envs0) 0 (map (eqToInTerm envs0) ts)

seqTopo :: SequFlowGraph
seqTopo = mkSeqTopo (select sol states)
  where sol = StateAnalysis.bruteForce Example.topoDreibein
        states = [0, 5, 0, 4]
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData

main :: IO ()
main = do
  let [num0] = numeric seqTopo
  drawTopology seqTopo num0
