
module Main where

import qualified EFA2.Topology.EfaGraph as Gr

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Set (Set)

import Text.Printf (printf)

import qualified EFA2.Solver.Equation as Equ
import EFA2.Solver.Equation
          (MkTermC, mkTerm,
           EqTerm, Term(Const), toAbsEquations, mapEqTermEnv)
import EFA2.Solver.EquationOrder (order)

import EFA2.Interpreter.Interpreter
          (eqToInTerm, interpretFromScratch, interpretTerm)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith (Val)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar, fromScalar)

import EFA2.Topology.Topology (makeAllEquations)
import EFA2.Topology.TopologyData
          (SequFlowGraph, NodeType(..), ELabel, defaultELabel)

import EFA2.Topology.Draw (drawDeltaTopology, drawTopology, drawAll)
import EFA2.Topology.TopologyData (SequFlowGraph, NodeType(..))


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


main :: IO ()
main = do
  let [num0] = numeric linearOne
  
  drawTopology linearOne num0
