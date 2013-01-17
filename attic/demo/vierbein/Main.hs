
module Main where

import qualified EFA2.Solver.Equation as Equ
import EFA2.Solver.Equation (EqTerm, mkVar, formatTerm, mapEqTermEnv, toAbsEquations)
import EFA2.Solver.EquationOrder (order)
import EFA2.Topology.TopologyData (SequFlowGraph, NodeType(..), ELabel, defaultELabel)
import EFA2.Topology.Topology (makeAllEquations)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter (interpretFromScratch, eqToInTerm)
import qualified EFA2.Report.Format as Format
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (UTFSig)
import EFA2.Topology.Draw (drawTopology)
import EFA2.Topology.EfaGraph (mkGraph)

import qualified Data.Map as M


symbolic :: SequFlowGraph -> Envs NoRecord EqTerm
symbolic g = res
  where envs = emptyEnv { recordNumber = SingleRecord rec,
                          energyMap = sigs0sym,
                          dtimeMap = dtimes0sym,
                          xMap = x0sym,
                          fetaMap = eta0sym }

        (_, ts) = makeAllEquations g [envs]

        tso = toAbsEquations $ order ts
        res = Equ.interpretEqTermFromScratch tso

numeric :: SequFlowGraph -> Envs SingleRecord UTFSig
numeric g = res
  where envs = emptyEnv { recordNumber = SingleRecord rec,
                          energyMap = sigs0num,
                          dtimeMap = dtimes0num,
                          xMap = x0num,
                          fetaMap = eta0num }

        (envs', ts) = makeAllEquations g [envs]

        tso = toAbsEquations $ order ts
        res = interpretFromScratch (SingleRecord rec) 1 (map (eqToInTerm envs') tso)


main :: IO ()
main = do
  let numres = numeric vierbein
      symres = symbolic vierbein

  print (mapEqTermEnv (Format.unUnicode . formatTerm) symres)
  print (mapEqTermEnv (Format.unUnicode . Format.list . map formatTerm . Equ.additiveTerms . Equ.pushMult) symres)

  drawTopology vierbein numres
  -- drawTopology vierbein symres -- waere noch zu schreiben...


-- Vierbein example ------------------------------------------------------------

--type Sc = Scal (Typ UT UT UT) Val


sec :: Idx.Section
sec = Idx.Section 0

rec :: Idx.Record
rec = Idx.Record 0

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Int -> Int -> idx
edgeIdx mkIdx x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))


dtimes0num :: DTimeMap UTFSig
dtimes0num = M.fromList [ (Idx.DTime rec sec, S.fromList [1.0]) ]

sigs0num :: EnergyMap UTFSig
sigs0num = M.fromList [ (edgeIdx Idx.Energy 0 2, S.fromList [3.0]) ]


eta0num :: FEtaMap UTFSig
eta0num = M.fromList $
   (edgeIdx Idx.FEta 0 2, S.map $ const 0.8) :
   (edgeIdx Idx.FEta 2 0, S.map $ const 0.8) :
   (edgeIdx Idx.FEta 1 2, S.map $ const 0.7) :
   (edgeIdx Idx.FEta 2 1, S.map $ const 0.7) :
   (edgeIdx Idx.FEta 2 3, S.map $ const 0.6) :
   (edgeIdx Idx.FEta 3 2, S.map $ const 0.6) :
   (edgeIdx Idx.FEta 2 4, S.map $ const 0.5) :
   (edgeIdx Idx.FEta 4 2, S.map $ const 0.5) :
   []


x0num :: XMap UTFSig
x0num = M.fromList $
   (edgeIdx Idx.X 2 0, S.fromList [0.2]) :
   (edgeIdx Idx.X 2 3, S.fromList [0.5]) :
   (edgeIdx Idx.X 2 4, S.fromList [0.5]) :
   []

---------------------------------------------------------------------------------



dtimes0sym :: DTimeMap EqTerm
dtimes0sym = M.fromList [ (Idx.DTime rec sec, mkVar $ Idx.DTime rec sec) ]

sigs0sym :: EnergyMap EqTerm
sigs0sym = M.fromList [ (edgeIdx Idx.Energy 0 2, mkVar $ edgeIdx Idx.Energy 0 1) ]


eta0sym :: FEtaMap EqTerm
eta0sym =
   M.fromList $
      (edgeIdx Idx.FEta 0 2, const $ mkVar $ edgeIdx Idx.FEta 0 2) :
      (edgeIdx Idx.FEta 2 0, const $ mkVar $ edgeIdx Idx.FEta 2 0) :
      (edgeIdx Idx.FEta 1 2, const $ mkVar $ edgeIdx Idx.FEta 1 2) :
      (edgeIdx Idx.FEta 2 1, const $ mkVar $ edgeIdx Idx.FEta 2 1) :
      (edgeIdx Idx.FEta 2 3, const $ mkVar $ edgeIdx Idx.FEta 2 3) :
      (edgeIdx Idx.FEta 3 2, const $ mkVar $ edgeIdx Idx.FEta 3 2) :
      (edgeIdx Idx.FEta 2 4, const $ mkVar $ edgeIdx Idx.FEta 2 4) :
      (edgeIdx Idx.FEta 4 2, const $ mkVar $ edgeIdx Idx.FEta 4 2) :
      []


x0sym :: XMap EqTerm
x0sym =
   M.fromList $
      (edgeIdx Idx.X 2 0, mkVar $ edgeIdx Idx.X 2 0) :
      (edgeIdx Idx.X 2 3, mkVar $ edgeIdx Idx.X 2 3) :
      (edgeIdx Idx.X 2 4, mkVar $ edgeIdx Idx.X 2 4) :
      []


makeNode :: Int -> Idx.SecNode
makeNode = Idx.SecNode sec . Idx.Node

makeNodes :: [(Int, NodeType)] -> [Gr.LNode Idx.SecNode NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)

makeEdges :: [(Int, Int)] -> [Gr.LEdge Idx.SecNode ELabel]
makeEdges es = map f es
  where f (a, b) = (Gr.Edge (makeNode a) (makeNode b), defaultELabel)


vierbein :: SequFlowGraph
vierbein = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Source), (2, Crossing), (3, Sink), (4, Sink)]
        es = makeEdges [(0, 2), (1, 2), (2, 3), (2, 4)]
