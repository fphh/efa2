
module Main where

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder (order)
import EFA2.Topology.TopologyData (Topology, NodeType(..), defaultELabel)
import EFA2.Topology.Topology (makeAllEquations, makeNodes, makeEdges)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter (interpretFromScratch, eqToInTerm)
import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (UTFSig)
import EFA2.Topology.Draw (drawTopology)
import EFA2.Topology.EfaGraph (mkGraph)

import qualified Data.Map as M


symbolic :: Topology -> Envs NoRecord EqTerm
symbolic g = res
  where envs = emptyEnv { recordNumber = SingleRecord rec,
                          energyMap = sigs0sym,
                          dtimeMap = dtimes0sym,
                          xMap = x0sym,
                          fetaMap = eta0sym }

        (_, ts) = makeAllEquations g [envs]

        tso = toAbsEquations $ order ts
        res = interpretEqTermFromScratch tso

numeric :: Topology -> Envs SingleRecord UTFSig
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

  print (mapEqTermEnv showEqTerm symres)
  print (mapEqTermEnv (showEqTerms . additiveTerms . pushMult) symres)

  drawTopology vierbein numres
  -- drawTopology vierbein symres -- waere noch zu schreiben...


-- Vierbein example ------------------------------------------------------------

--type Sc = Scal (Typ UT UT UT) Val


sec :: Idx.Section
sec = Idx.Section 0

rec :: Idx.Record
rec = Idx.Record 0

dtimes0num :: DTimeMap UTFSig
dtimes0num = M.fromList [ (Idx.DTime sec rec, S.fromList [1.0]) ]

sigs0num :: EnergyMap UTFSig
sigs0num = M.fromList [ (Idx.Energy sec rec 0 2, S.fromList [3.0]) ]


eta0num :: FEtaMap UTFSig
eta0num = M.fromList $
   (Idx.FEta sec rec 0 2, S.map $ const 0.8) :
   (Idx.FEta sec rec 2 0, S.map $ const 0.8) :
   (Idx.FEta sec rec 1 2, S.map $ const 0.7) :
   (Idx.FEta sec rec 2 1, S.map $ const 0.7) :
   (Idx.FEta sec rec 2 3, S.map $ const 0.6) :
   (Idx.FEta sec rec 3 2, S.map $ const 0.6) :
   (Idx.FEta sec rec 2 4, S.map $ const 0.5) :
   (Idx.FEta sec rec 4 2, S.map $ const 0.5) :
   []


x0num :: XMap UTFSig
x0num = M.fromList $
   (Idx.X sec rec 2 0, S.fromList [0.2]) :
   (Idx.X sec rec 2 3, S.fromList [0.5]) :
   (Idx.X sec rec 2 4, S.fromList [0.5]) :
   []

---------------------------------------------------------------------------------



dtimes0sym :: DTimeMap EqTerm
dtimes0sym = M.fromList [ (Idx.DTime sec rec, mkVar $ Idx.DTime sec rec) ]

sigs0sym :: EnergyMap EqTerm
sigs0sym = M.fromList [ (Idx.Energy sec rec 0 2, mkVar $ Idx.Energy sec rec 0 1) ]


eta0sym :: FEtaMap EqTerm
eta0sym =
   M.fromList $
      (Idx.FEta sec rec 0 2, const $ mkVar $ Idx.FEta sec rec 0 2) :
      (Idx.FEta sec rec 2 0, const $ mkVar $ Idx.FEta sec rec 2 0) :
      (Idx.FEta sec rec 1 2, const $ mkVar $ Idx.FEta sec rec 1 2) :
      (Idx.FEta sec rec 2 1, const $ mkVar $ Idx.FEta sec rec 2 1) :
      (Idx.FEta sec rec 2 3, const $ mkVar $ Idx.FEta sec rec 2 3) :
      (Idx.FEta sec rec 3 2, const $ mkVar $ Idx.FEta sec rec 3 2) :
      (Idx.FEta sec rec 2 4, const $ mkVar $ Idx.FEta sec rec 2 4) :
      (Idx.FEta sec rec 4 2, const $ mkVar $ Idx.FEta sec rec 4 2) :
      []


x0sym :: XMap EqTerm
x0sym =
   M.fromList $
      (Idx.X sec rec 2 0, mkVar $ Idx.X sec rec 2 0) :
      (Idx.X sec rec 2 3, mkVar $ Idx.X sec rec 2 3) :
      (Idx.X sec rec 2 4, mkVar $ Idx.X sec rec 2 4) :
      []


vierbein :: Topology
vierbein = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Source), (2, Crossing), (3, Sink), (4, Sink)]
        es = makeEdges [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel), (2, 4, defaultELabel)]

