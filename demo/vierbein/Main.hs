
module Main where

import qualified Data.Map as M

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder (order)
import EFA2.Topology.TopologyData (Topology, NodeType(..), defaultELabel, mkGraph)
import EFA2.Topology.Topology (makeAllEquations, makeNodes, makeEdges)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter (interpretFromScratch, eqToInTerm)
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (UTFSig)
import EFA2.Display.DrawGraph (drawTopology)


symbolic :: Topology -> Envs EqTerm
symbolic g = res
  where envs = emptyEnv { recordNumber = SingleRecord 0,
                          energyMap = sigs0sym,
                          dtimeMap = dtimes0sym,
                          xMap = x0sym,
                          fetaMap = eta0sym }

        (_, ts) = makeAllEquations g [envs]

        tso = toAbsEqTermEquations $ order ts
        res = interpretEqTermFromScratch tso

numeric :: Topology -> Envs UTFSig
numeric g = res
  where envs = emptyEnv { recordNumber = SingleRecord 0,
                          energyMap = sigs0num,
                          dtimeMap = dtimes0num,
                          xMap = x0num,
                          fetaMap = eta0num }

        (envs', ts) = makeAllEquations g [envs]

        tso = toAbsEqTermEquations $ order ts
        res = interpretFromScratch (SingleRecord 0) 1 (map (eqToInTerm envs') tso)


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


dtimes0num :: DTimeMap UTFSig
dtimes0num = M.fromList [ (DTimeIdx 0 0, S.fromList [1.0]) ]

sigs0num :: EnergyMap UTFSig
sigs0num = M.fromList [ (EnergyIdx 0 0 0 2, S.fromList [3.0]) ]


eta0num :: FEtaMap UTFSig
eta0num = M.fromList [ (FEtaIdx 0 0 0 2, S.map $ const 0.8),
                       (FEtaIdx 0 0 2 0, S.map $ const 0.8),
                       (FEtaIdx 0 0 1 2, S.map $ const 0.7),
                       (FEtaIdx 0 0 2 1, S.map $ const 0.7),
                       (FEtaIdx 0 0 2 3, S.map $ const 0.6),
                       (FEtaIdx 0 0 3 2, S.map $ const 0.6),
                       (FEtaIdx 0 0 2 4, S.map $ const 0.5),
                       (FEtaIdx 0 0 4 2, S.map $ const 0.5) ]


x0num :: XMap UTFSig
x0num = M.fromList [ (XIdx 0 0 2 0, S.fromList [0.2]),
                     (XIdx 0 0 2 3, S.fromList [0.5]),
                     (XIdx 0 0 2 4, S.fromList [0.5]) ]

---------------------------------------------------------------------------------



dtimes0sym :: DTimeMap EqTerm
dtimes0sym = M.fromList [ (DTimeIdx 0 0, DTime (DTimeIdx 0 0)) ]

sigs0sym :: EnergyMap EqTerm
sigs0sym = M.fromList [ (EnergyIdx 0 0 0 2, Energy (EnergyIdx 0 0 0 1)) ]


eta0sym :: FEtaMap EqTerm
eta0sym = M.fromList [ (FEtaIdx 0 0 0 2, const $ FEta (FEtaIdx 0 0 0 2)),
                       (FEtaIdx 0 0 2 0, const $ FEta (FEtaIdx 0 0 2 0)),
                       (FEtaIdx 0 0 1 2, const $ FEta (FEtaIdx 0 0 1 2)),
                       (FEtaIdx 0 0 2 1, const $ FEta (FEtaIdx 0 0 2 1)),
                       (FEtaIdx 0 0 2 3, const $ FEta (FEtaIdx 0 0 2 3)),
                       (FEtaIdx 0 0 3 2, const $ FEta (FEtaIdx 0 0 3 2)),
                       (FEtaIdx 0 0 2 4, const $ FEta (FEtaIdx 0 0 2 4)),
                       (FEtaIdx 0 0 4 2, const $ FEta (FEtaIdx 0 0 4 2)) ]


x0sym :: XMap EqTerm
x0sym = M.fromList [ (XIdx 0 0 2 0, X (XIdx 0 0 2 0)),
                     (XIdx 0 0 2 3, X (XIdx 0 0 2 3)),
                     (XIdx 0 0 2 4, X (XIdx 0 0 2 4)) ]


vierbein :: Topology
vierbein = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Source), (2, Crossing), (3, Sink), (4, Sink)]
        es = makeEdges [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel), (2, 4, defaultELabel)]

