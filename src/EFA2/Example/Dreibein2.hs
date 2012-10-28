

module EFA2.Example.Dreibein2 where

import Data.Graph.Inductive
import qualified Data.Map as M


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar)
import EFA2.Solver.Equation (EqTerm, mkVar)


sec :: Idx.Section
sec = Idx.Section 0

rec0, rec1 :: Idx.Record
rec0 = Idx.Record 0
rec1 = Idx.Record 1


dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx sec rec0, toScalar 1.0) ]

sigs0num :: PowerMap Sc
sigs0num = M.fromList [ (PowerIdx sec rec0 0 1, toScalar 3.0),
                        (PowerIdx sec rec0 1 2, toScalar 1.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx sec rec0 1 0, S.map $ const 0.8),
                      (FEtaIdx sec rec0 0 1, S.map $ const 0.8),
                      (FEtaIdx sec rec0 1 2, S.map $ const 0.8),
                      (FEtaIdx sec rec0 2 1, S.map $ const 0.8),
                      (FEtaIdx sec rec0 1 3, S.map $ const 0.8),
                      (FEtaIdx sec rec0 3 1, S.map $ const 0.8) ]
x0num :: XMap Sc
x0num = M.fromList []

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx sec rec1, toScalar 1.0) ]


sigs1num :: PowerMap Sc
sigs1num = M.fromList [ (PowerIdx sec rec1 0 1, toScalar 3.5),
                        (PowerIdx sec rec1 1 2, toScalar 2.0) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx sec rec1 0 1, toScalar 0.5),
                          (DPowerIdx sec rec1 1 2, toScalar 1.0) ]

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx sec rec1 1 0, S.map $ const 0.9),
                       (FEtaIdx sec rec1 0 1, S.map $ const 0.9),
                       (FEtaIdx sec rec1 1 2, S.map $ const 0.9),
                       (FEtaIdx sec rec1 2 1, S.map $ const 0.9),
                       (FEtaIdx sec rec1 1 3, S.map $ const 0.9),
                       (FEtaIdx sec rec1 3 1, S.map $ const 0.9) ]
x1num :: XMap Sc
x1num = M.fromList []

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx sec rec1 1 0, S.map $ const 0.1),
                        (DEtaIdx sec rec1 0 1, S.map $ const 0.1),
                        (DEtaIdx sec rec1 1 2, S.map $ const 0.1),
                        (DEtaIdx sec rec1 2 1, S.map $ const 0.1),
                        (DEtaIdx sec rec1 1 3, S.map $ const 0.1),
                        (DEtaIdx sec rec1 3 1, S.map $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (DTimeIdx sec rec0, mkVar $ DTimeIdx sec rec0) ]

sigs0eq :: PowerMap EqTerm
sigs0eq =
   M.fromList [
      (PowerIdx sec rec0 0 1, mkVar $ PowerIdx sec rec0 0 1),
      (PowerIdx sec rec0 1 2, mkVar $ PowerIdx sec rec0 1 2)
      ]

eta0eq :: FEtaMap EqTerm
eta0eq =
   M.fromList [
      (FEtaIdx sec rec0 1 0, const $ mkVar $ FEtaIdx sec rec0 1 0),
      (FEtaIdx sec rec0 0 1, const $ mkVar $ FEtaIdx sec rec0 0 1),
      (FEtaIdx sec rec0 1 2, const $ mkVar $ FEtaIdx sec rec0 1 2),
      (FEtaIdx sec rec0 2 1, const $ mkVar $ FEtaIdx sec rec0 2 1),
      (FEtaIdx sec rec0 1 3, const $ mkVar $ FEtaIdx sec rec0 1 3),
      (FEtaIdx sec rec0 3 1, const $ mkVar $ FEtaIdx sec rec0 3 1)
      ]

x0eq :: XMap EqTerm
x0eq = M.fromList []

dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (DTimeIdx sec rec1, mkVar $ DTimeIdx sec rec1) ]

sigs1eq :: PowerMap EqTerm
sigs1eq =
   M.fromList [
      (PowerIdx sec rec1 0 1, mkVar $ PowerIdx sec rec1 0 1),
      (PowerIdx sec rec1 1 2, mkVar $ PowerIdx sec rec1 1 2)
      ]

dpower1eq :: DPowerMap EqTerm
dpower1eq =
   M.fromList [
      (DPowerIdx sec rec1 0 1, mkVar $ DPowerIdx sec rec1 0 1),
      (DPowerIdx sec rec1 1 2, mkVar $ DPowerIdx sec rec1 1 2)
      ]

eta1eq :: FEtaMap EqTerm
eta1eq =
   M.fromList [
      (FEtaIdx sec rec1 1 0, const $ mkVar $ FEtaIdx sec rec1 1 0),
      (FEtaIdx sec rec1 0 1, const $ mkVar $ FEtaIdx sec rec1 0 1),
      (FEtaIdx sec rec1 1 2, const $ mkVar $ FEtaIdx sec rec1 1 2),
      (FEtaIdx sec rec1 2 1, const $ mkVar $ FEtaIdx sec rec1 2 1),
      (FEtaIdx sec rec1 1 3, const $ mkVar $ FEtaIdx sec rec1 1 3),
      (FEtaIdx sec rec1 3 1, const $ mkVar $ FEtaIdx sec rec1 3 1)
      ]

x1eq :: XMap EqTerm
x1eq = M.fromList []

deta1eq :: DEtaMap EqTerm
deta1eq =
   M.fromList [
      (DEtaIdx sec rec1 1 0, const $ mkVar $ DEtaIdx sec rec1 1 0),
      (DEtaIdx sec rec1 0 1, const $ mkVar $ DEtaIdx sec rec1 0 1),
      (DEtaIdx sec rec1 1 2, const $ mkVar $ DEtaIdx sec rec1 1 2),
      (DEtaIdx sec rec1 2 1, const $ mkVar $ DEtaIdx sec rec1 2 1),
      (DEtaIdx sec rec1 1 3, const $ mkVar $ DEtaIdx sec rec1 1 3),
      (DEtaIdx sec rec1 3 1, const $ mkVar $ DEtaIdx sec rec1 3 1)
      ]


-- Energie teilt sich am Knoten 1
dreibein2 :: TheGraph EqTerm
dreibein2 = TheGraph g undefined
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]
