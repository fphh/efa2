

module EFA2.Example.Dreibein3 where

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

dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx sec 0, toScalar 1.0) ]

sigs0num :: PowerMap Sc
sigs0num = M.fromList [ (PowerIdx sec 0 0 1, toScalar 3.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx sec 0 1 0, S.map $ const 0.8),
                       (FEtaIdx sec 0 0 1, S.map $ const 0.8),
                       (FEtaIdx sec 0 1 2, S.map $ const 0.8),
                       (FEtaIdx sec 0 2 1, S.map $ const 0.8),
                       (FEtaIdx sec 0 1 3, S.map $ const 0.8),
                       (FEtaIdx sec 0 3 1, S.map $ const 0.8) ]

x0num :: XMap Sc
x0num = M.fromList [ (XIdx sec 0 1 2, toScalar 0.7),
                     (XIdx sec 0 1 3, toScalar 0.3) ]

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx sec 1, toScalar 1.0) ]


sigs1num :: PowerMap Sc
sigs1num = M.fromList [ (PowerIdx sec 1 0 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx sec 1 0 1, toScalar 0.5) ]

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx sec 1 1 0, S.map $ const 0.9),
                       (FEtaIdx sec 1 0 1, S.map $ const 0.9),
                       (FEtaIdx sec 1 1 2, S.map $ const 0.9),
                       (FEtaIdx sec 1 2 1, S.map $ const 0.9),
                       (FEtaIdx sec 1 1 3, S.map $ const 0.9),
                       (FEtaIdx sec 1 3 1, S.map $ const 0.9) ]

x1num :: XMap Sc
x1num = M.fromList [ (XIdx sec 1 1 2, toScalar 0.7),
                     (XIdx sec 1 1 3, toScalar 0.3) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx sec 1 1 0, S.map $ const 0.1),
                        (DEtaIdx sec 1 0 1, S.map $ const 0.1),
                        (DEtaIdx sec 1 1 2, S.map $ const 0.1),
                        (DEtaIdx sec 1 2 1, S.map $ const 0.1),
                        (DEtaIdx sec 1 1 3, S.map $ const 0.1),
                        (DEtaIdx sec 1 3 1, S.map $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (DTimeIdx sec 0, mkVar $ DTimeIdx sec 0) ]

sigs0eq :: PowerMap EqTerm
sigs0eq = M.fromList [ (PowerIdx sec 0 0 1, mkVar $ PowerIdx sec 0 0 1) ]

eta0eq :: FEtaMap EqTerm
eta0eq =
   M.fromList [
      (FEtaIdx sec 0 1 0, const $ mkVar $ FEtaIdx sec 0 1 0),
      (FEtaIdx sec 0 0 1, const $ mkVar $ FEtaIdx sec 0 0 1),
      (FEtaIdx sec 0 1 2, const $ mkVar $ FEtaIdx sec 0 1 2),
      (FEtaIdx sec 0 2 1, const $ mkVar $ FEtaIdx sec 0 2 1),
      (FEtaIdx sec 0 1 3, const $ mkVar $ FEtaIdx sec 0 1 3),
      (FEtaIdx sec 0 3 1, const $ mkVar $ FEtaIdx sec 0 3 1)
      ]


x0eq :: XMap EqTerm
x0eq =
   M.fromList [
      (XIdx sec 0 1 2, mkVar $ XIdx sec 0 1 2),
      (XIdx sec 0 1 3, mkVar $ XIdx sec 0 1 3)
      ]


dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (DTimeIdx sec 1, mkVar $ DTimeIdx sec 1) ]

sigs1eq :: PowerMap EqTerm
sigs1eq = M.fromList [ (PowerIdx sec 1 0 1, mkVar $ PowerIdx sec 1 0 1) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = M.fromList [ (DPowerIdx sec 1 0 1, mkVar $ DPowerIdx sec 1 0 1) ]

eta1eq :: FEtaMap EqTerm
eta1eq =
   M.fromList [
      (FEtaIdx sec 1 1 0, const $ mkVar $ FEtaIdx sec 1 1 0),
      (FEtaIdx sec 1 0 1, const $ mkVar $ FEtaIdx sec 1 0 1),
      (FEtaIdx sec 1 1 2, const $ mkVar $ FEtaIdx sec 1 1 2),
      (FEtaIdx sec 1 2 1, const $ mkVar $ FEtaIdx sec 1 2 1),
      (FEtaIdx sec 1 1 3, const $ mkVar $ FEtaIdx sec 1 1 3),
      (FEtaIdx sec 1 3 1, const $ mkVar $ FEtaIdx sec 1 3 1)
      ]

x1eq :: XMap EqTerm
x1eq =
   M.fromList [
      (XIdx sec 1 1 2, mkVar $ XIdx sec 1 1 2),
      (XIdx sec 1 1 3, mkVar $ XIdx sec 1 1 3)
      ]


deta1eq :: DEtaMap EqTerm
deta1eq =
   M.fromList [
      (DEtaIdx sec 1 1 0, const $ mkVar $ DEtaIdx sec 1 1 0),
      (DEtaIdx sec 1 0 1, const $ mkVar $ DEtaIdx sec 1 0 1),
      (DEtaIdx sec 1 1 2, const $ mkVar $ DEtaIdx sec 1 1 2),
      (DEtaIdx sec 1 2 1, const $ mkVar $ DEtaIdx sec 1 2 1),
      (DEtaIdx sec 1 1 3, const $ mkVar $ DEtaIdx sec 1 1 3),
      (DEtaIdx sec 1 3 1, const $ mkVar $ DEtaIdx sec 1 3 1)
      ]


-- Energie teilt sich am Knoten 1
dreibein3 :: TheGraph EqTerm
dreibein3 = TheGraph g undefined
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]
