

module EFA2.Example.Dreibein3 where

import Data.Graph.Inductive
import qualified Data.Map as M


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Scal, toScalar)
import EFA2.Signal.Typ
import EFA2.Signal.Base
import EFA2.Solver.Equation

type Sc = Scal (Typ UT UT UT) Val


dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx 0 0, toScalar 1.0) ]

sigs0num :: PowerMap Sc
sigs0num = M.fromList [ (PowerIdx 0 0 0 1, toScalar 3.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx 0 0 1 0, S.map $ const 0.8),
                       (FEtaIdx 0 0 0 1, S.map $ const 0.8),
                       (FEtaIdx 0 0 1 2, S.map $ const 0.8),
                       (FEtaIdx 0 0 2 1, S.map $ const 0.8),
                       (FEtaIdx 0 0 1 3, S.map $ const 0.8),
                       (FEtaIdx 0 0 3 1, S.map $ const 0.8) ]

x0num :: XMap Sc
x0num = M.fromList [ (XIdx 0 0 1 2, toScalar 0.7),
                     (XIdx 0 0 1 3, toScalar 0.3) ]

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx 0 1, toScalar 1.0) ]


sigs1num :: PowerMap Sc
sigs1num = M.fromList [ (PowerIdx 0 1 0 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx 0 1 0 1, toScalar 0.5) ]

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx 0 1 1 0, S.map $ const 0.9),
                       (FEtaIdx 0 1 0 1, S.map $ const 0.9),
                       (FEtaIdx 0 1 1 2, S.map $ const 0.9),
                       (FEtaIdx 0 1 2 1, S.map $ const 0.9),
                       (FEtaIdx 0 1 1 3, S.map $ const 0.9),
                       (FEtaIdx 0 1 3 1, S.map $ const 0.9) ]

x1num :: XMap Sc
x1num = M.fromList [ (XIdx 0 1 1 2, toScalar 0.7),
                     (XIdx 0 1 1 3, toScalar 0.3) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx 0 1 1 0, S.map $ const 0.1),
                        (DEtaIdx 0 1 0 1, S.map $ const 0.1),
                        (DEtaIdx 0 1 1 2, S.map $ const 0.1),
                        (DEtaIdx 0 1 2 1, S.map $ const 0.1),
                        (DEtaIdx 0 1 1 3, S.map $ const 0.1),
                        (DEtaIdx 0 1 3 1, S.map $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (DTimeIdx 0 0, mkVar $ DTimeIdx 0 0) ]

sigs0eq :: PowerMap EqTerm
sigs0eq = M.fromList [ (PowerIdx 0 0 0 1, mkVar $ PowerIdx 0 0 0 1) ]

eta0eq :: FEtaMap EqTerm
eta0eq =
   M.fromList [
      (FEtaIdx 0 0 1 0, const $ mkVar $ FEtaIdx 0 0 1 0),
      (FEtaIdx 0 0 0 1, const $ mkVar $ FEtaIdx 0 0 0 1),
      (FEtaIdx 0 0 1 2, const $ mkVar $ FEtaIdx 0 0 1 2),
      (FEtaIdx 0 0 2 1, const $ mkVar $ FEtaIdx 0 0 2 1),
      (FEtaIdx 0 0 1 3, const $ mkVar $ FEtaIdx 0 0 1 3),
      (FEtaIdx 0 0 3 1, const $ mkVar $ FEtaIdx 0 0 3 1)
      ]


x0eq :: XMap EqTerm
x0eq =
   M.fromList [
      (XIdx 0 0 1 2, mkVar $ XIdx 0 0 1 2),
      (XIdx 0 0 1 3, mkVar $ XIdx 0 0 1 3)
      ]


dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (DTimeIdx 0 1, mkVar $ DTimeIdx 0 1) ]

sigs1eq :: PowerMap EqTerm
sigs1eq = M.fromList [ (PowerIdx 0 1 0 1, mkVar $ PowerIdx 0 1 0 1) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = M.fromList [ (DPowerIdx 0 1 0 1, mkVar $ DPowerIdx 0 1 0 1) ]

eta1eq :: FEtaMap EqTerm
eta1eq =
   M.fromList [
      (FEtaIdx 0 1 1 0, const $ mkVar $ FEtaIdx 0 1 1 0),
      (FEtaIdx 0 1 0 1, const $ mkVar $ FEtaIdx 0 1 0 1),
      (FEtaIdx 0 1 1 2, const $ mkVar $ FEtaIdx 0 1 1 2),
      (FEtaIdx 0 1 2 1, const $ mkVar $ FEtaIdx 0 1 2 1),
      (FEtaIdx 0 1 1 3, const $ mkVar $ FEtaIdx 0 1 1 3),
      (FEtaIdx 0 1 3 1, const $ mkVar $ FEtaIdx 0 1 3 1)
      ]

x1eq :: XMap EqTerm
x1eq =
   M.fromList [
      (XIdx 0 1 1 2, mkVar $ XIdx 0 1 1 2),
      (XIdx 0 1 1 3, mkVar $ XIdx 0 1 1 3)
      ]


deta1eq :: DEtaMap EqTerm
deta1eq =
   M.fromList [
      (DEtaIdx 0 1 1 0, const $ mkVar $ DEtaIdx 0 1 1 0),
      (DEtaIdx 0 1 0 1, const $ mkVar $ DEtaIdx 0 1 0 1),
      (DEtaIdx 0 1 1 2, const $ mkVar $ DEtaIdx 0 1 1 2),
      (DEtaIdx 0 1 2 1, const $ mkVar $ DEtaIdx 0 1 2 1),
      (DEtaIdx 0 1 1 3, const $ mkVar $ DEtaIdx 0 1 1 3),
      (DEtaIdx 0 1 3 1, const $ mkVar $ DEtaIdx 0 1 3 1)
      ]


-- Energie teilt sich am Knoten 1
dreibein3 :: TheGraph EqTerm
dreibein3 = TheGraph g undefined
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]
