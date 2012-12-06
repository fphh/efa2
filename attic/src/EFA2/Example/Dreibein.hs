
module EFA2.Example.Dreibein where

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import qualified EFA2.Topology.EfaGraph as Gr

import EFA2.Interpreter.Env
import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar)
import EFA2.Solver.Equation (Term(Atom, Const), EqTerm, mkVar)

import qualified Data.Map as M


sec :: Idx.Section
sec = Idx.Section 0

rec0, rec1 :: Idx.Record
rec0 = Idx.Record 0
rec1 = Idx.Record 1

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Record -> Idx.Section -> Int -> Int -> idx
edgeIdx mkIdx rec s x y =
   mkIdx rec (Idx.SecNode s (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))


dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (Idx.DTime rec0 sec, toScalar 1.0) ]

{-
-- forward
power0num :: PowerMap Sc
power0num = M.fromList [ (Idx.Power rec0 sec 0 1, toScalar 3.0) ]
-}

-- backward
power0num :: PowerMap Sc
power0num = M.fromList [ (edgeIdx Idx.Power rec0 sec 2 1, toScalar 3.0),
                         (edgeIdx Idx.Power rec0 sec 3 1, toScalar 2.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (edgeIdx Idx.FEta rec0 sec 1 0, S.map $ const 0.8),
                       (edgeIdx Idx.FEta rec0 sec 0 1, S.map $ const 0.8),
                       (edgeIdx Idx.FEta rec0 sec 1 2, S.map $ const 0.8),
                       (edgeIdx Idx.FEta rec0 sec 2 1, S.map $ const 0.8),
                       (edgeIdx Idx.FEta rec0 sec 1 3, S.map $ const 0.8),
                       (edgeIdx Idx.FEta rec0 sec 3 1, S.map $ const 0.8) ]

{-
-- forward
x0num :: XMap Sc
x0num = M.fromList [ (edgeIdx Idx.X rec0 sec 1 2, toScalar 0.4) ]
-}

-- backward
x0num :: XMap Sc
x0num = M.fromList []


dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (Idx.DTime rec1 sec, toScalar 1.0) ]

{-
-- forward
power1num :: PowerMap Sc
power1num = M.fromList [ (edgeIdx Idx.Power rec1 sec 0 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (edgeIdx Idx.DPower rec1 sec 0 1, toScalar 0.5) ]
-}

-- backward
power1num :: PowerMap Sc
power1num = M.fromList [ (edgeIdx Idx.Power rec1 sec 2 1, toScalar 3.5),
                         (edgeIdx Idx.Power rec1 sec 3 1, toScalar 3.0) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (edgeIdx Idx.DPower rec1 sec 2 1, toScalar 0.5),
                          (edgeIdx Idx.DPower rec1 sec 3 1, toScalar 1.0) ]



eta1num :: FEtaMap Sc
eta1num = M.fromList [ (edgeIdx Idx.FEta rec1 sec 1 0, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 0 1, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 1 2, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 2 1, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 1 3, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 3 1, S.map $ const 0.9) ]

{-
x1num :: XMap Sc
x1num = M.fromList [ (edgeIdx Idx.X rec1 sec 1 2, toScalar 0.3) ]
-}

-- backward
x1num :: XMap Sc
x1num = M.fromList []

dx1num :: DXMap Sc
dx1num = M.fromList [ (edgeIdx Idx.DX rec1 sec 1 0, toScalar 0.0),
                      (edgeIdx Idx.DX rec1 sec 0 1, toScalar 0.0),
                      (edgeIdx Idx.DX rec1 sec 1 2, toScalar (-0.1)),
                      (edgeIdx Idx.DX rec1 sec 2 1, toScalar 0.0),
                      (edgeIdx Idx.DX rec1 sec 1 3, toScalar 0.1),
                      (edgeIdx Idx.DX rec1 sec 3 1, toScalar 0.0) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (edgeIdx Idx.DEta rec1 sec 1 0, S.map $ const 0.1),
                       (edgeIdx Idx.DEta rec1 sec 0 1, S.map $ const 0.1),
                       (edgeIdx Idx.DEta rec1 sec 1 2, S.map $ const 0.1),
                       (edgeIdx Idx.DEta rec1 sec 2 1, S.map $ const 0.1),
                       (edgeIdx Idx.DEta rec1 sec 1 3, S.map $ const 0.1),
                       (edgeIdx Idx.DEta rec1 sec 3 1, S.map $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = fmap Atom $ M.fromList [ (Idx.DTime rec0 sec, mkVar $ Idx.DTime rec0 sec) ]

{-
-- forward
power0eq :: PowerMap EqTerm
power0eq = fmap Atom $ M.fromList [ (edgeIdx Idx.Power rec0 sec 0 1, mkVar $ edgeIdx Idx.Power rec0 sec 0 1) ]
-}

-- backward
power0eq :: PowerMap EqTerm
power0eq =
   M.fromList [
      (edgeIdx Idx.Power rec0 sec 2 1, mkVar $ edgeIdx Idx.Power rec0 sec 2 1),
      (edgeIdx Idx.Power rec0 sec 3 1, mkVar $ edgeIdx Idx.Power rec0 sec 3 1)
      ]

eta0eq :: FEtaMap EqTerm
eta0eq =
   M.fromList [
      (edgeIdx Idx.FEta rec0 sec 1 0, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 1 0),
      (edgeIdx Idx.FEta rec0 sec 0 1, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 0 1),
      (edgeIdx Idx.FEta rec0 sec 1 2, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 1 2),
      (edgeIdx Idx.FEta rec0 sec 2 1, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 2 1),
      (edgeIdx Idx.FEta rec0 sec 1 3, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 1 3),
      (edgeIdx Idx.FEta rec0 sec 3 1, const $ mkVar $ edgeIdx Idx.FEta rec0 sec 3 1)
      ]

{-
-- forward
x0eq :: XMap EqTerm
x0eq = fmap Atom $ M.fromList [ (edgeIdx Idx.X rec0 sec 1 2, X (edgeIdx Idx.X rec0 sec 1 2) ]
-}

-- backward
x0eq :: XMap EqTerm
x0eq = M.fromList []

dtimes1eq:: DTimeMap EqTerm
dtimes1eq = fmap Atom $ M.fromList [ (Idx.DTime rec1 sec, mkVar $ Idx.DTime rec1 sec) ]

{-
-- forward
power1eq :: PowerMap EqTerm
power1eq = fmap Atom $ M.fromList [ (edgeIdx Idx.Power rec1 sec 0 1, mkVar $ edgeIdx Idx.Power rec1 sec 0 1) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = fmap Atom $ M.fromList [ (edgeIdx Idx.DPower rec1 sec 0 1, mkVar $ edgeIdx Idx.DPower rec1 sec 0 1) ]
-}

-- backward

power1eq :: PowerMap EqTerm
power1eq =
   M.fromList [
      (edgeIdx Idx.Power rec1 sec 2 1, mkVar $ edgeIdx Idx.Power rec1 sec 2 1),
      (edgeIdx Idx.Power rec1 sec 3 1, mkVar $ edgeIdx Idx.Power rec1 sec 3 1)
      ]

dpower1eq :: DPowerMap EqTerm
dpower1eq =
   M.fromList [
      (edgeIdx Idx.DPower rec1 sec 2 1, mkVar $ edgeIdx Idx.DPower rec1 sec 2 1),
      (edgeIdx Idx.DPower rec1 sec 3 1, mkVar $ edgeIdx Idx.DPower rec1 sec 3 1)
      ]


eta1eq :: FEtaMap EqTerm
eta1eq =
   M.fromList [
      (edgeIdx Idx.FEta rec1 sec 1 0, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 1 0),
      (edgeIdx Idx.FEta rec1 sec 0 1, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 0 1),
      (edgeIdx Idx.FEta rec1 sec 1 2, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 1 2),
      (edgeIdx Idx.FEta rec1 sec 2 1, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 2 1),
      (edgeIdx Idx.FEta rec1 sec 1 3, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 1 3),
      (edgeIdx Idx.FEta rec1 sec 3 1, const $ mkVar $ edgeIdx Idx.FEta rec1 sec 3 1)
      ]
{-
-- forward
x1eq :: XMap EqTerm
x1eq = M.fromList [ (edgeIdx Idx.X rec1 sec 1 2, X (edgeIdx Idx.X rec1 sec 1 2) ]
-}

-- backward
x1eq :: XMap EqTerm
x1eq = M.fromList []


dx1eq :: DXMap EqTerm
dx1eq =
   M.fromList [
      (edgeIdx Idx.DX rec1 sec 1 0, Const 0.0),
      (edgeIdx Idx.DX rec1 sec 0 1, Const 0.0),
      (edgeIdx Idx.DX rec1 sec 1 2, mkVar $ edgeIdx Idx.DX rec1 sec 1 2),
      (edgeIdx Idx.DX rec1 sec 2 1, Const 0.0),
      (edgeIdx Idx.DX rec1 sec 1 3, mkVar $ edgeIdx Idx.DX rec1 sec 1 3),
      (edgeIdx Idx.DX rec1 sec 3 1, Const 0.0)
      ]


deta1eq :: DEtaMap EqTerm
deta1eq =
   M.fromList [
      (edgeIdx Idx.DEta rec1 sec 1 0, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 1 0),
      (edgeIdx Idx.DEta rec1 sec 0 1, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 0 1),
      (edgeIdx Idx.DEta rec1 sec 1 2, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 1 2),
      (edgeIdx Idx.DEta rec1 sec 2 1, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 2 1),
      (edgeIdx Idx.DEta rec1 sec 1 3, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 1 3),
      (edgeIdx Idx.DEta rec1 sec 3 1, const $ mkVar $ edgeIdx Idx.DEta rec1 sec 3 1)
      ]


-- Energie wird am Knoten 1 geteilt.
graph :: SequFlowGraph
graph =
   Gr.emap (\() -> defaultELabel) $
   Gr.ixmap (Idx.SecNode sec) topo

topo :: Topology
topo = Gr.mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeSimpleEdges [(0, 1), (1, 2), (1, 3)]
