
module EFA2.Example.Dreibein2 where

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph (mkGraph)

import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar)
import EFA2.Solver.Equation (EqTerm, mkVar)

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

sigs0num :: PowerMap Sc
sigs0num = M.fromList [ (edgeIdx Idx.Power rec0 sec 0 1, toScalar 3.0),
                        (edgeIdx Idx.Power rec0 sec 1 2, toScalar 1.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (edgeIdx Idx.FEta rec0 sec 1 0, S.map $ const 0.8),
                      (edgeIdx Idx.FEta rec0 sec 0 1, S.map $ const 0.8),
                      (edgeIdx Idx.FEta rec0 sec 1 2, S.map $ const 0.8),
                      (edgeIdx Idx.FEta rec0 sec 2 1, S.map $ const 0.8),
                      (edgeIdx Idx.FEta rec0 sec 1 3, S.map $ const 0.8),
                      (edgeIdx Idx.FEta rec0 sec 3 1, S.map $ const 0.8) ]
x0num :: XMap Sc
x0num = M.fromList []

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (Idx.DTime rec1 sec, toScalar 1.0) ]


sigs1num :: PowerMap Sc
sigs1num = M.fromList [ (edgeIdx Idx.Power rec1 sec 0 1, toScalar 3.5),
                        (edgeIdx Idx.Power rec1 sec 1 2, toScalar 2.0) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (edgeIdx Idx.DPower rec1 sec 0 1, toScalar 0.5),
                          (edgeIdx Idx.DPower rec1 sec 1 2, toScalar 1.0) ]

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (edgeIdx Idx.FEta rec1 sec 1 0, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 0 1, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 1 2, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 2 1, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 1 3, S.map $ const 0.9),
                       (edgeIdx Idx.FEta rec1 sec 3 1, S.map $ const 0.9) ]
x1num :: XMap Sc
x1num = M.fromList []

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (edgeIdx Idx.DEta rec1 sec 1 0, S.map $ const 0.1),
                        (edgeIdx Idx.DEta rec1 sec 0 1, S.map $ const 0.1),
                        (edgeIdx Idx.DEta rec1 sec 1 2, S.map $ const 0.1),
                        (edgeIdx Idx.DEta rec1 sec 2 1, S.map $ const 0.1),
                        (edgeIdx Idx.DEta rec1 sec 1 3, S.map $ const 0.1),
                        (edgeIdx Idx.DEta rec1 sec 3 1, S.map $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (Idx.DTime rec0 sec, mkVar $ Idx.DTime rec0 sec) ]

sigs0eq :: PowerMap EqTerm
sigs0eq =
   M.fromList [
      (edgeIdx Idx.Power rec0 sec 0 1, mkVar $ edgeIdx Idx.Power rec0 sec 0 1),
      (edgeIdx Idx.Power rec0 sec 1 2, mkVar $ edgeIdx Idx.Power rec0 sec 1 2)
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

x0eq :: XMap EqTerm
x0eq = M.fromList []

dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (Idx.DTime rec1 sec, mkVar $ Idx.DTime rec1 sec) ]

sigs1eq :: PowerMap EqTerm
sigs1eq =
   M.fromList [
      (edgeIdx Idx.Power rec1 sec 0 1, mkVar $ edgeIdx Idx.Power rec1 sec 0 1),
      (edgeIdx Idx.Power rec1 sec 1 2, mkVar $ edgeIdx Idx.Power rec1 sec 1 2)
      ]

dpower1eq :: DPowerMap EqTerm
dpower1eq =
   M.fromList [
      (edgeIdx Idx.DPower rec1 sec 0 1, mkVar $ edgeIdx Idx.DPower rec1 sec 0 1),
      (edgeIdx Idx.DPower rec1 sec 1 2, mkVar $ edgeIdx Idx.DPower rec1 sec 1 2)
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

x1eq :: XMap EqTerm
x1eq = M.fromList []

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


-- Energie teilt sich am Knoten 1
dreibein2 :: TheGraph EqTerm
dreibein2 = TheGraph g undefined
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeWithDirEdges [(0, 1), (1, 2), (1, 3)]
