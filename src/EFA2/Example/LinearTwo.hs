module EFA2.Example.LinearTwo where

import Data.Graph.Inductive (mkGraph)
import qualified Data.Map as M

import EFA2.Topology.Topology
          (makeEdges, makeNodes)
import EFA2.Topology.TopologyData
          (Topology, NodeType(Source, Sink, Crossing), defaultELabel)

import EFA2.Solver.Equation (EqTerm, Term(Const), mkVar)
import EFA2.Interpreter.Env
          (DTimeIdx(DTimeIdx), DTimeMap,
           FEtaIdx(FEtaIdx), FEtaMap,
           DEtaIdx(DEtaIdx), DEtaMap,
           PowerIdx(PowerIdx), PowerMap,
           DPowerIdx(DPowerIdx), DPowerMap,
           DXIdx(DXIdx), XMap, DXMap)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar)



sec :: Idx.Section
sec = Idx.Section 0


dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx sec 0, toScalar 1.0) ]


-- forward
power0num :: PowerMap Sc
power0num = M.fromList [ (PowerIdx sec 0 0 1, toScalar 3.0) ]

{-
-- backward
power0num :: PowerMap Sc
power0num = M.fromList [ (PowerIdx sec 0 2 1, toScalar 3.0) ]
-}

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx sec 0 1 0, S.map $ const 0.8),
                       (FEtaIdx sec 0 0 1, S.map $ const 0.8),
                       (FEtaIdx sec 0 1 2, S.map $ const 0.8),
                       (FEtaIdx sec 0 2 1, S.map $ const 0.8) ]

x0num :: XMap Sc
x0num = M.fromList []

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx sec 1, toScalar 1.0) ]


-- forward
power1num :: PowerMap Sc
power1num = M.fromList [ (PowerIdx sec 1 0 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx sec 1 0 1, toScalar 0.5) ]

{-
-- backward
power1num :: PowerMap Sc
power1num = M.fromList [ (PowerIdx sec 1 2 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx sec 1 2 1, toScalar 0.5) ]
-}

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx sec 1 1 0, S.map $ const 0.9),
                       (FEtaIdx sec 1 0 1, S.map $ const 0.9),
                       (FEtaIdx sec 1 1 2, S.map $ const 0.9),
                       (FEtaIdx sec 1 2 1, S.map $ const 0.9) ]

x1num :: XMap Sc
x1num = M.fromList []

dx1num :: DXMap Sc
dx1num = M.fromList [ (DXIdx sec 1 0 1, toScalar 0.0),
                      (DXIdx sec 1 1 0, toScalar 0.0),
                      (DXIdx sec 1 1 2, toScalar 0.0),
                      (DXIdx sec 1 2 1, toScalar 0.0) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx sec 1 1 0, S.map $ const 0.1),
                        (DEtaIdx sec 1 0 1, S.map $ const 0.1),
                        (DEtaIdx sec 1 1 2, S.map $ const 0.1),
                        (DEtaIdx sec 1 2 1, S.map $ const 0.1) ]

---------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (DTimeIdx sec 0, mkVar $ DTimeIdx sec 0) ]


-- forward
power0eq :: PowerMap EqTerm
power0eq = M.fromList [ (PowerIdx sec 0 0 1, mkVar $ PowerIdx sec 0 0 1) ]


{-
-- backwards
power0eq :: PowerMap EqTerm
power0eq = M.fromList [ (PowerIdx sec 0 2 1, mkVar $ PowerIdx sec 0 2 1) ]
-}


eta0eq :: FEtaMap EqTerm
eta0eq = M.fromList [ (FEtaIdx sec 0 1 0, const $ mkVar $ FEtaIdx sec 0 1 0),
                      (FEtaIdx sec 0 0 1, const $ mkVar $ FEtaIdx sec 0 0 1),
                      (FEtaIdx sec 0 1 2, const $ mkVar $ FEtaIdx sec 0 1 2),
                      (FEtaIdx sec 0 2 1, const $ mkVar $ FEtaIdx sec 0 2 1) ]

x0eq :: XMap EqTerm
x0eq = M.fromList []

dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (DTimeIdx sec 1, mkVar $ DTimeIdx sec 1) ]


-- forward
power1eq :: PowerMap EqTerm
power1eq = M.fromList [ (PowerIdx sec 1 0 1, mkVar $ PowerIdx sec 1 0 1) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = M.fromList [ (DPowerIdx sec 1 0 1, mkVar $ DPowerIdx sec 1 0 1) ]

{-
-- backward
power1eq :: PowerMap EqTerm
power1eq = M.fromList [ (PowerIdx sec 1 2 1, Power (PowerIdx sec 1 2 1)) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = M.fromList [ (DPowerIdx sec 1 2 1, DPower (DPowerIdx sec 1 2 1)) ]
-}

eta1eq :: FEtaMap EqTerm
eta1eq = M.fromList [ (FEtaIdx sec 1 1 0, const $ mkVar $ FEtaIdx sec 1 1 0),
                      (FEtaIdx sec 1 0 1, const $ mkVar $ FEtaIdx sec 1 0 1),
                      (FEtaIdx sec 1 1 2, const $ mkVar $ FEtaIdx sec 1 1 2),
                      (FEtaIdx sec 1 2 1, const $ mkVar $ FEtaIdx sec 1 2 1) ]

x1eq :: XMap EqTerm
x1eq = M.fromList []

dx1eq :: DXMap EqTerm
dx1eq = M.fromList [ (DXIdx sec 1 0 1, Const 0.0),
                     (DXIdx sec 1 1 0, Const 0.0),
                     (DXIdx sec 1 1 2, Const 0.0),
                     (DXIdx sec 1 2 1, Const 0.0) ]

deta1eq :: DEtaMap EqTerm
deta1eq = M.fromList [ (DEtaIdx sec 1 1 0, const $ mkVar $ DEtaIdx sec 1 1 0),
                       (DEtaIdx sec 1 0 1, const $ mkVar $ DEtaIdx sec 1 0 1),
                       (DEtaIdx sec 1 1 2, const $ mkVar $ DEtaIdx sec 1 1 2),
                       (DEtaIdx sec 1 2 1, const $ mkVar $ DEtaIdx sec 1 2 1) ]


graph :: Topology
graph = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Crossing), (2, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel)]

