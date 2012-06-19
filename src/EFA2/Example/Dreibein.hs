

module EFA2.Example.Dreibein where

import Data.Graph.Inductive
import qualified Data.Map as M


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils
import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Solver.Equation

type Sc = Scal (Typ UT UT UT) Val


dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx 0 0, toScalar 1.0) ]

power0num :: PowerMap Sc
power0num = M.fromList [ (PowerIdx 0 0 0 1, toScalar 3.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx 0 0 1 0, smap $ const 0.8), 
                      (FEtaIdx 0 0 0 1, smap $ const 0.8),
                      (FEtaIdx 0 0 1 2, smap $ const 0.8), 
                      (FEtaIdx 0 0 2 1, smap $ const 0.8),
                      (FEtaIdx 0 0 1 3, smap $ const 0.8), 
                      (FEtaIdx 0 0 3 1, smap $ const 0.8) ]

x0num :: XMap Sc
x0num = M.fromList [ (XIdx 0 0 1 2, toScalar 0.4) ]

dtimes1num:: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx 0 1, toScalar 1.0) ]

power1num :: PowerMap Sc
power1num = M.fromList [ (PowerIdx 0 1 0 1, toScalar 3.5) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx 0 1 0 1, toScalar 0.5) ]

eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx 0 1 1 0, smap $ const 0.9), 
                       (FEtaIdx 0 1 0 1, smap $ const 0.9),
                       (FEtaIdx 0 1 1 2, smap $ const 0.9), 
                       (FEtaIdx 0 1 2 1, smap $ const 0.9),
                       (FEtaIdx 0 1 1 3, smap $ const 0.9), 
                       (FEtaIdx 0 1 3 1, smap $ const 0.9) ]

x1num :: XMap Sc
x1num = M.fromList [ (XIdx 0 1 1 2, toScalar 0.3) ]

dx1num :: DXMap Sc
dx1num = M.fromList [ (DXIdx 0 1 1 0, toScalar 0.0), 
                      (DXIdx 0 1 0 1, toScalar 0.0),
                      (DXIdx 0 1 1 2, toScalar (-0.1)), 
                      (DXIdx 0 1 2 1, toScalar 0.0),
                      (DXIdx 0 1 1 3, toScalar 0.1), 
                      (DXIdx 0 1 3 1, toScalar 0.0) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx 0 1 1 0, smap $ const 0.1), 
                       (DEtaIdx 0 1 0 1, smap $ const 0.1),
                       (DEtaIdx 0 1 1 2, smap $ const 0.1), 
                       (DEtaIdx 0 1 2 1, smap $ const 0.1),
                       (DEtaIdx 0 1 1 3, smap $ const 0.1), 
                       (DEtaIdx 0 1 3 1, smap $ const 0.1) ]


------------------------------------------------------------------------------

dtimes0eq :: DTimeMap EqTerm
dtimes0eq = M.fromList [ (DTimeIdx 0 0, DTime (DTimeIdx 0 0)) ]

power0eq :: PowerMap EqTerm
power0eq = M.fromList [ (PowerIdx 0 0 0 1, Power (PowerIdx 0 0 0 1)) ]

eta0eq :: FEtaMap EqTerm
eta0eq = M.fromList [ (FEtaIdx 0 0 1 0, const $ FEta (FEtaIdx 0 0 1 0)), 
                      (FEtaIdx 0 0 0 1, const $ FEta (FEtaIdx 0 0 0 1)),
                      (FEtaIdx 0 0 1 2, const $ FEta (FEtaIdx 0 0 1 2)), 
                      (FEtaIdx 0 0 2 1, const $ FEta (FEtaIdx 0 0 2 1)),
                      (FEtaIdx 0 0 1 3, const $ FEta (FEtaIdx 0 0 1 3)), 
                      (FEtaIdx 0 0 3 1, const $ FEta (FEtaIdx 0 0 3 1)) ]
x0eq :: XMap EqTerm
x0eq = M.fromList [ (XIdx 0 0 1 2, X (XIdx 0 0 1 2)) ]

dtimes1eq:: DTimeMap EqTerm
dtimes1eq = M.fromList [ (DTimeIdx 0 1, DTime (DTimeIdx 0 1)) ]

power1eq :: PowerMap EqTerm
power1eq = M.fromList [ (PowerIdx 0 1 0 1, Power (PowerIdx 0 1 0 1)) ]

dpower1eq :: DPowerMap EqTerm
dpower1eq = M.fromList [ (DPowerIdx 0 1 0 1, DPower (DPowerIdx 0 1 0 1)) ]

eta1eq :: FEtaMap EqTerm
eta1eq = M.fromList [ (FEtaIdx 0 1 1 0, const $ FEta (FEtaIdx 0 1 1 0)), 
                      (FEtaIdx 0 1 0 1, const $ FEta (FEtaIdx 0 1 0 1)),
                      (FEtaIdx 0 1 1 2, const $ FEta (FEtaIdx 0 1 1 2)), 
                      (FEtaIdx 0 1 2 1, const $ FEta (FEtaIdx 0 1 2 1)),
                      (FEtaIdx 0 1 1 3, const $ FEta (FEtaIdx 0 1 1 3)), 
                      (FEtaIdx 0 1 3 1, const $ FEta (FEtaIdx 0 1 3 1)) ]
x1eq :: XMap EqTerm
x1eq = M.fromList [ (XIdx 0 1 1 2, X (XIdx 0 1 1 2)) ]

dx1eq :: DXMap EqTerm
dx1eq = M.fromList [ (DXIdx 0 1 1 0, DX (DXIdx 0 1 1 0)), 
                     (DXIdx 0 1 0 1, DX (DXIdx 0 1 0 1)),
                     (DXIdx 0 1 1 2, DX (DXIdx 0 1 1 2)), 
                     (DXIdx 0 1 2 1, DX (DXIdx 0 1 2 1)),
                     (DXIdx 0 1 1 3, DX (DXIdx 0 1 1 3)), 
                     (DXIdx 0 1 3 1, DX (DXIdx 0 1 3 1)) ]


deta1eq :: DEtaMap EqTerm
deta1eq = M.fromList [ (DEtaIdx 0 1 1 0, const $ DEta (DEtaIdx 0 1 1 0)), 
                       (DEtaIdx 0 1 0 1, const $ DEta (DEtaIdx 0 1 0 1)),
                       (DEtaIdx 0 1 1 2, const $ DEta (DEtaIdx 0 1 1 2)), 
                       (DEtaIdx 0 1 2 1, const $ DEta (DEtaIdx 0 1 2 1)),
                       (DEtaIdx 0 1 1 3, const $ DEta (DEtaIdx 0 1 1 3)), 
                       (DEtaIdx 0 1 3 1, const $ DEta (DEtaIdx 0 1 3 1)) ]


-- Energie wird am Knoten 1 geteilt.
graph :: Topology
graph = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Crossing), (2, Sink), (3, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

