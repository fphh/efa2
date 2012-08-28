module EFA2.Example.LinearOne where

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


dtimes0 :: DTimeMap UTFSig
dtimes0 = M.fromList [(DTimeIdx 0 0, sfromList [1.0])]

sigs0 :: EnergyMap UTFSig
sigs0 = M.fromList [ (EnergyIdx 0 0 0 1, sfromList [3]) ]

dtimes1 :: DTimeMap UTFSig
dtimes1 = M.fromList [(DTimeIdx 0 1, sfromList [1.0])]


sigs1 :: EnergyMap UTFSig
sigs1 = M.fromList [ (EnergyIdx 0 1 0 1, sfromList [3.3]) ]

-------------------------

type InTermScalar = TC Scalar (Typ UT UT UT) (Data Nil (InTerm Val))

dtimes0s :: DTimeMap InTermScalar
dtimes0s = M.fromList [ (DTimeIdx 0 0, toScalar (InConst 20.0)) ]

sigs0s :: EnergyMap InTermScalar
sigs0s = M.fromList [ (EnergyIdx 0 0 0 1, toScalar (InConst 3.0)) ]

dtimes1s :: DTimeMap InTermScalar
dtimes1s = M.fromList [ (DTimeIdx 0 1, toScalar (InConst 20.0)) ]

sigs1s :: EnergyMap InTermScalar
sigs1s = M.fromList [ (EnergyIdx 0 1 0 1, toScalar (InConst 3.3)) ]

-------------------------

dtimes0ss :: DTimeMap InTermScalar
dtimes0ss = M.fromList [ (DTimeIdx 0 0, toScalar (DTIdx (DTimeIdx 0 0))) ]

sigs0ss :: EnergyMap InTermScalar
sigs0ss = M.fromList [ (EnergyIdx 0 0 1 0, toScalar (EIdx (EnergyIdx 0 0 1 0))) ]

dtimes1ss :: DTimeMap InTermScalar
dtimes1ss = M.fromList [ (DTimeIdx 0 1, toScalar (DTIdx (DTimeIdx 0 1))) ]

sigs1ss :: EnergyMap InTermScalar
sigs1ss = M.fromList [ (EnergyIdx 0 1 0 1, toScalar (EIdx (EnergyIdx 0 1 0 1))) ]


linearOne :: TheGraph InTermScalar
linearOne = TheGraph g sigs0s
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Sink)]
        es = makeEdges [(0, 1, defaultELabel)]
