module EFA2.Example.LinearOne where

import Data.Graph.Inductive (mkGraph)
import qualified Data.Map as M

import EFA2.Topology.Topology
          (makeEdges, makeNodes)
import EFA2.Topology.TopologyData
          (NodeType(Source, Sink), defaultELabel)

import EFA2.Interpreter.Env
          (Index(Energy, DTime),
           DTimeIdx(DTimeIdx), DTimeMap,
           EnergyIdx(EnergyIdx), EnergyMap)
import EFA2.Solver.Equation (EqTerm, Term(Atom,Const))
import EFA2.Example.SymSig (TheGraph(TheGraph))
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (UTFSig, TC, Scalar, toScalar)
import EFA2.Signal.Typ (Typ, UT)
import EFA2.Signal.Data (Data, Nil)


dtimes0 :: DTimeMap UTFSig
dtimes0 = M.fromList [(DTimeIdx 0 0, S.fromList [1.0])]

sigs0 :: EnergyMap UTFSig
sigs0 = M.fromList [ (EnergyIdx 0 0 0 1, S.fromList [3]) ]

dtimes1 :: DTimeMap UTFSig
dtimes1 = M.fromList [(DTimeIdx 0 1, S.fromList [1.0])]


sigs1 :: EnergyMap UTFSig
sigs1 = M.fromList [ (EnergyIdx 0 1 0 1, S.fromList [3.3]) ]

-------------------------

type InTermScalar = TC Scalar (Typ UT UT UT) (Data Nil EqTerm)

dtimes0s :: DTimeMap InTermScalar
dtimes0s = M.fromList [ (DTimeIdx 0 0, toScalar (Const 20.0)) ]

sigs0s :: EnergyMap InTermScalar
sigs0s = M.fromList [ (EnergyIdx 0 0 0 1, toScalar (Const 3.0)) ]

dtimes1s :: DTimeMap InTermScalar
dtimes1s = M.fromList [ (DTimeIdx 0 1, toScalar (Const 20.0)) ]

sigs1s :: EnergyMap InTermScalar
sigs1s = M.fromList [ (EnergyIdx 0 1 0 1, toScalar (Const 3.3)) ]

-------------------------

dtimes0ss :: DTimeMap InTermScalar
dtimes0ss = M.fromList [ (DTimeIdx 0 0, toScalar $ Atom $ DTime $ DTimeIdx 0 0) ]

sigs0ss :: EnergyMap InTermScalar
sigs0ss = M.fromList [ (EnergyIdx 0 0 1 0, toScalar $ Atom $ Energy $ EnergyIdx 0 0 1 0) ]

dtimes1ss :: DTimeMap InTermScalar
dtimes1ss = M.fromList [ (DTimeIdx 0 1, toScalar $ Atom $ DTime $ DTimeIdx 0 1) ]

sigs1ss :: EnergyMap InTermScalar
sigs1ss = M.fromList [ (EnergyIdx 0 1 0 1, toScalar $ Atom $ Energy $ EnergyIdx 0 1 0 1) ]


linearOne :: TheGraph InTermScalar
linearOne = TheGraph g sigs0s
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Sink)]
        es = makeEdges [(0, 1, defaultELabel)]
