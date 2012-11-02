module EFA2.Example.LinearOne where

import EFA2.Topology.Topology
          (makeEdges, makeNodes)
import EFA2.Topology.TopologyData
          (NodeType(Source, Sink), defaultELabel)
import EFA2.Topology.EfaGraph (mkGraph)

import EFA2.Interpreter.Env
          (Index(Energy, DTime), DTimeMap, EnergyMap)
import EFA2.Solver.Equation (EqTerm, Term(Atom,Const))
import EFA2.Example.SymSig (TheGraph(TheGraph))

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (UTFSig, TC, Scalar, toScalar)
import EFA2.Signal.Typ (Typ, UT)
import EFA2.Signal.Data (Data, Nil)

import qualified Data.Map as M


sec :: Idx.Section
sec = Idx.Section 0

rec0, rec1 :: Idx.Record
rec0 = Idx.Record 0
rec1 = Idx.Record 1


dtimes0 :: DTimeMap UTFSig
dtimes0 = M.fromList [(Idx.DTime sec rec0, S.fromList [1.0])]

sigs0 :: EnergyMap UTFSig
sigs0 = M.fromList [ (Idx.Energy sec rec0 0 1, S.fromList [3]) ]

dtimes1 :: DTimeMap UTFSig
dtimes1 = M.fromList [(Idx.DTime sec rec1, S.fromList [1.0])]


sigs1 :: EnergyMap UTFSig
sigs1 = M.fromList [ (Idx.Energy sec rec1 0 1, S.fromList [3.3]) ]

-------------------------

type InTermScalar = TC Scalar (Typ UT UT UT) (Data Nil EqTerm)

dtimes0s :: DTimeMap InTermScalar
dtimes0s = M.fromList [ (Idx.DTime sec rec0, toScalar (Const 20.0)) ]

sigs0s :: EnergyMap InTermScalar
sigs0s = M.fromList [ (Idx.Energy sec rec0 0 1, toScalar (Const 3.0)) ]

dtimes1s :: DTimeMap InTermScalar
dtimes1s = M.fromList [ (Idx.DTime sec rec1, toScalar (Const 20.0)) ]

sigs1s :: EnergyMap InTermScalar
sigs1s = M.fromList [ (Idx.Energy sec rec1 0 1, toScalar (Const 3.3)) ]

-------------------------

dtimes0ss :: DTimeMap InTermScalar
dtimes0ss = M.fromList [ (Idx.DTime sec rec0, toScalar $ Atom $ DTime $ Idx.DTime sec rec0) ]

sigs0ss :: EnergyMap InTermScalar
sigs0ss = M.fromList [ (Idx.Energy sec rec0 1 0, toScalar $ Atom $ Energy $ Idx.Energy sec rec0 1 0) ]

dtimes1ss :: DTimeMap InTermScalar
dtimes1ss = M.fromList [ (Idx.DTime sec rec1, toScalar $ Atom $ DTime $ Idx.DTime sec rec1) ]

sigs1ss :: EnergyMap InTermScalar
sigs1ss = M.fromList [ (Idx.Energy sec rec1 0 1, toScalar $ Atom $ Energy $ Idx.Energy sec rec1 0 1) ]


linearOne :: TheGraph InTermScalar
linearOne = TheGraph g sigs0s
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Sink)]
        es = makeEdges [(0, 1, defaultELabel)]
