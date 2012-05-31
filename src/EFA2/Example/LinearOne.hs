{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module EFA2.Example.LinearOne where

import Data.Graph.Inductive
import qualified Data.Map as M

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils
import EFA2.Signal.Signal


dtimes0 :: DTimeMap UTFSig
dtimes0 = M.fromList [(DTimeIdx 0 0, sfromList [1.0])]

sigs0 :: EnergyMap UTFSig
sigs0 = M.fromList [ (EnergyIdx 0 0 0 1, sfromList [3]) ]

dtimes1 :: DTimeMap UTFSig
dtimes1 = M.fromList [(DTimeIdx 0 1, sfromList [1.2])]


sigs1 :: EnergyMap UTFSig
sigs1 = M.fromList [ (EnergyIdx 0 1 0 1, sfromList [3.3]) ]


linearOne :: TheGraph UTFSig
linearOne = TheGraph g sigs0
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Sink)]
        es = makeEdges [(0, 1, defaultELabel)]