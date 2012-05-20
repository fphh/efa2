{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module EFA2.Example.LinearOne (linearOne, dtimes) where

import Data.Graph.Inductive
import qualified Data.Map as M

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils

dtimes = M.fromList [(DTimeIdx 0 0, take 50 (repeat 1.0))]

numOf = 3
sigs :: EnergyMap [Val]
sigs = M.fromList [ (EnergyIdx 0 0 0 1, take 50 ([0, 2] ++ [3, 3.1 ..])),
                    (EnergyIdx 0 0 1 0, take 50 ([0, 1] ++ [2, 2.1 ..])) ]

linearOne :: TheGraph [Val]
linearOne = TheGraph g sigs
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Sink)]
        es = makeEdges [(0, 1, defaultELabel)]