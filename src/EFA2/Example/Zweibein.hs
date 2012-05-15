


module EFA2.Example.Zweibein (zweibein) where

import Data.Graph.Inductive
import qualified Data.Map as M

import Control.Monad.Error


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils

import Debug.Trace

numOf = 3

sigs :: PowerMap [Val]
sigs = M.fromList [ (PowerIdx 0 0 0 1, replicate numOf 3.0),
                    (PowerIdx 0 0 1 0, replicate numOf 2.2),
                    (PowerIdx 0 0 0 2, replicate numOf 2.0),
                    (PowerIdx 0 0 2 0, replicate numOf 1.7) ]


zweibein :: TheGraph [Val]
zweibein = TheGraph g sigs
  where g = mkGraph ns es
        es = makeWithDirEdges [(0, 1), (0, 2)]
        ns = makeNodes $ map f [0, 1, 2]
        f x = (x, Crossing)
