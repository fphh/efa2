


module EFA2.Example.Vierbein (vierbein) where

import Data.Graph.Inductive
import qualified Data.Map as M

import EFA2.Topology.TopologyData (NodeType(Crossing))
import EFA2.Topology.Topology

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils

numOf :: Int
numOf = 3

sigs :: PowerMap [Val]
sigs = M.fromList [ (PowerIdx 0 0 0 2, replicate numOf 3.0),
                    (PowerIdx 0 0 2 0, replicate numOf 2.2),
                    (PowerIdx 0 0 2 3, replicate numOf 1.0),
                    (PowerIdx 0 0 3 2, replicate numOf 0.6),
                    (PowerIdx 0 0 1 2, replicate numOf 2.0),
                    (PowerIdx 0 0 2 1, replicate numOf 1.8),
                    (PowerIdx 0 0 2 4, replicate numOf 3.0),
                    (PowerIdx 0 0 4 2, replicate numOf 1.0) ]


vierbein :: TheGraph [Val]
vierbein = TheGraph g sigs
   where g = mkGraph (makeNodes $ map f ns) (makeEdges (pairs es1) ++ makeEdges (pairs es2))
         ns = [0..4]
         es1 = [1, 2, 3]
         es2 = [0, 2, 4]
         f x = (x, Crossing)
