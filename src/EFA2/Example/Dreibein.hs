

module EFA2.Example.Dreibein (dreibein) where

import Data.Graph.Inductive
import qualified Data.Map as M

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Topology.Graph
import EFA2.Example.SymSig



numOf = 3

sigs :: PowerMap [Val]
sigs = M.fromList [ (PowerIdx 0 0 0 1, replicate numOf 3.0),
                    (PowerIdx 0 0 1 0, replicate numOf 2.2),
                    (PowerIdx 0 0 1 2, replicate numOf 1.8),
                    (PowerIdx 0 0 2 1, replicate numOf 1.0),
                    (PowerIdx 0 0 1 3, replicate numOf 0.4),
                    (PowerIdx 0 0 3 1, replicate numOf 0.2) ]


dreibein :: TheGraph [Val]
dreibein = TheGraph g sigs
   where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2))
         no = [0..2]
         no2 = [3]
 