

module EFA2.Example.Dreibein (dreibein) where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Signal.Arith
import EFA2.Term.TermData
import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Example.SymSig

numOf = 3

sigs :: LRPowerEnv [Val]
sigs (PowerIdx 0 1) = return (replicate numOf 3.0)
sigs (PowerIdx 1 0) = return (replicate numOf 2.2)
sigs (PowerIdx 1 2) = return (replicate numOf 1.8)
sigs (PowerIdx 2 1) = return (replicate numOf 1.0)
sigs (PowerIdx 1 3) = return (replicate numOf 0.4)
sigs (PowerIdx 3 1) = return (replicate numOf 0.2)
sigs idx = throwError (PowerIdxError idx)


dreibein :: (Signal a) => TheGraph [a]
dreibein = TheGraph g (signal sigs)
   where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2))
         no = [0..2]
         no2 = [3]
 