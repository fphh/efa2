


module EFA2.Example.Vierbein (vierbein) where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Signal.Arith
import EFA2.Term.TermData
import EFA2.Term.Env
import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Example.SymSig

numOf = 3

sigs :: LRPowerEnv [Val]
sigs (PowerIdx 0 2) = return (replicate numOf 3.0)
sigs (PowerIdx 2 0) = return (replicate numOf 2.2)
sigs (PowerIdx 2 3) = return (replicate numOf 1.0)
sigs (PowerIdx 3 2) = return (replicate numOf 0.6)

sigs (PowerIdx 1 2) = return (replicate numOf 2.0)
sigs (PowerIdx 2 1) = return (replicate numOf 1.8)
sigs (PowerIdx 2 4) = return (replicate numOf 3.0)
sigs (PowerIdx 4 2) = return (replicate numOf 1.0)
sigs idx = throwError (PowerIdxError idx M.empty)


vierbein :: (Signal a) => TheGraph [a]
vierbein = TheGraph g (signal sigs)
   where g = mkGraph (makeNodes ns) (makeEdges es1 ++ makeEdges es2)
         ns = [0..4]
         es1 = [1, 2, 3]
         es2 = [0, 2, 4]
 