

module EFA2.Example.Loop where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Graph.Graph
import EFA2.Graph.GraphData
import EFA2.Signal.SignalData
import EFA2.Signal.TH

import EFA2.Term.TermData

sigs :: LRPowerEnv [Val]

sigs (PowerIdx 0 1) = return [3.0]
sigs (PowerIdx 1 0) = return [2.2]
sigs (PowerIdx 1 2) = return [1.8]
sigs (PowerIdx 2 1) = return [1.0]
sigs (PowerIdx 2 3) = return [1.1]
sigs (PowerIdx 3 2) = return [0.5]

sigs (PowerIdx 1 4) = return [0.4]
sigs (PowerIdx 4 1) = return [0.3]
sigs (PowerIdx 4 5) = return [0.1]
sigs (PowerIdx 5 4) = return [0.05]

sigs (PowerIdx 4 2) = return [0.2]
sigs (PowerIdx 2 4) = return [0.1]
sigs idx = throwError (PowerIdxError idx)



symSigs :: LRPowerEnv [InTerm Abs]
symSigs (PowerIdx 0 1) = return [InConst 3.0]
symSigs (PowerIdx 1 0) = return [InConst 2.2]
symSigs (PowerIdx 1 2) = return [InConst 1.8]
symSigs (PowerIdx 2 1) = return [InConst 1.0]
symSigs (PowerIdx 2 3) = return [InConst 1.1]
symSigs (PowerIdx 3 2) = return [InConst 0.5]

symSigs (PowerIdx 1 4) = return [InConst 0.4]
symSigs (PowerIdx 4 1) = return [InConst 0.3]
symSigs (PowerIdx 4 5) = return [InConst 0.1]
symSigs (PowerIdx 5 4) = return [InConst 0.05]

symSigs (PowerIdx 4 2) = return [InConst 0.2]
symSigs (PowerIdx 2 4) = return [InConst 0.1]
symSigs idx = throwError (PowerIdxError idx)



loop :: (Gr NLabel ELabel, LRPowerEnv [InTerm Abs])
loop = (g, symSigs)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2) ++ makeEdges [4, 2])
        no = [0..3]
        no2 = [4, 5]
