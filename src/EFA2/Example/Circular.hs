

module EFA2.Example.Circular where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Graph.Graph
import EFA2.Signal.SignalData
import EFA2.Signal.TH


sigs :: LRPowerEnv [Val]
sigs (PowerIdx 0 1) = return [3]
sigs (PowerIdx 1 0) = return [2.2]
sigs (PowerIdx 1 2) = return [1.8]
sigs (PowerIdx 2 1) = return [1]
sigs (PowerIdx 2 3) = return [1.1]
sigs (PowerIdx 3 2) = return [0.5]

sigs (PowerIdx 1 4) = return [0.3]
sigs (PowerIdx 4 1) = return [0.4]
sigs (PowerIdx 4 5) = return [0.05]
sigs (PowerIdx 5 4) = return [0.1]

sigs (PowerIdx 4 2) = return [0.1]
sigs (PowerIdx 2 4) = return [0.2]
sigs idx = throwError (PowerIdxError idx)


circular :: (Gr NLabel ELabel, LRPowerEnv [Val])
circular = (g, sigs)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (no2 ++ [1]) ++ makeEdges [2, 4])
        no = [0..3]
        no2 = [5, 4]
