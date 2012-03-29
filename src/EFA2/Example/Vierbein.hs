


module EFA2.Example.Vierbein where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Graph.Graph
import EFA2.Signal.SignalData
import EFA2.Signal.TH

sigs :: LRPowerEnv [Val]
sigs (PowerIdx 0 2) = return [3.0]
sigs (PowerIdx 2 0) = return [2.2]
sigs (PowerIdx 2 3) = return [1.0]
sigs (PowerIdx 3 2) = return [0.6]

sigs (PowerIdx 1 2) = return [2.0]
sigs (PowerIdx 2 1) = return [1.8]
sigs (PowerIdx 2 4) = return [3.0]
sigs (PowerIdx 4 2) = return [1.0]
sigs idx = throwError (PowerIdxError idx)


vierbein :: (Gr NLabel ELabel, LRPowerEnv [Val])
vierbein = (g, sigs)
   where g = mkGraph (makeNodes ns) (makeEdges es1 ++ makeEdges es2)
         ns = [0..4]
         es1 = [1, 2, 3]
         es2 = [0, 2, 4]
 