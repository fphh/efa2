{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EFA2.Example.Loop where

import Data.Graph.Inductive
import qualified Data.Map as M

import Control.Monad.Error


import EFA2.Signal.Arith
import EFA2.Term.TermData
import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Example.SymSig


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

instance Signal Val where
         signal = sigs
         toSignal = id

instance Signal InTerm where
         signal = symSig sigs
         toSignal = InConst

loop :: (Signal a) => (Gr NLabel ELabel, LRPowerEnv [a])
loop = (g, signal)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2) ++ makeEdges [4, 2])
        no = [0..3]
        no2 = [4, 5]
