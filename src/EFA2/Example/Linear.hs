{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Linear where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Error

import EFA2.Graph.Graph
import EFA2.Signal.SignalData
import EFA2.Signal.TH


sigs :: LRPowerEnv [Val]
sigs (PowerIdx 0 1) = return [3.0, 3.0]
sigs (PowerIdx 1 0) = return [2.2, 2.2]
sigs (PowerIdx 1 2) = return [1.8, 1.8]
sigs (PowerIdx 2 1) = return [1.0, 1.0]
sigs (PowerIdx 2 3) = return [0.4, 0.4]
sigs (PowerIdx 3 2) = return [0.2, 0.2]
sigs idx = throwError (PowerIdxError idx)

linear :: (Gr NLabel ELabel, LRPowerEnv [Val])
linear = (g, sigs)
  where g = mkGraph (makeNodes no) (makeEdges no)
        no = [0..3]
