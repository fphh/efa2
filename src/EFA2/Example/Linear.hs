{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Linear where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import EFA2.Graph.Graph
--import EFA2.Graph.GraphData
--import EFA2.Signal.Sequence
--import EFA2.Signal.SignalGeneration
import EFA2.Signal.SignalData


sig :: Int -> Int -> Signal Vec PSample
sig 0 1 = toVSig [3.0, 3.0]
sig 1 0 = toVSig [2.2, 2.2]
sig 1 2 = toVSig [1.8, 1.8]
sig 2 1 = toVSig [1.0, 1.0]
sig 2 3 = toVSig [0.4, 0.4]
sig 3 2 = toVSig [0.2, 0.2]


linear :: (Gr NLabel ELabel, PowerEnv (Signal Vec PSample))
linear = (g, M.fromList sigs)
  where g = mkGraph (makeNodes no) (makeEdges no)
        no = [0..3]
        sigs = concatMap f (edges g)
        f (x, y) = [(mkPowerIdx x y, sig x y), (mkPowerIdx y x, sig y x)]