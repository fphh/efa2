{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Circular where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import EFA2.Graph.Graph
import EFA2.Signal.SignalData


sig :: Int -> Int -> Signal PSample
sig 0 1 = UV.fromList [3]
sig 1 0 = UV.fromList [2.2]
sig 1 2 = UV.fromList [1.8]
sig 2 1 = UV.fromList [1]
sig 2 3 = UV.fromList [1.1]
sig 3 2 = UV.fromList [0.5]

sig 1 4 = UV.fromList [0.3]
sig 4 1 = UV.fromList [0.4]
sig 4 5 = UV.fromList [0.05]
sig 5 4 = UV.fromList [0.1]

sig 4 2 = UV.fromList [0.1]
sig 2 4 = UV.fromList [0.2]


circular :: (Gr NLabel ELabel, SampleEnv PSample)
circular = (g, M.fromList sigs)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (no2 ++ [1]) ++ makeEdges [2, 4])
        no = [0..3]
        no2 = [5, 4]
        sigs = concatMap f (edges g)
        f (x, y) = [(mkIdx x y, sig x y), (mkIdx y x, sig y x)]
