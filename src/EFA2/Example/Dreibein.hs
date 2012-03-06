{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Dreibein where

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
sig 1 3 = UV.fromList [0.4]
sig 3 1 = UV.fromList [0.2]

dreibein :: (Gr NLabel ELabel, SampleEnv PSample)
dreibein = (g, M.fromList sigs)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2))
        no = [0..2]
        no2 = [3]
        sigs = concatMap f (edges g)
        f (x, y) = [(mkIdx x y, sig x y), (mkIdx y x, sig y x)]