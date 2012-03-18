{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Dreibein where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import EFA2.Graph.Graph
import EFA2.Graph.GraphData
import EFA2.Signal.SignalData
import EFA2.Signal.Sequence
import EFA2.Signal.SignalGeneration

t :: Time
t = dfromList [TSample 0, TSample 1]

sig :: Int -> Int -> Power
sig 0 1 = list2Power [3.0, 3.0]
sig 1 0 = list2Power [2.2, 2.2]
sig 1 2 = list2Power [1.8, 1.8]
sig 2 1 = list2Power [1, 1]
sig 1 3 = list2Power [0.4, 0.4]
sig 3 1 = list2Power [0.2, 0.2]

-- dreibein :: (Gr NLabel ELabel, Record)
-- dreibein = (g, Record t M.fromList sigs)
--   where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2))
--         no = [0..2]
--         no2 = [3]
--         sigs = concatMap f (edges g)
--         f (x, y) = [(mkIdx x y, sig x y), (mkIdx y x, sig y x)]