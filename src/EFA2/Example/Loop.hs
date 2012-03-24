


{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}


module EFA2.Example.Loop where

import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import EFA2.Graph.Graph
import EFA2.Signal.SignalData


sig :: Int -> Int -> Signal Vec PSample
sig 0 1 = toVSig [3]
sig 1 0 = toVSig [2.2]
sig 1 2 = toVSig [1.8]
sig 2 1 = toVSig [1]
sig 2 3 = toVSig [1.1]
sig 3 2 = toVSig [0.5]

sig 1 4 = toVSig [0.4]
sig 4 1 = toVSig [0.3]
sig 4 5 = toVSig [0.1]
sig 5 4 = toVSig [0.05]

sig 4 2 = toVSig [0.2]
sig 2 4 = toVSig [0.1]


loop :: (Gr NLabel ELabel, PowerEnv (Signal Vec PSample))
loop = (g, M.fromList sigs)
  where g = mkGraph (makeNodes no ++ makeNodes no2) (makeEdges no ++ makeEdges (1:no2) ++ makeEdges [4, 2])
        no = [0..3]
        no2 = [4, 5]
        sigs = concatMap f (edges g)
        f (x, y) = [(mkPowerIdx x y, sig x y), (mkPowerIdx y x, sig y x)]
