{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EFA2.Example.Loop (loop) where

import Data.Graph.Inductive
import qualified Data.Map as M

import Control.Monad.Error


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils

import Debug.Trace

numOf = 3

sigs :: PowerMap [Val]
sigs = M.fromList [ (PowerIdx 0 0 0 1, replicate numOf 3.0),
                    (PowerIdx 0 0 1 0, replicate numOf 2.2),
                    (PowerIdx 0 0 1 2, replicate numOf 1.8),
                    (PowerIdx 0 0 2 1, replicate numOf 1.0),
                    (PowerIdx 0 0 2 3, replicate numOf 1.1),
                    (PowerIdx 0 0 3 2, replicate numOf 0.5),
                    (PowerIdx 0 0 1 4, replicate numOf 0.4),
                    (PowerIdx 0 0 4 1, replicate numOf 0.3),
                    (PowerIdx 0 0 4 5, replicate numOf 0.1),
                    (PowerIdx 0 0 5 4, replicate numOf 0.05),
                    (PowerIdx 0 0 4 2, replicate numOf 0.2),
                    (PowerIdx 0 0 2 4, replicate numOf 0.1) ]


loop :: TheGraph [Val]
loop = trace (show es) $ TheGraph g sigs
  where g = mkGraph ns es
        es = makeWithDirEdges (pairs no) ++ makeEdges [(2, 4, ELabel AgainstDir)] ++ makeWithDirEdges (pairs (1:no2))
        no = [0..3]
        no2 = [4, 5]
        ns = makeNodes $ map f (no ++ no2)
        f x = (x, Crossing)
