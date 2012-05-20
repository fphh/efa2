{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EFA2.Example.Loop (loop, etas, pows, dtimes) where

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

dtimes :: DTimeMap [Val]
dtimes = M.fromList [(DTimeIdx 0 0, [2, 2, 2])]

sigs :: EnergyMap [Val]
sigs = M.fromList [ (EnergyIdx 0 0 0 1, [2.3, 2.4, 3]),
                    (EnergyIdx 0 0 1 0, [2, 2.1, 2.2]),
                    (EnergyIdx 0 0 1 2, replicate numOf 1.8),
                    (EnergyIdx 0 0 2 1, replicate numOf 1.0),
                    (EnergyIdx 0 0 2 3, replicate numOf 1.1),
                    (EnergyIdx 0 0 3 2, replicate numOf 0.5),
                    (EnergyIdx 0 0 1 4, replicate numOf 0.4),
                    (EnergyIdx 0 0 4 1, replicate numOf 0.3),
                    (EnergyIdx 0 0 4 5, replicate numOf 0.1),
                    (EnergyIdx 0 0 5 4, replicate numOf 0.05),
                    (EnergyIdx 0 0 4 2, replicate numOf 0.2),
                    (EnergyIdx 0 0 2 4, replicate numOf 0.1) ]

etas :: FEtaMap [Val]
etas = M.fromList [ (FEtaIdx 0 0 0 1, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 1 0, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 1 2, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 2 1, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 2 3, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 3 2, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 1 4, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 4 1, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 4 5, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 5 4, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 4 2, map (\x -> x/(x+1))),
                    (FEtaIdx 0 0 2 4, map (\x -> x/(x+1))) ]


pows :: PowerMap [Val]
pows = M.fromList [ (PowerIdx 0 0 0 1, [1, 2, 3]),
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
loop = TheGraph g sigs
  where g = mkGraph ns es
        es = makeWithDirEdges (pairs no)
             ++ makeEdges [(2, 4, defaultELabel { flowDirection =  AgainstDir})] -- Why do we have to do this?
             ++ makeWithDirEdges (pairs (1:no2))
        no = [0..3]
        no2 = [4, 5]
        ns = makeNodes $ map f (no ++ no2)
        f x = (x, Crossing)
