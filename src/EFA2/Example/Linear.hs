{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module EFA2.Example.Linear (linear) where

import Data.Graph.Inductive
import qualified Data.Map as M

import Control.Monad.Error

import EFA2.Topology.Topology

import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Example.SymSig


numOf = 3

sigs :: PowerMap [Val]
sigs = M.fromList [ (PowerIdx 0 0 0 1, replicate numOf 3.0),
                    (PowerIdx 0 0 1 0, replicate numOf 2.2),
                    (PowerIdx 0 0 1 2, replicate numOf 2.2),
                    (PowerIdx 0 0 2 1, replicate numOf 1.0),
                    (PowerIdx 0 0 2 3, replicate numOf 1.0),
                    (PowerIdx 0 0 3 2, replicate numOf 0.4) ]


linear :: TheGraph [Val]
linear = TheGraph g sigs
  where g = mkGraph (makeNodes no) (makeEdges no)
        no = [0..3]
