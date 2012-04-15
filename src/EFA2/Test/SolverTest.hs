{-# LANGUAGE TemplateHaskell #-}


module EFA2.Test.SolverTest where



import qualified Data.Set as S
import qualified Data.List as L
import Data.Graph.Inductive

import Test.QuickCheck
import Test.QuickCheck.All


import EFA2.Topology.RandomTopology
import EFA2.Topology.Graph
import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.Env
import EFA2.Solver.DirEquation
import EFA2.Signal.Arith


prop_solver :: Int -> Double -> Bool
prop_solver numOfNodes ratio | numOfNodes > 1 && numOfNodes < 300 && ratio > 2.0 && ratio < 5.0 = length dirs == 2*(length $ edges g)
  where seed = 12
        g = randomTopology seed numOfNodes ratio
        terms = [ PowerIdx 0 0 0 1 .= [2.2 :: Val] ]
        xenvts = envToEqTerms (randomXEnv 0 0 g)
        eenvts = envToEqTerms (randomEtaEnv 17 0 g)

        ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
        ho = hornOrder ts
        dirs = directEquations ho
prop_solver _ _ = True

runTests = $quickCheckAll
