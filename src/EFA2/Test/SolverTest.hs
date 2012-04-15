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


-- | Given x and eta environments, the number of all solved (directed) equations should be equal the
-- double of the number of edges in the graph, that is, every power position has been calculated.
-- This is a good example for the use of various functions together.
prop_solver :: Int -> Double -> Gen Prop
prop_solver seed ratio =
  ratio > 2.0 && ratio < 5.0 ==> length dirs == 2*(length $ edges g)
  where numOfNodes = 50
        --seed = 0
        g = randomTopology seed numOfNodes ratio
        terms = [ PowerIdx 0 0 0 1 .= [2.2 :: Val] ]
        xenvts = envToEqTerms (randomXEnv 0 0 g)
        eenvts = envToEqTerms (randomEtaEnv 17 0 g)

        ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
        ho = hornOrder ts
        dirs = directEquations ho

runTests = $quickCheckAll
