{-# LANGUAGE TemplateHaskell #-}


module EFA2.Test.SolverTest where



import qualified Data.Set as S
import qualified Data.List as L
import Data.Graph.Inductive

import Test.QuickCheck
import Test.QuickCheck.All

import Debug.Trace


import EFA2.Topology.RandomTopology
import EFA2.Topology.Graph
import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.Env
import EFA2.Solver.IsVar
import EFA2.Solver.DirEquation
import EFA2.Signal.Arith
import EFA2.Utils.Utils


-- | Given x and eta environments, the number of all solved (directed) equations should be equal the
-- double of the number of edges in the graph, that is, every power position has been calculated.
-- This is a good example for the use of various functions together.
prop_solver :: Int -> Gen Bool
prop_solver seed = do
  ratio <- choose (2.0, 5.0)
  let g = randomTopology 0 50 ratio

      terms = map give [ PowerIdx 0 0 0 1 ]

      xenvts = envToEqTerms (randomXEnv 0 1 g)
      eenvts = envToEqTerms (randomEtaEnv 17 1 g)

      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
      isV = isVar g ts
      (given, nov, givExt, rest) = splitTerms isV ts
      ss = givExt ++ rest

      ho = hornOrder isV ss
      dirs = directEquations isV ho

  return $ length dirs == 2*(length (edges g)) - 1 -- minus one, because one PowerIdx is given.


prop_orderOfEqs :: Int ->  Gen Bool
prop_orderOfEqs seed = do
  ratio <- choose (2.0, 6.0)
  let g = randomTopology seed 50 ratio

      terms = map give [ PowerIdx 0 0 0 1 ]

      xenvts = envToEqTerms (randomXEnv 0 1 g)
      eenvts = envToEqTerms (randomEtaEnv 17 1 g)

      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
      isV = isVar g ts
      (given, nov, givExt, rest) = splitTerms isV ts
      ss = givExt ++ rest

      ho = hornOrder isV ss
      dirs = directEquations isV ho
      dirsets = reverse $ L.foldl' f [S.empty] (map (mkVarSet isV) dirs) -- For _:a:b:_, b includes a
      f (a:acc) s = (S.union a s):a:acc
      atMostOneMore (s, t) = S.size (s S.\\ t) <= 1

  return $ all atMostOneMore (pairs dirsets)

runTests = $quickCheckAll
