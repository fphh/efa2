{-# LANGUAGE TemplateHaskell #-}


module EFA.Test.IsVar where



import qualified Data.Set as S
import qualified Data.List as L
import Data.Graph.Inductive

import Test.QuickCheck
import Test.QuickCheck.All

import Debug.Trace


import EFA.Topology.RandomTopology
import EFA.Topology.Topology
import EFA.Solver.Equation
import EFA.Solver.IsVar
import EFA.Interpreter.Arith
import EFA.Equation.Env
import EFA.Utility


{-
prop_splitTerms :: Int -> Gen Bool
prop_splitTerms seed = do
  ratio <- choose (2.0, 4.0)
  let numOfNodes = 50
      g = randomTopology seed numOfNodes ratio
      terms = map give [ PowerIdx 0 0 0 1 ]
      xenvts = envToEqTerms (randomXEnv 0 0 g)
      eenvts = envToEqTerms (randomEtaEnv 17 0 g)
      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq g ++ mkNodeEq g
      isV = isVar g ts
      (given, nov, givenExt, rest) = splitTerms isV ts
  return $ length given + length nov + length givenExt + length rest == length ts
-}

prop_isVar :: Int -> Gen Bool
prop_isVar seed = do
  ratio <- choose (2.0, 6.0)
  let numOfNodes = 50
      g = randomTopology seed numOfNodes ratio
      terms = map give [ PowerIdx 0 0 0 1 ]
      xenvts = envToEqTerms (randomXEnv 3 1 g)
      eenvts = envToEqTerms (randomEtaEnv 17 1 g)
      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq g ++ mkNodeEq g
      isV1 = isVar g ts
      isV2 = isVarFromEqs ts
  return $ all (\t -> isV1 t == isV2 t) ts

runTests = $quickCheckAll
