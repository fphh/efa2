
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error

import Debug.Trace


import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.EqInterpreter
import EFA2.Solver.TermData
import EFA2.Solver.Solve
import EFA2.Solver.Env
import EFA2.Solver.DependencyGraph

import EFA2.Topology.RandomTopology
import EFA2.Topology.Graph

--import EFA2.Graph.GraphData
--import EFA2.Graph.Graph
import EFA2.Signal.Arith

import EFA2.Utils.Utils

--import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

--import EFA2.Example.SymSig

--import EFA2.Example.Dreibein
--import EFA2.Example.Linear
--import EFA2.Example.LinearOne
--import EFA2.Example.LinearX
--import EFA2.Example.LinearTwo
--import EFA2.Example.Loop
--import EFA2.Example.Circular
--import EFA2.Example.Vierbein





main :: IO ()
main = do
  let g = randomTopology 12 10 2.0

{-
      terms :: [EqTerm]
      terms = [ 
                mkVar (VarIdx 1) := mkVar (VarIdx 0),
                mkVar (VarIdx 0) := mkVar (PowerIdx 0 2),
                EtaIdx 0 1 .= [0.1, 0.2, 0.5, 0.8 :: Val],
                PowerIdx 0 2 .= [2.2, 6, 2, 2.6 :: Val],
                mkVar (PowerIdx 0 3) := FAbs (mkVar (PowerIdx 0 1)) (mkVar (EtaIdx 0 1)),
                mkVar (VarIdx 0) := (mkVar (PowerIdx 0 1)) :* (mkVar (EtaIdx 0 1)) ]
-}
      terms = [ PowerIdx 0 1 .= [2.2, 2.5, 2.7 :: Val] ]

      xenvts = envToEqTerms (randomXEnv 0 3 g)
      eenvts = envToEqTerms (randomEtaEnv 17 5 g)

      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq g ++ mkNodeEq g
      depg1 = dpgDiffByAtMostOne ts
      depg2 = dpgHasSameVariable ts

      ho = hornOrder ts
      dirs = directEquations ho

{-
  drawAll [
    drawTopologyX' g,
    drawDependencyGraph depg1,
    --drawDependencyGraph depg2,
    --drawDependencyGraph (dpg terms),
    --putStrLn (hornsToStr $ snd $ makeHornClauses ts),
   -- putStrLn (showEqTerms ts),
    -- drawDependencyGraph (transClose depg),
    putStrLn ("Solution order:\n" ++ showEqTerms ho) ]
-}

  putStrLn (showEqTerms ts)
{-
  putStrLn ("Dir equations:\n" ++ showEqTerms dirs)

  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn ("Number of equatins solved: " ++ show (length dirs))
  drawTopologyX' g

-}