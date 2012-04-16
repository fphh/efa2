
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
import EFA2.Topology.GraphData

--import EFA2.Graph.GraphData
--import EFA2.Graph.Graph
import EFA2.Signal.Arith

import EFA2.Utils.Utils

import EFA2.Display.FileSave
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


topo :: Gr NLabel ELabel
topo = mkGraph (map mkLNode [0..4]) (map (uncurry mkLEdge) [(1, 0), (1, 2), (3, 1), (4, 1)])

 


main :: IO ()
main = do
  let --g = randomTopology 4 4 3
      g = randomTopology 0 1000 3.0

      terms = [ PowerIdx 0 0 0 1 .= [2.2 :: Val] ]

      xenvts = envToEqTerms (randomXEnv 0 1 g)
      eenvts = envToEqTerms (randomEtaEnv 17 1 g)

      ts = terms ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
      isV = isVar -- isVarFromEqs ts

      depg1 = dpgDiffByAtMostOne isV (filter (not . isGiven isV) ts)
      --depg2 = dpgHasSameVariable ts

      ho = hornOrder isV ts
      dirs = directEquations isV ho
      -- (a, fs) = makeHornClauses ts


  -- putStrLn (showEqTerms (filter (not . isGiven isV) ts))

  putStrLn ("Number of undir eq:" ++ show (length ts))
  --putStrLn ("Number of undir feq:" ++ show (length $ filter (not . isGiven isV) ts))

  --putStrLn ("Dir equations:\n" ++ showEqTerms dirs)
  
  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn ("Number of equations solved: " ++ show (length dirs))

  --drawTopologyX' g
  --drawDependencyGraph depg1

  --writeDependencyGraph depg1

  --putStrLn (hornsToStr ho)