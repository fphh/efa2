
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import System.IO

import Debug.Trace


import EFA.Solver.Equation
import EFA.Solver.Horn
import EFA.Solver.DirEquation
import EFA.Solver.DependencyGraph
import EFA.Solver.IsVar
import EFA.Solver.EquationOrder

import EFA.Interpreter.Interpreter
import EFA.Interpreter.InTerm
import EFA.Equation.Env
import EFA.Interpreter.Arith

import EFA.Topology.Topology

import EFA.Utility

import EFA.Display.FileSave
import EFA.Graph.Draw

import EFA.Example.SymSig
import EFA.Example.Loop


main :: IO ()
main = do
  let TheGraph g sigs = loop
      -- penvts = envToEqTerms sigs
      (_, ts) = makeAllEquations g [envs]

      envs = emptyEnv { powerMap = sigs }
      ts' = map (eqToInTerm envs) (order ts)

      res :: Envs [Val]
      res = interpretFromScratch ts'


  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn "===================="

  putStrLn (showInTerms ts')
  putStrLn "===================="
  putStrLn ("Number of undeq: " ++ show (length ts))
  putStrLn ("Number of deq:   " ++ show (length ts'))


  drawTopology g res