
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import System.IO

import Debug.Trace


import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.DependencyGraph
import EFA2.Solver.IsVar
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith

import EFA2.Signal.Signal

import EFA2.Topology.Topology
--import EFA2.Topology.RandomTopology

import EFA2.Utils.Utils

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig
--import EFA2.Example.Loop
import EFA2.Example.LinearTwo


main :: IO ()
main = do
  let TheGraph g _ = linearTwo

      envs0 = emptyEnv { recordNumber = SingleRecord 0,
                         energyMap = sigs0ss,
                         dtimeMap = dtimes0ss,
                         fetaMap = eta0ss }

      envs1 = emptyEnv { recordNumber = SingleRecord 1,
                         energyMap = sigs1ss,
                         dpowerMap = dpower1ss,
                         detaMap = deta1ss,
                         dtimeMap = dtimes1ss,
                         fetaMap = eta1ss }


      (envs0', ts0) = makeAllEquations g [envs0]
      (envs1', ts1) = makeAllEquations g [envs1]
      envs = envUnion [envs0', envs1']
      t = map (eqToInTerm envs) $ mkAllDiffEqs 1 0 g

      ts = map (eqToInTerm envs) (order (ts0 ++ ts1))

      ts' = ts ++ [x] ++ t ++ [y] 
      [x, y] = mkDiffEquations 1 (map (eqToInTerm envs) ts1)

      res = interpretFromScratch (recordNumber envs) 1 (toAbsEquations ts')

  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn (showEqTerms $ mkAllDiffEqs 1 0 g)
  putStrLn "===================="
  putStrLn (showEqTerms (ts0 ++ ts1))
  putStrLn "===================="
  putStrLn (showInTerms ts')
  putStrLn "===================="

  putStrLn ("Number of undeq: " ++ show (length ts'))
  putStrLn ("Number of deq:   " ++ show (length ts'))

  --drawTopology g res --  (res { recordNumber = SingleRecord 0 })
  print (mapEnv showInTerm res)

