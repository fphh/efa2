
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
import EFA2.Example.LinearOne


main :: IO ()
main = do
  let TheGraph g sigs = linearOne
      (_, ts) = makeAllEquations g [envs0]

      envs0 = emptyEnv { energyMap = sigs0, dtimeMap = dtimes0, 
                         fetaMap = M.fromList [(FEtaIdx 0 0 1 0, smap (const 0.8)), (FEtaIdx 0 0 0 1, smap (const 0.7))] }

      envs1 = emptyEnv { energyMap = sigs1, dtimeMap = dtimes1, fetaMap = M.fromList [(FEtaIdx 0 1 1 0, id)] }


      ts' = map (eqToInTerm envs0) (order ts)

      res = interpretFromScratch 1 ts'

  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn "===================="
  putStrLn (showEqTerms ts)
  putStrLn "===================="
  putStrLn (showInTerms ts')
  putStrLn "===================="
  putStrLn ("Number of undeq: " ++ show (length ts'))
  putStrLn ("Number of deq:   " ++ show (length ts'))

  drawTopology g res