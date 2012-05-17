
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

import EFA2.Topology.Topology
import EFA2.Topology.RandomTopology

import EFA2.Utils.Utils

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig
import EFA2.Example.Loop


main :: IO ()
main = do
  let TheGraph g sigs = loop
      -- g = randomTopology 12 100 3.0
      --xenv = randomXEnv 14 3 g
      --nenv = randomEtaEnv 34 3 g
      -- sigs = M.fromList [(EnergyIdx 0 0 0 1, [1])]
      (_, ts) = makeAllEquations g [envs]


      envs = emptyEnv { energyMap = sigs }
      ts' = map (eqToInTerm envs) (order ts)

      res :: Envs [Val]
      res = interpretFromScratch ts'

      e = emptyEnv { energyMap = M.fromList [(EnergyIdx 0 0 0 1, [1, 2, 3])], xMap = xMap res, etaMap = etaMap res }
      (_, ts'') = makeAllEquations g [e]
      ts''' = map (eqToInTerm e) (order ts'')

      res2 :: Envs [Val]

      res2 = interpretFromScratch ts'''

  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn "===================="
  putStrLn (showEqTerms ts'')
  putStrLn "===================="
  putStrLn (showInTerms ts''')
  putStrLn "===================="
  putStrLn ("Number of undeq: " ++ show (length ts))
  putStrLn ("Number of deq:   " ++ show (length ts'''))

  print res2
  drawTopology g res2