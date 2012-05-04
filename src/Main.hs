
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
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith

import EFA2.Topology.Topology

import EFA2.Utils.Utils

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig
import EFA2.Example.Zweibein
import EFA2.Example.Loop


main :: IO ()
main = do
  let TheGraph g sigs = loop
      penvts = envToEqTerms sigs
      ts = penvts ++ makeAllEquations g
 
      isV = isVarFromEqs ts
      (given, nov, givExt, rest) = splitTerms isV ts


      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho
      envs = Envs sigs M.empty M.empty M.empty M.empty M.empty

      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd


  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn (showEqTerms ts)
  putStrLn "===================="
  putStrLn ("Number of undeq: " ++ show (length ts))
  putStrLn ("Number of given: " ++ show (length given))
  putStrLn ("Number of noVar: " ++ show (length nov))

  putStrLn ("Number of gvExt: " ++ show (length givExt))
  putStrLn ("Number of rest : " ++ show (length rest))

  putStrLn "===================="
  putStrLn ("Number of equations solved: " ++ show (length dirs))
  putStrLn (showInTerms gd)

  drawTopology g res