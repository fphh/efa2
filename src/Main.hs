
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


import EFA2.Topology.RandomTopology
import EFA2.Topology.Graph
import EFA2.Topology.GraphData

import EFA2.Interpreter.Arith

import EFA2.Utils.Utils

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig

import EFA2.Example.Dreibein
import EFA2.Example.Linear
--import EFA2.Example.LinearOne
--import EFA2.Example.LinearX
--import EFA2.Example.LinearTwo
--import EFA2.Example.Loop
--import EFA2.Example.Circular
import EFA2.Example.Vierbein


topo :: Gr NLabel ELabel
topo = mkGraph (map mkLNode [0..4]) (map (uncurry mkLEdge) [(1, 0), (1, 2), (3, 1), (4, 1)])

 


main :: IO ()
main = do
  let --g = randomTopology 41 5 2
      g = randomTopology 0 10 4.0
      --TheGraph g sigs = vierbein

      sigs = M.fromList [ (PowerIdx 0 0 0 1, [2, 3, 4.5]) ]
      xsigs = randomXEnv 17 3 g
      esigs = randomEtaEnv 17 3 g

      penvts = envToEqTerms sigs
      xenvts = envToEqTerms xsigs
      eenvts = envToEqTerms esigs

      --ts = penvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g
      ts = penvts ++ xenvts ++ eenvts ++ mkEdgeEq 0 0 g ++ mkNodeEq 0 0 g

      isV = isVar g ts
      (given, nov, givExt, rest) = splitTerms isV ts
      ss = givExt ++ rest

      ho = hornOrder isV ss
      dirs = directEquations isV ho
      envs = Envs sigs  esigs M.empty M.empty xsigs M.empty
      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd

  putStrLn ("Number of nodes: " ++ show (noNodes g))
  putStrLn ("Number of edges: " ++ show (length $ edges g))
  putStrLn "===================="
  putStrLn ("Number of undeq: " ++ show (length ts))
  putStrLn ("Number of given: " ++ show (length given))
  putStrLn ("Number of noVar: " ++ show (length nov))
  putStrLn ("Number of gvExt: " ++ show (length givExt))
  putStrLn ("Number of rest : " ++ show (length rest))
  putStrLn "===================="
  putStrLn ("Number of equations solved: " ++ show (length dirs))
  --putStrLn stderr (showInTerms gd)
  --hPutStrLn stderr (show res)

  drawTopology g res

  --drawTopologyX' g
 