
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
import EFA2.Topology.Topology

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
import EFA2.Example.Loop
--import EFA2.Example.Circular
import EFA2.Example.Vierbein


--topo :: Gr NLabel ELabel
--topo = mkGraph (map mkLNode [0, 1, 2, 3, 6]) (map (uncurry mkLEdge) [(0, 1), (2, 1), (1, 3), (1,6)])

 


main :: IO ()
main = do
  let g = randomTopology 41 10 3.0
      --g = randomTopology 0 100 4.0
      --TheGraph g _ = loop
      --TheGraph g sigs = dreibein
      sigs = M.fromList [ (PowerIdx 0 0 0 1, [3.0]) ]

      xsigs = (randomXEnv 17 1 g)
      esigs = randomEtaEnv 17 1 g

      penvts = envToEqTerms sigs
      xenvts = envToEqTerms xsigs
      eenvts = envToEqTerms esigs

      --ts = penvts ++ mkEdgeEq g ++ mkNodeEq g
      ts = penvts ++ xenvts ++ eenvts ++ mkEdgeEq g ++ mkNodeEq g
      varset = L.foldl' f S.empty ts
      f acc (v := Given) = S.insert v acc
      f acc _ = acc
      isV = isVarFromEqs varset

      (given, nov, givExt, rest) = splitTerms isV ts

      dpg = dpgDiffByAtMostOne isV (givExt ++ rest)
      dpg2 = dpgHasSameVariable isV (givExt ++ rest)
      dpg3 = L.foldl' (flip delEdge) dpg2 (edges dpg)

      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho
      envs = Envs sigs M.empty esigs M.empty xsigs M.empty
      --envs = Envs sigs M.empty M.empty M.empty M.empty M.empty

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
  --putStrLn (showEqTerms ts)
  putStrLn (showInTerms gd)
  --putStrLn stderr (show res)
  --drawTopologyX' g

{-
  drawAll [
    drawTopologyX' g,

    drawTopology g res ]
-}