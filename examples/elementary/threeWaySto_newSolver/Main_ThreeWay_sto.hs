
module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Display.DrawGraph

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

main :: IO ()
main = do
  
  let s01 = [0, 2, 2, 0]
      s10 = [0, 0.8, 0.8, 0]
      s12 = [0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0]
      s31 = [0, 0.25, 0.25, 0]

      s01' = [0, 0]
      s10' = [0, 0]
      s12' = [0.3, 0.3]
      s21' = [0.2, 0.2]
      s13' = [-0.3, -0.3]
      s31' = [-0.6, -0.6]
      n = 3


      time = [0, 0] ++ (concatMap (replicate 3) [1..])

      pMap =  M.fromList [ (PPosIdx 0 1, concat $ replicate n (s01 ++ s01')),
                           (PPosIdx 1 0, concat $ replicate n (s10 ++ s10')), 
                           (PPosIdx 1 2, concat $ replicate n (s12 ++ s12')),
                           (PPosIdx 2 1, concat $ replicate n (s21 ++ s21')),
                           (PPosIdx 1 3, concat $ replicate n (s13 ++ s13')),
                           (PPosIdx 3 1, concat $ replicate n (s31 ++ s31')) ]

      (sqEnvs, sqTopo) = makeSequence (PowerRecord time pMap) topo 

      storage0 = PowerIdx (-1) 0 24 25
      --storage0 = PowerIdx (-1) 0 16 17

      (sqEnvs', ts') = makeAllEquations sqTopo sqEnvs
      sigs = M.unions (map powerMap sqEnvs')
      ts = [give storage0] ++ ts'

      f x | x < 0 = -x
      f x = x
      envs = emptyEnv { powerMap = M.insert storage0 [3.0] (M.map (map f) sigs) }


      gd = map (eqToInTerm envs) (order ts)

      res :: Envs [Val]
      res = interpretFromScratch gd


  putStrLn (showInTerms gd)
  drawTopology sqTopo res

